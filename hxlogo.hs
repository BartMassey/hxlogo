-- Copyright © 2010 Bart Massey
-- This program is licensed under the "3-clause ('new') BSD License".
-- See the file COPYING in this distribution for license information.

{-# LANGUAGE ExistentialQuantification #-}

-- Explore the wonders of XHB via
-- a Haskell version of xlogo

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef
import Data.Word
import Graphics.XHB
import Graphics.XHB.Connection.Extension
import Graphics.XHB.Gen.Render as R
import System.Exit
import System.IO

import Graphics.XHB.Utils
import RenderLogo

-- Trapezoids were introduced in Render 0.4.
logoRenderUsable :: Connection -> IO Bool
logoRenderUsable c = do
  let ext = R.extension
  present <- extensionPresent c ext
  case present of
    False -> return False
    True -> do
      versionReceipt <- R.queryVersion c 0 11
      Right (MkQueryVersionReply major minor) <- getReply versionReceipt
      return $ major == 0 && minor >= 4



-- XXX This is almost surely broken on big-endian machines.
-- An xhb fix is needed to make this portable.
atomsToPropertyList :: [ATOM] -> [Word8]
atomsToPropertyList as =
  concatMap atomToProperty as
  where
    atomToProperty :: ATOM -> [Word8]
    atomToProperty a =
      let aid = (fromXid $ toXid a) :: Word32 in
      map (\i -> fromIntegral $ (aid `shiftR` (8 * i)) .&. 0xff) [0..3]

initialWidth :: Word16
initialWidth = 100

initialHeight :: Word16
initialHeight = 100

data EventContext = EventContext {
    connection_EventContext :: Connection, 
    window_EventContext :: WINDOW, 
    gc_EventContext :: GCONTEXT,
    wmProtocols_EventContext :: ATOM,
    closeMessage_EventContext :: ATOM, 
    width_EventContext :: IORef Word16,
    height_EventContext :: IORef Word16,
    renderInfo_EventContext :: Maybe (PICTURE, PICTURE) }

main :: IO ()
main = do
  Just c <- connect
  _ <- handleErrors c
  pixels <- logoPixels c
  w <- newResource c
  let rw = getRoot c
  let eventMask = [EventMaskExposure, EventMaskStructureNotify]
  let vp = toValueParam [(CWEventMask, toMask eventMask),
                         (CWBackPixel, bgPixel pixels)]
  createWindow c $ MkCreateWindow {
    depth_CreateWindow = 0, 
    wid_CreateWindow = w, 
    parent_CreateWindow = rw,
    x_CreateWindow = 0,
    y_CreateWindow = 0,
    width_CreateWindow = initialWidth, 
    height_CreateWindow = initialHeight, 
    border_width_CreateWindow = 0,
    class_CreateWindow = WindowClassInputOutput,
    visual_CreateWindow = 0,
    value_CreateWindow = vp }
  closeMessage <- internifyAtom c True "WM_DELETE_WINDOW"
  wm <- internifyAtom c True "WM_PROTOCOLS"
  let props = [closeMessage]
  changeProperty c $ MkChangeProperty {
    mode_ChangeProperty = PropModeReplace,
    window_ChangeProperty = w,
    property_ChangeProperty = wm,
    type_ChangeProperty = fromXid $ toXid (4 :: Word32), -- XA_ATOM 
    format_ChangeProperty = 32,
    data_len_ChangeProperty = fromIntegral $ length props,
    data_ChangeProperty = atomsToPropertyList props }
  mapWindow c w
  sync c
  gc <- logoGC c w pixels
  widthRef <- newIORef initialWidth
  heightRef <- newIORef initialHeight
  renderable <- logoRenderUsable c
  renderInfo <-
    case renderable of
      True -> do
        grayPicture <- logoGrayPicture c w
        windowPicture <- logoWindowPicture c w
        return $ Just (grayPicture, windowPicture)
      False -> return Nothing
  handleEvents $ EventContext {
    connection_EventContext = c, 
    window_EventContext = w, 
    gc_EventContext = gc,
    wmProtocols_EventContext = wm,
    closeMessage_EventContext = closeMessage,
    width_EventContext = widthRef,
    height_EventContext = heightRef,
    renderInfo_EventContext = renderInfo }

handleErrors :: Connection -> IO ThreadId
handleErrors c = forkIO $ forever $ do
  e <- waitForError c
  hPutStrLn stderr $ showError e

showError :: SomeError -> String
showError e = show e

-- Event-handling code originally from Antoine Latter.
--   http://community.haskell.org/~aslatter/code/xhb/Demo.hs  
-- Now munged quite a bit.

handleEvents :: EventContext -> IO ()
handleEvents ctx = forever $ do
  e <- waitForEvent (connection_EventContext ctx)
  handleEvent ctx e

data EventHandler =  forall a . Event a => 
                     EventHandler (EventContext -> a -> IO ())

handleEvent :: EventContext -> SomeEvent -> IO ()
handleEvent ctx ev = 
  tryHandleEvent ctx ev hs
  where 
    hs = [EventHandler exposeHandler,
          EventHandler closeHandler,
          EventHandler resizeHandler]

tryHandleEvent :: EventContext -> SomeEvent -> [EventHandler] -> IO ()
tryHandleEvent _ _ [] = return ()
tryHandleEvent ctx ev (EventHandler fn : hs) = do
  case fromEvent ev of
    Just ev' -> fn ctx ev'
    _ -> tryHandleEvent ctx ev hs

exposeHandler :: EventContext -> ExposeEvent -> IO ()
exposeHandler ctx e = do
  let c = connection_EventContext ctx
  let w = window_EventContext ctx
  let gc = gc_EventContext ctx
  width <- readIORef $ width_EventContext ctx
  height <- readIORef $ height_EventContext ctx
  case renderInfo_EventContext ctx of
    Nothing -> renderLogoCore c w gc width height
    Just (grayPicture, windowPicture) -> 
      renderLogoRender c grayPicture windowPicture width height
  sync (connection_EventContext ctx)

-- http://linuxsoftware.co.nz/blog/2008/08/12/
--   handling-window-close-in-an-x11-app
closeHandler :: EventContext -> ClientMessageEvent -> IO ()
closeHandler ctx e = do
  let messageType = type_ClientMessageEvent e
  if messageType == wmProtocols_EventContext ctx
    then do let cm = closeMessage_EventContext ctx
            let ClientData32 clientData = data_ClientMessageEvent e
            if fromXid (toXid cm) == head clientData
              then exitWith ExitSuccess
              else return ()
    else return ()

resizeHandler :: EventContext -> ConfigureNotifyEvent -> IO ()
resizeHandler ctx e = do
  let widthRef = width_EventContext ctx
  let heightRef = height_EventContext ctx
  curWidth <- readIORef widthRef
  curHeight <- readIORef heightRef
  let newWidth = width_ConfigureNotifyEvent e
  let newHeight = height_ConfigureNotifyEvent e
  if newWidth /= curWidth || newHeight /= curHeight
    then do writeIORef widthRef newWidth
            writeIORef heightRef newHeight
    else return ()
