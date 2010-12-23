{-# LANGUAGE ExistentialQuantification #-}
-- Copyright © 2010 Bart Massey
-- Explore the wonders of XHB via
-- a Haskell version of xlogo
import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.Word
import Graphics.XHB
import System.Exit
import System.IO

import RenderLogo
import XString

logoInternAtom :: Connection -> String -> IO ATOM
logoInternAtom c s = do
  let name = xString s
  atomReceipt <-
    internAtom c $ MkInternAtom {
      only_if_exists_InternAtom = True, 
      name_len_InternAtom = length_XString name, 
      name_InternAtom = chars_XString name }
  Right atom <- getReply atomReceipt
  return atom


data EventContext = EventContext {
    connection_EventContext :: Connection, 
    window_EventContext :: WINDOW, 
    gc_EventContext :: GCONTEXT,
    wmProtocols_EventContext :: ATOM,
    closeMessage_EventContext :: ATOM }

atomsToPropertyList :: [ATOM] -> [Word8]
atomsToPropertyList as =
  concatMap atomToProperty as
  where
    atomToProperty :: ATOM -> [Word8]
    atomToProperty a =
      let aid = (fromXid $ toXid a) :: Word32 in
      map (\i -> fromIntegral $ (aid `shiftR` (8 * i)) .&. 0xff) [0..3]

main :: IO ()
main = do
  Just c <- connect
  _ <- handleErrors c
  pixels <- logoPixels c
  w <- newResource c
  let rw = getRoot c
  let eventMask = [EventMaskExposure]
  let vp = toValueParam [(CWEventMask, toMask eventMask),
                         (CWBackPixel, bgPixel pixels)]
  createWindow c $ MkCreateWindow {
    depth_CreateWindow = 0, 
    wid_CreateWindow = w, 
    parent_CreateWindow = rw,
    x_CreateWindow = 0,
    y_CreateWindow = 0,
    width_CreateWindow = 100, 
    height_CreateWindow = 100, 
    border_width_CreateWindow = 0,
    class_CreateWindow = WindowClassInputOutput,
    visual_CreateWindow = 0,
    value_CreateWindow = vp }
  closeMessage <- logoInternAtom c "WM_DELETE_WINDOW"
  wm <- logoInternAtom c "WM_PROTOCOLS"
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
  handleEvents $ EventContext {
    connection_EventContext = c, 
    window_EventContext = w, 
    gc_EventContext = gc,
    wmProtocols_EventContext = wm,
    closeMessage_EventContext = closeMessage }

sync :: Connection -> IO ()
sync c = do
  _ <- getInputFocus c
  return ()

handleErrors :: Connection -> IO ThreadId
handleErrors c = forkIO $ forever $ do
  e <- waitForError c
  putStrLn $ showError e

showError :: SomeError -> String
showError e = show e

-- Event-handling code originally from Antoine Latter
-- http://community.haskell.org/~aslatter/code/xhb/Demo.hs  
-- Now munged beyond recognition

handleEvents :: EventContext -> IO ()
handleEvents ctx = forever $ do
  e <- waitForEvent (connection_EventContext ctx)
  handleEvent ctx e

data EventHandler =  forall a . Event a => EventHandler (a -> IO ())

handleEvent :: EventContext -> SomeEvent -> IO ()
handleEvent ctx ev = 
  tryHandleEvent ev hs
  where 
    hs = [EventHandler $ exposeHandler ctx,
          EventHandler $ closeHandler ctx]

tryHandleEvent :: SomeEvent -> [EventHandler] -> IO ()
tryHandleEvent _ [] = return ()
tryHandleEvent ev (EventHandler fn : hs) = do
  case fromEvent ev of
    Just ev' -> fn ev'
    _ -> tryHandleEvent ev hs

exposeHandler :: EventContext -> ExposeEvent -> IO ()
exposeHandler ctx e = do
  print e
  renderLogoCore 
    (connection_EventContext ctx) 
    (window_EventContext ctx) 
    (gc_EventContext ctx) 
    100 100
  sync (connection_EventContext ctx)

-- http://linuxsoftware.co.nz/blog/2008/08/12/
--   handling-window-close-in-an-x11-app
closeHandler :: EventContext -> ClientMessageEvent -> IO ()
closeHandler ctx e = do
   hPutStr stderr "Client Message: "
   let messageType = type_ClientMessageEvent e
   if messageType == wmProtocols_EventContext ctx
     then do hPutStr stderr "checking detail..."
             let cm = closeMessage_EventContext ctx
             let ClientData32 clientData = data_ClientMessageEvent e
             if fromXid (toXid cm) == head clientData
               then do hPutStrLn stderr "and exiting"
                       exitWith ExitSuccess
               else hPutStrLn stderr "wrong message, ignored"
     else hPutStrLn stderr "wrong type, ignored"
