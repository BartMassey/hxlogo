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
    closeMessage_EventContext :: ATOM }

atomsToPropertyList :: [ATOM] -> [Word8]
atomsToPropertyList as =
  concatMap atomToProperty as
  where
    atomToProperty :: ATOM -> [Word8]
    atomToProperty a =
      let aid = (fromXid $ toXid a) :: Word32 in
      map (\i -> fromIntegral $ (aid `shiftR` (8 * i)) .&. 0xff) [3,2..0]

main :: IO ()
main = do
  Just c <- connect
  _ <- handleErrors c
  pixels <- logoPixels c
  w <- newResource c
  let rw = getRoot c
  let vp = toValueParam [(CWEventMask, toMask [EventMaskExposure]),
                         (CWBackPixel, bgPixel pixels)]
  createWindow c (MkCreateWindow
                  0 w rw
                  0 0 100 100 0
                  WindowClassInputOutput 0
                  vp)
  closeMessage <- logoInternAtom c "WM_DELETE_WINDOW"
  wm <- logoInternAtom c "WM_PROTOCOLS"
  let props = [wm]
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

closeHandler :: EventContext -> ClientMessageEvent -> IO ()
closeHandler ctx e = do
   let messageType = type_ClientMessageEvent e
   if messageType == closeMessage_EventContext ctx
     then do hPutStrLn stderr "Exiting"
             exitWith ExitSuccess
     else return ()
