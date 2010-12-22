{-# LANGUAGE ExistentialQuantification #-}
-- Copyright © 2010 Bart Massey
-- Explore the wonders of XHB via
-- a Haskell version of xlogo
import Control.Concurrent
import Control.Monad
import Graphics.XHB

import RenderLogo

data EventContext = EventContext {
    connection_EventContext :: Connection, 
    window_EventContext :: WINDOW, 
    gc_EventContext :: GCONTEXT }


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
  mapWindow c w
  sync c
  gc <- logoGC c w pixels
  handleEvents $ EventContext {
    connection_EventContext = c, 
    window_EventContext = w, 
    gc_EventContext = gc }

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
    hs = [EventHandler (exposeHandler ctx)]

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

-- closeHandler :: EventContext -> ClientMessageEvent -> IO ()
-- closeHandler ctx e = do
--   let ClientData32 (message : _) = data_ClientMessageEvent e
  
