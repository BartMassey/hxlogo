{-# LANGUAGE ExistentialQuantification #-}
-- Copyright © 2010 Bart Massey
-- Explore the wonders of XHB via
-- a Haskell version of xlogo
import Control.Concurrent
import Control.Monad
import Graphics.XHB
import System.IO

main = do
  Just c <- connect
  _ <- handleErrors c
  let rw = getRoot c
  w <- newResource c
  let vp = toValueParam [(CWEventMask, toMask [EventMaskExposure])]
  createWindow c (MkCreateWindow
                  0 w rw
                  0 0 100 100 0
                  WindowClassInputOutput 0
                  vp)
  _ <- handleEvents c w
  mapWindow c w
  sync c
  putStr "> "
  hFlush stdout
  s <- getLine
  return ()

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

handleEvents :: Connection -> WINDOW -> IO ThreadId
handleEvents c w = forkIO $ forever $ do
  e <- waitForEvent c
  handleEvent c w e

handleEvent :: Connection -> WINDOW -> SomeEvent -> IO ()
handleEvent c w ev = tryHandleEvent c w ev hs
  where 
    hs = [EventHandler exposeHandler]

data EventHandler =  forall a . Event a 
                  => EventHandler (Connection -> WINDOW -> a -> IO ())

exposeHandler :: Connection -> WINDOW -> ExposeEvent -> IO ()
exposeHandler c w _ =
  clearArea c (MkClearArea False w 0 0 100 100)

tryHandleEvent :: Connection
               -> WINDOW
               -> SomeEvent 
               -> [EventHandler] 
               -> IO ()
tryHandleEvent _ _ _ [] = return ()
tryHandleEvent c w ev (EventHandler fn : hs) = do
  case fromEvent ev of
    Just ev'' -> fn c w ev''
    _ -> tryHandleEvent c w ev hs
