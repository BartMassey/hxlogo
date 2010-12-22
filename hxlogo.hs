{-# LANGUAGE ExistentialQuantification #-}
-- Copyright © 2010 Bart Massey
-- Explore the wonders of XHB via
-- a Haskell version of xlogo
import Control.Concurrent
import Control.Monad
import Data.Word
import Graphics.XHB
import System.IO

import RenderLogo

main :: IO ()
main = do
  Just c <- connect
  _ <- handleErrors c
  let white = getWhite c
  let vp = toValueParam [(CWEventMask, toMask [EventMaskExposure]),
                         (CWBackPixel, white)]
  let rw = getRoot c
  w <- newResource c
  createWindow c (MkCreateWindow
                  0 w rw
                  0 0 100 100 0
                  WindowClassInputOutput 0
                  vp)
  gc <- logoGC c w
  _ <- handleEvents c w gc
  mapWindow c w
  sync c
  putStr "> "
  hFlush stdout
  _ <- getLine
  return ()

getWhite :: Connection -> Word32
getWhite = 
  white_pixel_SCREEN . defaultScreen
    

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

handleEvents :: Connection -> WINDOW -> GCONTEXT -> IO ThreadId
handleEvents c w gc = forkIO $ forever $ do
  e <- waitForEvent c
  handleEvent c w gc e

handleEvent :: Connection -> WINDOW -> GCONTEXT -> SomeEvent -> IO ()
handleEvent c w gc ev = tryHandleEvent c w ev hs
  where 
    hs = [EventHandler (exposeHandler gc)]

data EventHandler =  forall a . Event a 
                  => EventHandler (Connection -> WINDOW -> a -> IO ())

exposeHandler :: GCONTEXT -> Connection -> WINDOW -> ExposeEvent -> IO ()
exposeHandler gc c w e = do
  print e
  clearArea c (MkClearArea False w 0 0 0 0)
  renderLogoCore c w gc 100 100
  sync c

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
