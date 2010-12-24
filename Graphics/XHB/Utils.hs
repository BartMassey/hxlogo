-- Copyright © 2010 Bart Massey

module Graphics.XHB.Utils (toDrawable, defaultScreen, internifyAtom,
                           sync)
where

import Graphics.XHB

import Graphics.XHB.XString

toDrawable :: XidLike a => a -> DRAWABLE
toDrawable = fromXid .toXid

defaultScreen :: Connection -> SCREEN 
defaultScreen = head . roots_Setup . connectionSetup

internifyAtom :: Connection -> Bool -> String -> IO ATOM
internifyAtom c onlyIfExists s = do
  let name = xString s
  atomReceipt <-
    internAtom c $ MkInternAtom {
      only_if_exists_InternAtom = onlyIfExists, 
      name_len_InternAtom = length_XString name, 
      name_InternAtom = chars_XString name }
  Right atom <- getReply atomReceipt
  return atom

sync :: Connection -> IO ()
sync c = do
  _ <- getInputFocus c
  return ()
