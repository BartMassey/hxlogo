-- Copyright © 2010 Bart Massey
-- This program is licensed under the "3-clause ('new') BSD License".
-- See the file COPYING in this distribution for license information.

-- | These are simply some random utlities for dealing with X.
-- There is nothiing too clever here; just some stuff that kept
-- coming up.

module Graphics.XHB.Utils (toDrawable, defaultScreen, internifyAtom,
                           sync)
where

import Graphics.XHB

import Graphics.XHB.XString

-- | In X, a "drawable" is a window or pixmap. This type class
-- maintains that notion in a type-safe way.
class XidLike a => DrawableLike a where
  toDrawable :: a -> DRAWABLE
  toDrawable = fromXid .toXid

instance DrawableLike WINDOW
instance DrawableLike PIXMAP

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
