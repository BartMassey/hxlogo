-- Copyright © 2010 Bart Massey
-- This program is licensed under the "3-clause ('new') BSD License".
-- See the file COPYING in this distribution for license information.

-- | In X, strings are typically length-list, where the list
-- is an array of Word8. Note that character-width issues are
-- ill-specified and poorly-handled. Anyhow...
-- 
-- This module provides a data structure representing an X
-- string, and a conversion routine for making one out of a
-- Haskell 'String'. This is one of two considered approaches;
-- the other, using a type class, was rejected since doing it
-- this way caches the conversion.
module Graphics.XHB.XString (XString(..), xString)
where

import Foreign.C.Types
import Foreign.C.String
import Data.Word

data XString = XString {
  length_XString :: Word16,
  chars_XString :: [CChar]
}

-- | Convert a Haskell 'String' to an 'XString'.
xString :: String -> XString
xString s = XString {
  length_XString = fromIntegral $ length s,
  chars_XString = map castCharToCChar s }
