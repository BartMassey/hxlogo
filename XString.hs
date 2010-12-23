-- Copyright © 2010 Bart Massey

module XString
where

import Foreign.C.Types
import Foreign.C.String
import Data.Word

data XString = XString {
  length_XString :: Word16,
  chars_XString :: [CChar]
}

xString :: String -> XString
xString s = XString {
  length_XString = fromIntegral $ length s,
  chars_XString = map castCharToCChar s }
