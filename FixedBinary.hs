{-# LANGUAGE FlexibleInstances, UndecidableInstances, EmptyDataDecls #-}
-- Copyright © 2010 Bart Massey
-- Binary fixed point representations
module FixedBinary (HasFixedBits(..), fromFixed, fromRealFrac, 
                    B16_16, B24_8)
where
  
import Data.Fixed
import Data.Ratio
import Data.Word

class HasFixedBits a where
  intBits :: p a -> Word
  fracBits :: p a -> Word

instance HasFixedBits b => HasResolution b where
  resolution a = 2 ^ fracBits a

-- Convert a RealFrac to a Fixed in the obvious way.
fromRealFrac :: (RealFrac a, HasResolution b) => a -> Fixed b
fromRealFrac a = 
  typeify undefined
  where
    typeify :: HasResolution b => Fixed b -> Fixed b
    typeify b = 
      fromRational (truncate (a * fromIntegral r) % r)
      where
        r = resolution b

-- Convert a Fixed to an integer by removing the implicit decimal point.
-- XXX This will silently truncate and/or overflow
fromFixed :: (HasResolution a, Integral b) => Fixed a -> b
fromFixed a = truncate (a * fromIntegral (resolution a))

data Binary16_16
type B16_16 = Fixed Binary16_16

instance HasFixedBits Binary16_16 where
  intBits _ = 16
  fracBits _ = 16

data Binary24_8
type B24_8 = Fixed Binary24_8

instance HasFixedBits Binary24_8 where
  intBits _ = 24
  fracBits _ = 8
