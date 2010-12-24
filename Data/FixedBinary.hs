-- Copyright © 2010 Bart Massey
-- This program is licensed under the "3-clause ('new') BSD License".
-- See the file COPYING in this distribution for license information.

{-# LANGUAGE FlexibleInstances, UndecidableInstances, EmptyDataDecls #-}

-- | In a number of applications, binary fixed point
-- representations are needed-- that are based on integral
-- types with a fixed number of mantissa and exponent bits.
-- This modules is a bit of glue atop 'Data.Fixed' to help
-- supply that need. It's a bit strange, but the short version
-- is that the types 'B16_16' and 'B24_24' provided by this
-- module correspond to 'Fixed' types with the appropriate
-- resolution.
module Data.FixedBinary (HasFixedBits(..), 
                         fromFixed, fromRealFrac, 
                         B16_16, B24_8)
where
  
import Data.Fixed
import Data.Ratio
import Data.Word

-- | Associate a given number of integral and fractional
-- bits with a given type.
class HasFixedBits a where
  intBits :: p a -> Word
  fracBits :: p a -> Word

instance HasFixedBits b => HasResolution b where
  resolution a = 2 ^ fracBits a

-- | Convert a 'RealFrac' to the nearest corresponding 'Fixed'.
fromRealFrac :: (RealFrac a, HasResolution b) => a -> Fixed b
fromRealFrac a = 
  typeify undefined
  where
    typeify :: HasResolution b => Fixed b -> Fixed b
    typeify b = 
      fromRational (truncate (a * fromIntegral r) % r)
      where
        r = resolution b

-- | Cast a 'Fixed' to an 'Integral' by removing the implicit decimal point.
-- This will silently truncate and/or overflow if the 'Integral' is
-- not a good match for the 'Fixed'.
fromFixed :: (HasResolution a, Integral b) => Fixed a -> b
fromFixed a = truncate (a * fromIntegral (resolution a))

data Binary16_16
-- | Type of 16.16 fixed-point numbers.
type B16_16 = Fixed Binary16_16

instance HasFixedBits Binary16_16 where
  intBits _ = 16
  fracBits _ = 16

data Binary24_8
-- | Type of 24.8 fixed-point numbers.
type B24_8 = Fixed Binary24_8

instance HasFixedBits Binary24_8 where
  intBits _ = 24
  fracBits _ = 8
