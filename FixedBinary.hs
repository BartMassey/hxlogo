{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
-- Copyright © 2010 Bart Massey
-- Binary fixed point representations
module FixedBinary (FixedBinary(..), B16_16)
where
  
import Data.Word
import Data.Fixed

class FixedBinary a where
  intBits :: p a -> Word
  fracBits :: p a -> Word
  -- XXX This will silently truncate and/or overflow
  fromFixedBinary :: (Integral b) => Fixed a -> b
  fromFixedBinary a = truncate (a * fromIntegral (2 ^ fracBits a))

instance FixedBinary b => HasResolution b where
  resolution a = 2 ^ fracBits a

data Binary16_16 = Binary16_16
type B16_16 = Fixed Binary16_16

instance FixedBinary Binary16_16 where
  intBits _ = 16
  fracBits _ = 16
