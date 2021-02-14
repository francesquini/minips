module Utils where

import Data.Int
import Data.Word

(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

class Integral a => Integral32 a
instance Integral32 Word32
instance Integral32 Int32

signExtend :: Integral32 a =>  Int16 -> a
signExtend = fromIntegral

zeroExtend :: Integral32 a => Int16 -> a
zeroExtend i = fromIntegral (fromIntegral i :: Word16)

