{-# LANGUAGE OverloadedStrings #-}

module Utils (
    module Utils
  , N.showHex
  )
where

import Data.Bits
import Data.Text (pack)
import Data.Int
import Data.Word
import Numeric as N
import Rainbow
import Data.Function ((&))
import System.Exit

import qualified Debug.Trace as T

trace :: String -> a -> a
trace = T.trace
-- trace _ a = a

class (Integral a, Show a) => Integral32 a
instance Integral32 Word32
instance Integral32 Int32

signExtend :: Integral32 a =>  Int16 -> a
signExtend = fromIntegral

zeroExtend :: Integral32 a => Int16 -> a
zeroExtend i = fromIntegral (fromIntegral i :: Word16)

breakWord :: Integral a => Word32 -> [a]
breakWord w =
  map maskShift [24, 16, 8, 0]
  where
    maskShift n = fromIntegral $ ((0xFF `shiftL` n) .&. w) `shiftR` n

outputLog :: Radiant -> String -> IO ()
outputLog c s = putChunkLn $ chunk (pack s) & fore c

rlog, glog, blog :: String -> IO ()
rlog = outputLog red
glog = outputLog green
blog = outputLog blue

notImplemented :: String -> IO ()
notImplemented err = do
  rlog $ "Not implemented: " <> err <> ". Terminating execution."
  exitFailure
