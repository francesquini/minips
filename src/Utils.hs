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


fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

trd3 :: (a, b, c) -> c
trd3 (_, _, x) = x

class TriFunctor f where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'

  map1   :: (a -> a') -> f a b c -> f a' b c
  map1 f1 = trimap f1 id id

  map2   :: (b -> b') -> f a b c -> f a b' c
  map2 f2 = trimap id f2 id

  map3   :: (c -> c') -> f a b c -> f a b c'
  map3 f3 = trimap id id f3

instance TriFunctor (,,) where
  trimap f1 f2 f3 (a, b, c) = (f1 a, f2 b, f3 c)
