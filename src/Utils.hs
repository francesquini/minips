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
import Text.Printf
import Data.Binary.IEEE754
import Control.Monad
import System.Directory

trace :: String -> a -> a
trace = T.trace
-- trace _ a = a

class (Integral a, Show a) => Integral32 a
instance Integral32 Word32
instance Integral32 Int32

powerOf2 :: Int -> Bool
powerOf2 x = x > 0 && ((x .&. (x - 1)) == 0)

signExtend :: Integral32 a =>  Int16 -> a
signExtend = fromIntegral

zeroExtend :: Integral32 a => Int16 -> a
zeroExtend i = fromIntegral (fromIntegral i :: Word16)

breakWord :: Integral a => Word32 -> [a]
breakWord w =
  map maskShift [0, 8, 16, 24]
  where
    maskShift n = fromIntegral $ ((0xFF `shiftL` n) .&. w) `shiftR` n

words32FromDouble :: Double -> (Word32, Word32)
words32FromDouble db =
  (dbu, dbl)
  where
    db0 = doubleToWord db
    dbu = fromIntegral $ db0 `shiftR` 32
    dbl = fromIntegral $ db0 .&. 0xffffffff

doubleFromWords32 :: Word32 -> Word32 -> Double
doubleFromWords32 up low =
  wordToDouble $ (w64up `shiftL` 32) .|. w64low
  where
    w64up  = fromIntegral up :: Word64
    w64low = fromIntegral low :: Word64

showHex32 :: Word32 -> String
showHex32 = printf "%08x"

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

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, x, _, _) = x

trd4 :: (a, b, c, d) -> c
trd4 (_, _, x, _) = x

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, x) = x

class QuadFunctor f where
  quadmap ::
    (a -> a') ->
    (b -> b') ->
    (c -> c') ->
    (d -> d') ->
    f a b c d -> f a' b' c' d'

  map1   :: (a -> a') -> f a b c d -> f a' b c d
  map1 f1 = quadmap f1 id id id

  map2   :: (b -> b') -> f a b c d -> f a b' c d
  map2 f2 = quadmap id f2 id id

  map3   :: (c -> c') -> f a b c d -> f a b c' d
  map3 f3 = quadmap id id f3 id

  map4   :: (d -> d') -> f a b c d -> f a b c d'
  map4 f4 = quadmap id id id f4

instance QuadFunctor (,,,) where
  quadmap f1 f2 f3 f4 (a, b, c, d) = (f1 a, f2 b, f3 c, f4 d)

(.>>) :: Monad m => (t -> m a) -> (t -> m b) -> t -> m b
f .>> g = \x -> f x >> g x

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b' <- b; if b' then t else f

whenM :: Monad m => m Bool -> m () -> m ()
whenM b act = ifM b act (pure ())

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

infixl 4 >>$
(>>$) :: Monad m => m (a -> m b) -> a -> m b
(>>$) f = join . ap f . pure
{-# INLINE (>>$) #-}

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path =
  whenM (doesFileExist path) (removeFile path)

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = undefined
