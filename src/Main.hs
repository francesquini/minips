{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Main where

import Architecture
import Emulator

import System.Environment
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word


readWordFun :: Endianness -> Get Word32
readWordFun Big   = getWord32be
readWordFun Little = getWord32le

listOfWord32 :: Get Word32 -> Get [Word32]
listOfWord32 gwf = do
  empty <- isEmpty
  if empty
    then return []
    else (:) <$> gwf <*> listOfWord32 gwf

getWords :: Endianness -> ByteString -> [Word32]
getWords e = runGet (listOfWord32 (readWordFun e))

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  txt0 <- B.readFile $ fileName <> ".text"
  dt0  <- B.readFile $ fileName <> ".data"
  let txt = getWords Little txt0
      dt  = getWords Little dt0
  -- mapM_ print (map (`showHex` "")  wrd)
  let instr = map decodeInstr txt
  mapM_ print instr
  simulate txt dt
