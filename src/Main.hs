module Main where

import Architecture (Endianness (Big, Little))
import InstrDecoder (disassemble)
import Emulator
import Utils

import System.Environment
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word
import Data.Time.Clock
import Text.Printf

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

usage :: IO ()
usage = print "Use: minips run arquivo / minips decode arquivo"

main :: IO ()
main = do
  [opt, fileName] <- getArgs
  let end = Little
  txt0 <- B.readFile $ fileName <> ".text"
  dt0  <- B.readFile $ fileName <> ".data"
  let txt = getWords end txt0
      dt  = getWords end dt0
  if opt == "decode" then do
    putStrLn $ disassemble txt
  else if opt == "run" then do
    t0 <- getCurrentTime
    (r, i, j) <- simulate end txt dt
    let totIns = r + i + j
    t1 <- getCurrentTime
    glog "--------------------------"
    glog $ printf "Instruction count: %d (R: %d I: %d J: %d)" totIns r i j
    glog $ "IPS: " ++ show (fromIntegral totIns / diffUTCTime t1 t0)
  else
    usage
