module Main where

import Architecture (Endianness (Big, Little))
import Data.Binary.Get
    ( runGet, isEmpty, getWord32le, getWord32be, Get )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock ( diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds )
import Data.Word ( Word32 )
import Emulator ( simulate )
import InstrDecoder (disassemble)
import System.Environment ( getArgs )
import System.Directory
import System.IO
import Text.Printf ( printf )
import Utils ( glog, ifM )
import Constants

readWordFun :: Endianness -> Get Word32
readWordFun Big = getWord32be
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

run :: String -> FilePath -> IO ()
run opt fileName = do
  let end = Little
  txt0 <- B.readFile $ fileName <> ".text"
  dt0 <- B.readFile $ fileName <> ".data"
  let txt = getWords end txt0
      dt = getWords end dt0

  let roFname = fileName <> ".rodata"
  ro <- ifM (doesFileExist roFname)
         (getWords end <$> B.readFile roFname)
         (return [])

  if opt == "decode"
    then do
      putStrLn $ disassemble txt
    else
      if opt == "run"
        then do
          t0 <- getCurrentTime
          (c, (r, i, j, fr)) <- simulate end (txt, dt, ro)
          t1 <- getCurrentTime
          let totIns = r + i + j + fr
              diffTime = fromRational $ toRational $ nominalDiffTimeToSeconds (diffUTCTime t1 t0) :: Double
          glog "--------------------------"
          glog $ printf "Instruction count: %d (R: %d I: %d J: %d FR: %d)" totIns r i j fr
          glog $ printf "Simulation Time: %.2f sec." diffTime
          glog $ printf "Average IPS: %.2f" (fromIntegral totIns / diffTime)

          glog "\n\nSimulated execution times for:"
          glog "--------------------------"

          glog "Monocycle"
          glog $ printf "\tCycles: %d" c
          glog $ printf "\tFrequency: %.4f MHz" monocycleFrequency
          let timeMono = picoToSeconds $ monocycleClockPeriod * fromIntegral c
          glog $ printf "\tEstimated execution time: %.4f sec." timeMono
          glog $ printf "\tIPC: %.2f" (fromIntegral totIns / fromIntegral c :: Double)
          glog $ printf "\tMIPS: %.2f" (fromIntegral totIns / timeMono / mega :: Double)

          glog "Pipelined"
          glog $ printf "\tCycles: %d" (c + 4)
          glog $ printf "\tFrequency: %.4f MHz" multicycleFrequency
          let timeMulti = picoToSeconds $ multicycleClockPeriod * fromIntegral (c + 4)
          glog $ printf "\tEstimated execution time: %.4f sec." timeMulti
          glog $ printf "\tIPC: %.2f" (fromIntegral totIns / fromIntegral (c + 4) :: Double)
          glog $ printf "\tMIPS: %.2f" (fromIntegral totIns / timeMulti / mega :: Double)

          glog $ printf "Speedup Monocycle/Pipeline: %.2fx" (timeMono/timeMulti)

        else usage

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  args <- getArgs
  if length args /= 2
    then usage
    else do
      [opt, fileName] <- getArgs
      run opt fileName
