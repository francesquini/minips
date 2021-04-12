module Main where

import Architecture (Endianness (Big, Little), Executable)
import Data.Binary.Get
    (runGet, isEmpty, getWord32le, getWord32be, Get)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Data.Word (Word32)
import Emulator (simulate)
import InstrDecoder (disassemble)
import System.Environment (getArgs)
import System.Directory
import Text.Printf (printf)
import Utils (glog, ifM, removeFileIfExists)
import Constants
import Data.Time (UTCTime)
import MinipsST
import Text.Read (readMaybe)
import qualified MemoryConfig as MC
import System.Random

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

readProgram :: Endianness -> FilePath -> IO Executable
readProgram end fileName =  do
  txt0 <- B.readFile $ fileName <> ".text"
  dt0  <- B.readFile $ fileName <> ".data"
  let txt = getWords end txt0
      dt  = getWords end dt0

  let roFname = fileName <> ".rodata"
  ro <- ifM (doesFileExist roFname)
         (getWords end <$> B.readFile roFname)
         (return [])
  return (txt, ro, dt)


usage :: IO ()
usage = putStrLn . unlines $ [
    "Use: minips {run|decode|trace|debug} [conf] arquivo / minips decode arquivo"
  , "", ""
  , "Exemplos:"
  , "---------"
  , "minips decode 01.soma"
  , "minips run 01.soma"
  , "minips run 1 01.soma"
  , "minips run 2 02.soma"
  , "minips trace 3 01.soma"
  , "minips debug 4 01.soma"
  , "", ""
  , "Opções:"
  , "-------"
  , "run - Executa o programa e imprime estatísticas básicas ao final."
  , "decode - Decodifica o programa e imprime o assembly. Conf é ignorada."
  , "trace - O mesmo que run e escreve no arquivo 'minips.trace' os acessos feitos à memória."
  , "debug - O mesmo que trace, contudo com muito mais informações sobre os acessos às caches/memória."
  , "", ""
  , "Configurações de memória possíveis:"
  , "-----------------------------------"
  , ""
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| Conf | Níveis | Tipo      | Tamanho     | Map.   | Tam./Linha | Política  |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 1*   | 0      | -         | -           | -      | -          | -         |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 2    | 1      | Unificada | 1024        | Direto | 32         | Aleatória |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 3    | 1      | Split     | 512/cada    | Direto | 32         | Aleatória |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 4    | 1      | Split     | 512/cada    | Direto | 32         | LRU       |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 5    | 1      | Split     | 512/cada    | 4 vias | 32         | LRU       |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , "| 6    | 2      | Split     | L1 512/cada | 4 vias | 64         | LRU       |"
  , "|      |        | Unificada | L2 4096     | 8 vias | 64         | LRU       |"
  , "|------|--------|-----------|-------------|--------|------------|-----------|"
  , ""
  , "* Padrão se não especificado"
  ]



printReport :: (Int, ICount) -> UTCTime -> UTCTime -> IO ()
printReport (c, (r, i, j, fr)) t0 t1 =
  mapM_ glog report
  where
    totIns = r + i + j + fr
    diffTime = fromRational $ toRational $ nominalDiffTimeToSeconds (diffUTCTime t1 t0) :: Double
    report = concat [header, monocycle, multicycle, footer]
    header = [
        "--------------------------"
      , printf "Instruction count: %d (R: %d I: %d J: %d FR: %d)" totIns r i j fr
      , printf "Simulation Time: %.2f sec." diffTime
      , printf "Average IPS: %.2f" (fromIntegral totIns / diffTime)
      ]
    timeMono = picoToSeconds $ monocycleClockPeriod * fromIntegral c
    monocycle = [
        "\n\nSimulated execution times for:"
      , "--------------------------"
      , "Monocycle"
      , printf "\tCycles: %d" c
      , printf "\tFrequency: %.4f MHz" monocycleFrequency
      , printf "\tEstimated execution time: %.4f sec." timeMono
      , printf "\tIPC: %.2f" (fromIntegral totIns / fromIntegral c :: Double)
      , printf "\tMIPS: %.2f" (fromIntegral totIns / timeMono / (siMultiplier @Mega) :: Double)
      ]
    timeMulti = picoToSeconds $ multicycleClockPeriod * fromIntegral (c + 4)
    multicycle = [
        "Pipelined"
      , printf "\tCycles: %d" (c + 4)
      , printf "\tFrequency: %.4f MHz" multicycleFrequency
      , printf "\tEstimated execution time: %.4f sec." timeMulti
      , printf "\tIPC: %.2f" (fromIntegral totIns / fromIntegral (c + 4) :: Double)
      , printf "\tMIPS: %.2f" (fromIntegral totIns / timeMulti /  (siMultiplier @Mega) :: Double)
      ]
    footer = [printf "Speedup Monocycle/Pipeline: %.2fx" (timeMono/timeMulti)]


run :: String -> Maybe Int -> FilePath -> IO ()
run opt mConf fileName = do
  if conf < 0 || conf > 5 then
    usage
  else do
    exe <- readProgram end fileName
    if opt == "decode"
      then do
        putStrLn $ disassemble exe
      else if opt `elem` ["run", "trace", "debug"] then do
             g <- newStdGen
             removeFileIfExists traceFile
             let memConf = MC.getConf conf opt g
             t0 <- getCurrentTime
             simRes <- simulate exe memConf
             t1 <- getCurrentTime
             printReport simRes t0 t1
          else usage
  where
    end = Little
    conf = maybe 0 (subtract 1) mConf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [opt, fileName]       -> run opt Nothing fileName
    [opt, conf, fileName] -> run opt (readMaybe conf) fileName
    _                     -> usage
