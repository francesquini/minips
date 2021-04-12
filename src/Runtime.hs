module Runtime where

import Prelude hiding (read)

import Architecture
import qualified InstrDecoder as D
import MemoryHierarchy
import Utils

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Word

import System.IO

type ICount = (Int, Int, Int, Int)
rTypeICount, iTypeICount, jTypeICount, frTypeICount :: ICount -> Int
rTypeICount  = fst4
iTypeICount  = snd4
jTypeICount  = trd4
frTypeICount = fth4

data Minips = Minips {
    memory          :: MemoryHierarchy
  , registers       :: IntMap Word32
  , fpRegisters     :: IntMap Word32
  , iCount          :: ICount
  , cycles          :: Int
  , delaySlotAddr   :: Maybe Word32
  , traceFileHandle :: Maybe Handle
  }

tick :: Minips -> Minips
tick m@Minips{cycles = c} = m{cycles = c + 1}

regRead :: RegName -> Minips -> Word32
regRead regName st =
  registers st IM.! fromEnum regName

fpRegRead :: FPRegName -> Minips -> Word32
fpRegRead fpRegName st =
  fpRegisters st IM.! fromEnum fpRegName

memRead :: AccessType -> Address -> Minips -> (((Word32, Latency), Log), Minips)
memRead aType ad st =
  ((ret, log0), st{memory = mh})
  where
    ((ret, mh), log0) = runMemoryHierarchyST (read aType ad) (memory st)

-- Write OPs
regWrite :: Integral32 a => RegName -> a -> Minips -> Minips
regWrite Zero _ m = m
regWrite r v m@Minips {registers=regs} =
  m{registers= regs'}
  where
    regs' = IM.insert (fromEnum r) (fromIntegral v) regs

fpRegWrite :: Integral32 a => FPRegName -> a -> Minips -> Minips
fpRegWrite r v m@Minips {fpRegisters=fpRegs} =
  m{fpRegisters= fpRegs'}
  where
    fpRegs' = IM.insert (fromEnum r) (fromIntegral v) fpRegs

memWrite :: Address -> Word32 -> Minips -> ((Latency, Log), Minips)
memWrite ad val st =
  ((lat, log0), st{memory=mh})
  where
    ((lat, mh), log0) = runMemoryHierarchyST (write ad val) (memory st)

getBranchDelaySlotAddress :: Minips -> Maybe Word32
getBranchDelaySlotAddress = delaySlotAddr

setBranchDelaySlotAddress :: Maybe Word32 -> Minips -> Minips
setBranchDelaySlotAddress mi m = m{delaySlotAddr= mi}

decodeInstruction :: Word32 -> Minips -> (InstrWord, Minips)
decodeInstruction w m = (D.decodeInstruction w, m)

incPC :: Minips -> Minips
incPC st = regWrite Pc (regRead Pc st + 4) st

incRICount, incIICount, incJICount, incFRCount :: Minips -> Minips
incRICount m@Minips{iCount=c} = m{iCount = map1 (+1) c}
incIICount m@Minips{iCount=c} = m{iCount = map2 (+1) c}
incJICount m@Minips{iCount=c} = m{iCount = map3 (+1) c}
incFRCount m@Minips{iCount=c} = m{iCount = map4 (+1) c}
