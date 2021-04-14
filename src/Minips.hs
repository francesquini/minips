module Minips where

import Prelude hiding (read)

import Architecture
import qualified InstrDecoder as D
import MemoryHierarchy
import Utils

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Word

import System.IO

data  ICount = ICount {
    rTypeICount  :: !Int
  , iTypeICount  :: !Int
  , jTypeICount  :: !Int
  , frTypeICount :: !Int
  , fiTypeICount :: !Int
  }

data Minips = Minips {
    memory          :: MemoryHierarchy
  , registers       :: IntMap Word32
  , fpRegisters     :: IntMap Word32
  , iCount          :: !ICount
  , cycles          :: !Int
  , delaySlotAddr   :: Maybe Word32
  , traceFileHandle :: Maybe Handle
  }

addTicks :: Int -> Minips -> Minips
addTicks ticks m@Minips{cycles = c} = m{cycles = c + ticks}

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

incRICount, incIICount, incJICount, incFRCount, incFICount :: Minips -> Minips
incRICount m@Minips{iCount=c} = m{iCount = c{rTypeICount=rTypeICount c + 1}}
incIICount m@Minips{iCount=c} = m{iCount = c{iTypeICount=iTypeICount c + 1}}
incJICount m@Minips{iCount=c} = m{iCount = c{jTypeICount=jTypeICount c + 1}}
incFRCount m@Minips{iCount=c} = m{iCount = c{frTypeICount=frTypeICount c + 1}}
incFICount m@Minips{iCount=c} = m{iCount = c{fiTypeICount=fiTypeICount c + 1}}

getMemStats :: Minips -> [(String, AccessStats)]
getMemStats st = fst . fst $ runMemoryHierarchyST getMHStats (memory st)
