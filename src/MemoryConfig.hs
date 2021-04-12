module MemoryConfig
  (
    getConf
  )
where

import MemoryHierarchy
import System.Random

defLineSize :: Int
defLineSize = 32

defRAMLatency :: Latency
defRAMLatency = 100

defRAM :: Bool -> MemoryLevel
defRAM = mkRAM "RAM" defLineSize defRAMLatency

defAttributes :: (Int, Int)
defAttributes = lineAttributes defLineSize

data TraceLevel = None | Basic | Full

traceLevel :: String -> TraceLevel
traceLevel "trace"  = Basic
traceLevel "debug"  = Full
traceLevel _        = None

tracePars :: TraceLevel -> (Bool, Bool)
tracePars None  = (False, False)
tracePars Basic = (True,  False)
tracePars Full  = (True,  True)

getConf :: RandomGen g => Int -> String -> g -> MemoryHierarchy
getConf conf tlStr = (confs !! conf) (traceLevel tlStr)

confs :: RandomGen g => [TraceLevel -> g -> MemoryHierarchy]
confs = [conf1, conf2, conf3, conf4, conf5, conf6]


-- No cache, 32 bytes/line
conf1 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf1 tl _ =
  Unified ram defAttributes (fst tp)
  where
   tp = tracePars tl
   ram = defRAM (snd tp)

-- Unified, single-level, 1024 bytes, direct-mapped, 32 bytes/line, random
conf2 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf2 tl g =
  Unified l1c defAttributes (fst tp)
  where
    ram = defRAM (snd tp)
    l1c = mkCache
      "L1"
      1    -- 1 cycle Latency
      1024
      defLineSize
      1    --assoc
      ram
      (randomReplacementPolicy g)
      (snd tp)
    tp = tracePars tl

-- Split, single-level, 512 bytes each, direct-mapped, 32 bytes/line, random
conf3 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf3 tl g =
  Split l1d l1i defAttributes (fst tp)
  where
    tp = tracePars tl
    ram = defRAM (snd tp)
    (g1, g2) = split g
    l1d = mkCache
      "L1d"
      1 -- 1 cycle Latency
      512 -- size
      defLineSize
      1 -- assoc
      ram
      (randomReplacementPolicy g1)
      (snd tp)
    l1i = mkCache
      "L1i"
      1
      512
      defLineSize
      1
      ram
      (randomReplacementPolicy g2)
      (snd tp)

-- Split, single-level, 512 bytes each, direct-mapped, 32 bytes/line, lru
conf4 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf4 tl _ =
  Split l1d l1i defAttributes (fst tp)
  where
    ram = defRAM (snd tp)
    l1d = mkCache
      "L1d"
      1 -- 1 cycle Latency
      512 -- size
      defLineSize
      1 -- assoc
      ram
      lruReplacementPolicy
      (snd tp)
    l1i = mkCache
      "L1i"
      1
      512
      defLineSize
      1
      ram
      lruReplacementPolicy
      (snd tp)
    tp = tracePars tl


-- Split, single-level, 512 bytes each, 4-way, 32 bytes/line, lru
conf5 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf5 tl _ =
  Split l1d l1i defAttributes (fst tp)
  where
    ram = defRAM (snd tp)
    l1d = mkCache
      "L1d"
      1 -- 1 cycle Latency
      512 -- size
      defLineSize
      4 -- assoc
      ram
      lruReplacementPolicy
      (snd tp)
    l1i = mkCache
      "L1i"
      1
      512
      defLineSize
      4
      ram
      lruReplacementPolicy
      (snd tp)
    tp = tracePars tl

-- Split, double-level, L1 512 bytes each 4-way, L2 unified 4096 8-way, 64 bytes/line, lru
conf6 :: RandomGen g => TraceLevel -> g -> MemoryHierarchy
conf6 tl _ =
  Split l1d l1i (lineAttributes lnSz) (fst tp)
  where
    lnSz = 64
    ram = mkRAM "RAM" lnSz defRAMLatency (snd tp)
    l2  = mkCache
      "L2"
      10
      4096
      lnSz
      8
      ram
      lruReplacementPolicy
      (snd tp)
    l1d = mkCache
      "L1d"
      1 -- 1 cycle Latency
      512 -- size
      lnSz
      4 -- assoc
      l2
      lruReplacementPolicy
      (snd tp)
    l1i = mkCache
      "L1i"
      1
      512
      lnSz
      4
      l2
      lruReplacementPolicy
      (snd tp)
    tp = tracePars tl
