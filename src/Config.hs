module Config where

import DataTypes

-- Seems to affect tempo, why?
ticksPerMeasure :: Tick
-- ticksPerMeasure = 2048
-- ticksPerMeasure = 1024
ticksPerMeasure = 512
-- ticksPerMeasure = 256

tickPeriod :: Integer -> Int
tickPeriod bpm =
  round $ fromIntegral (4 * 60) / fromIntegral (bpm * ticksPerMeasure) * 10 ^ 6
