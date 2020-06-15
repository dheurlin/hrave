module Animations
  ( DeltaTime
  , Frame
  , Animation(..)
  , cycleAnimation
  , TimeStamp
  , AbsFrame
  , AbsAnimation
  , stepAnim
  , makeFrameEvent
  , toAbsAnimation
  , testAnimation
  )
where

import Data.Maybe

import           Reactive.Banana
import           Reactive.Banana.Frameworks

type DeltaTime         = Integer
type Frame a           = (DeltaTime, a)
-- | An animation with relative timestaps.
-- Must be converted to 'AbsAnimation' to be played
newtype Animation a  =  Animation [Frame a]

unAnimation :: Animation a -> [AbsFrame a]
unAnimation (Animation xs) = xs

type TimeStamp         = Integer
type AbsFrame a        = (TimeStamp, a)
-- | An animation with absolute timestamps which can be played back
newtype AbsAnimation a = AbsAnimation [AbsFrame a]
  deriving (Show)

unAbsAnimation :: AbsAnimation a -> [AbsFrame a]
unAbsAnimation (AbsAnimation xs) = xs

cycleAnimation :: Maybe DeltaTime -> Animation a -> Animation a
cycleAnimation pause (Animation xs) = Animation $ zip cTimes cFrames
 where
  (times@(t : ts), frames) = unzip xs
  cFrames                  = cycle frames
  cTimes                   = times ++ cycle ((t + fromMaybe 1 pause) : ts)

-- | Advances the animation by one step if the correct point in time has been
-- reached, and returns (currentFrame, rest). If the correct point in time
-- has not been reached, returns (Nothing, rest)
stepAnim :: TimeStamp -> AbsAnimation a -> (Maybe a, AbsAnimation a)
stepAnim now (AbsAnimation []) = (Nothing, AbsAnimation [])
stepAnim now (AbsAnimation anim@((time, a) : xs))
  | now == time = (Just a, AbsAnimation xs)
  | otherwise   = (Nothing, AbsAnimation anim)

makeFrameEvent :: Event TimeStamp -> AbsAnimation a -> MomentIO (Event a)
makeFrameEvent eCounter anim = do
  eAnimation <- accumE (Nothing, anim) $ (. snd) . stepAnim <$> eCounter
  pure $ fromJust . fst <$> filterE (isJust . fst) eAnimation

toAbsAnimation :: Animation a -> AbsAnimation a
toAbsAnimation (Animation xs) = AbsAnimation $ zip absoluteTimes frames
 where
  (times, frames) = unzip xs
  absoluteTimes   = tail $ scanl (+) 0 times
