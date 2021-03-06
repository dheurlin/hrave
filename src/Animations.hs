module Animations
  ( Animation(..)
  , unAnimation
  -- , DeltaTime
  , cycleAnimation
  , cycleAnimationPause
  , pauseFrame
  , pauseAnim
  , TimeStamp
  , AbsFrame
  , AbsAnimation
  , stepAnim
  , makeFrameEvent
  , toAbsAnimation
  )
where

import DataTypes

import Data.Maybe

import           Reactive.Banana
import           Reactive.Banana.Frameworks

type Duration = Integer
type Frame a  = (Duration, a)

-- | An animation with relative timestaps.
-- Must be converted to 'AbsAnimation' to be played
newtype Animation a  =  Animation [Frame a]
  deriving Show

unAnimation :: Animation a -> [AbsFrame a]
unAnimation (Animation xs) = xs

instance Semigroup (Animation a) where
  (Animation as) <> (Animation bs) = Animation $ as <> bs

instance Monoid (Animation a) where
  mempty = Animation []

pauseAnim :: Empty a => Duration -> Animation a
pauseAnim d = Animation [pauseFrame d]

pauseFrame :: Empty a => Duration -> Frame a
pauseFrame d = (d, emptyElem)

cycleAnimationPause :: Empty a => Duration -> Animation a -> Animation a
cycleAnimationPause pt anim =
  anim <> pauseAnim pt <> cycleAnimationPause pt anim

cycleAnimation :: Animation a -> Animation a
cycleAnimation (Animation as) = Animation $ cycle as

printAnim :: Show a => Int -> Animation a -> IO ()
printAnim n (Animation as) = mapM_ print $ take n as

sumDurations :: Animation a -> Integer
sumDurations (Animation as) = sum . map fst $ as

type TimeStamp         = Integer
type AbsFrame a        = (TimeStamp, a)

-- | An animation with absolute timestamps which can be played back
newtype AbsAnimation a = AbsAnimation [AbsFrame a]
  deriving (Show)

unAbsAnimation :: AbsAnimation a -> [AbsFrame a]
unAbsAnimation (AbsAnimation xs) = xs

toAbsAnimation :: Animation a -> AbsAnimation a
toAbsAnimation (Animation xs) = AbsAnimation $ zip absoluteTimes frames
 where
  (times, frames) = unzip xs
  absoluteTimes   = scanl (+) 0 times

-- | Advances the animation by one step if the correct point in time has been
-- reached, and returns (currentFrame, rest). If the correct point in time
-- has not been reached, returns (Nothing, rest)
stepAnim :: TimeStamp -> AbsAnimation a -> (Maybe a, AbsAnimation a)
stepAnim now (AbsAnimation []) = (Nothing, AbsAnimation [])
stepAnim now (AbsAnimation anim@((time, a) : xs))
  | now == time = (Just a, AbsAnimation xs)
  | otherwise   = (Nothing, AbsAnimation anim)

-- | Given a frame counting event, returns an event which emits frames as the
-- counter reaches the point at which that frame should be displayed
makeFrameEvent :: Event TimeStamp -> AbsAnimation a -> MomentIO (Event a)
makeFrameEvent eCounter anim = do
  eAnimation <- accumE (Nothing, anim) $ (. snd) . stepAnim <$> eCounter
  pure $ fromJust . fst <$> filterE (isJust . fst) eAnimation
