-- -*- Mode: Haskell; -*-
--
-- This module describes how to create MIDI file from MIDA environment.
--
-- Copyright © 2014, 2015 Mark Karpov
--
-- MIDA is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- MIDA is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Mida.Midi
  ( genMidi
  , topDefs )
where

import Control.Monad.State.Strict
import Data.Foldable (foldl')
import Data.List (zipWith7)
import Mida.Language (HasEnv, setRandGen, evalDef)
import Numeric.Natural
import Prelude hiding (mod)
import qualified Codec.Midi as Midi

-- | 'Batch' is collection of all parameters needed to generate complete
-- MIDI track.

data Batch = Batch
  { btDur  :: [Int] -- ^ Duration stream
  , btVel  :: [Int] -- ^ Velocity stream
  , btPch  :: [Int] -- ^ Pitch stream
  , _btMod :: Maybe [Int] -- ^ Optional modulation stream
  , _btBth :: Maybe [Int] -- ^ Optional breath stream
  , _btAft :: Maybe [Int] -- ^ Optional aftertouch stream
  , _btBnd :: Maybe [Int] -- ^ Optional pitch bend stream
  }

-- | Apply transformation on all streams in 'Batch'.

infixl 4 <!>

(<!>) :: ([Int] -> [Int]) -> Batch -> Batch
f <!> (Batch d v p m t a b) =
  Batch (f d) (f v) (f p) (f <$> m) (f <$> t) (f <$> a) (f <$> b)

-- | Modulation parameters. This defines how sound will be modulated (if at
-- all).

data ModParams = ModParams
  { mpValue    :: !Int -- ^ Value or amplitude of modulation
  , mpFigure   :: !(Maybe Figure) -- ^ Figure or shape of modulation
  , mpDuration :: !Int -- ^ Duration in ticks
  , mpChannel  :: !Int -- ^ Channel index
  , mpProducer :: !(Int -> Int -> Midi.Message) -- ^ Producer function
  , mpUpBounds :: !(Int, Int) -- ^ Boundaries for ascending modulation
  , mpDnBounds :: !(Int, Int) -- ^ Boundaries for descending modulation
  }

-- | Modulation figures.

data Figure
  = FigStatic          -- ^ Static: no modulation
  | FigUpDown          -- ^ First ascending and then descending
  | FigDownUp          -- ^ First descending and then ascending
  | FigUp              -- ^ Ascending for the whole duration
  | FigDown            -- ^ Descending for the whole duration
    deriving (Eq, Show, Bounded, Enum)

-- | Default modulation parameters.

modP :: ModParams
modP = ModParams
  { mpValue    = 0
  , mpFigure   = Just FigStatic
  , mpDuration = 0
  , mpChannel  = 0
  , mpProducer = flip Midi.ControlChange 1
  , mpUpBounds = (0x0000, 0x007f)
  , mpDnBounds = (0x007f, 0x0000) }

-- | Default modulation parameters for breath modulation.

bthP :: ModParams
bthP = modP
  { mpProducer = flip Midi.ControlChange 2 }

-- | Default modulation parameters for aftertouch modulation.

aftP :: ModParams
aftP = modP
  { mpProducer = Midi.ChannelPressure }

-- | Default modulation parameters for pitch bend modulation.

bndP :: ModParams
bndP = modP
  { mpProducer = Midi.PitchWheel
  , mpUpBounds = (0x2000, 0x3fff)
  , mpDnBounds = (0x2000, 0x0000) }

-- | A synonym for what we use as track definition.

type Track = Midi.Track Int

-- | Generate MIDI file from MIDA environment.

genMidi :: HasEnv m
  => Natural           -- ^ Seed for random generator
  -> Natural           -- ^ Q value: number of ticks per quarter note
  -> Natural           -- ^ Duration in number of quarter notes
  -> m Midi.Midi       -- ^ MIDI file
genMidi seed q b = do
  setRandGen seed
  voices <- filter defined <$> mapM request [0..mvIndex]
  return Midi.Midi
    { Midi.fileType = Midi.MultiTrack
    , Midi.timeDiv  = Midi.TicksPerBeat (fromIntegral q)
    , Midi.tracks   = zipWith (toTrack . slice (b * q)) voices [0..] }

-- | Generate MIDI track given its index.

request :: HasEnv m
  => Natural           -- ^ Track index
  -> m Batch           -- ^ 'Batch' for this track
request n = do
  dur <- eval' defDur
  vel <- eval' defVel
  pch <- eval' defPch
  mod <- eval' defMod
  bth <- eval' defBth
  aft <- eval' defAft
  bnd <- eval' defBnd
  return $ Batch dur vel pch (f mod) (f bth) (f aft) (f bnd)
  where eval' name = fmap fromIntegral <$> evalDef (name ++ show n)
        f     x    = if null x then Nothing else Just x

-- | Check if all necessary components in a 'Batch' are defined.

defined :: Batch -> Bool
defined Batch {..} = all (not . null) [btDur, btVel, btPch]

-- | Take some part (determined by number of ticks) of every
-- infinite stream in 'Batch' making it finite.

slice
  :: Natural           -- ^ Requested duration in ticks
  -> Batch             -- ^ 'Batch' of infinite streams
  -> Batch             -- ^ 'Batch' of finite streams
slice t' batch@Batch {..} = take (f 0 0 btDur) <!> batch
  where t = fromIntegral t'
        f !i _  []     = i
        f !i !a (x:xs) = if x + a >= t then succ i else f (succ i) (x + a) xs

-- | Generate MIDI 'Track' from 'Batch'.

toTrack
  :: Batch             -- ^ Batch
  -> Int               -- ^ Channel number
  -> Track             -- ^ Result track
toTrack (Batch d v p m t a b) i =
  concat (zipWith7 f d v p (r m) (r t) (r a) (r b)) ++ [(0, Midi.TrackEnd)]
  where r  = maybe (repeat Nothing) (fmap Just)
        f d' v' p' m' t' a' b' =
          mixEvents
          [ figure m' d' i modP
          , figure t' d' i bthP
          , figure a' d' i aftP
          , figure b' d' i bndP
          , [ (0, Midi.NoteOn i p' v')
            , (d', Midi.NoteOn i p' 0)] ]

-- | Merge several tracks together. There is 'Codec.Midi.merge' thing, but
-- I'm not sure it does it right, moreover it's not documented.

mixEvents :: [Track] -> Track
mixEvents = foldl' mixPair mempty

-- | Merge just two tracks.

mixPair :: Track -> Track -> Track
mixPair [] xs = xs
mixPair xs [] = xs
mixPair (x:xs) (y:ys) = r : mixPair xs' ys'
  where (r, xs', ys')
          | fst x <= fst y = (x, xs, f y (fst x) : ys)
          | otherwise      = (y, f x (fst y) : xs, ys)
        f (i, msg) c = (i - c, msg)

-- | Generate track fragment representing modulation of one parameter.

figure
  :: Maybe Int         -- ^ Raw modulation value, if present
  -> Int               -- ^ Duration in ticks of entire fragment
  -> Int               -- ^ Channel index
  -> ModParams         -- ^ Default modulation parameters
  -> Track             -- ^ Result fragment
figure Nothing _ _ _ = []
figure (Just raw) d ch p =
  fig p { mpValue    = v
        , mpFigure   = f
        , mpDuration = d
        , mpChannel  = ch }
  where (fi, v) = quotRem raw 128
        f | fi <= fromEnum (maxBound :: Figure) = Just (toEnum fi)
          | otherwise = Nothing

-- | Transform modulation parameters into fragment of 'Track'.

fig :: ModParams -> Track
fig ModParams { mpDuration = 0 } = []
fig (ModParams v f d ch p ub db) =
  let getGen x = case x of
        FigStatic -> figStc ub
        FigUpDown -> figRtn ub d
        FigDownUp -> figRtn db d
        FigUp     -> figLin ub d
        FigDown   -> figLin db d
  in maybe [] (zip (0 : repeat 1) . fmap (p ch)) (getGen <$> f <*> pure v)

-- | Generate static stream of values.

figStc
  :: (Int, Int)        -- ^ Beginning and end values
  -> Int               -- ^ Amplitude (from 0 to 127)
  -> [Int]             -- ^ Resulting stream
figStc be x = [draw be x 1]

-- | Generate “up-down” or “down-up” stream of values (i.e. “returning
-- stream”).

figRtn
  :: (Int, Int)        -- ^ Beginning and end values
  -> Int               -- ^ Total number of elements in stream
  -> Int               -- ^ Amplitude (from 0 to 127)
  -> [Int]             -- ^ Resulting stream
figRtn be q x = f <$> [0..l] ++ reverse [0..(q - l - 1)]
  where f c = draw be (x * c) l
        l   = q `div` 2

-- | Generate linear stream of values.

figLin
  :: (Int, Int)        -- ^ Beginning and end values
  -> Int               -- ^ Total number of elements in stream
  -> Int               -- ^ Amplitude (from 0 to 127)
  -> [Int]             -- ^ Resulting stream
figLin be q x = f <$> [0..q]
  where f c = draw be (x * c) q

-- | Calculate one point from stream of integer values.

draw
  :: (Int, Int)        -- ^ Beginning and end values
  -> Int               -- ^ Numerator
  -> Int               -- ^ Denominator
  -> Int               -- ^ Value of this point
draw (b, e) n d = b + (n * (e - b)) `gdiv` (127 * d)
  where x `gdiv` y = round (fromIntegral x / fromIntegral y :: Double)

-- | Collection of “top-level” definitions.

topDefs :: [String]
topDefs =
  [ x ++ show n | x <- [defDur,defVel,defPch,defMod,defBth,defAft,defBnd]
                , n <- [0..mvIndex] ]

-- | Maximal voice index. @15@ means that we can have 16 voices total.

mvIndex :: Natural
mvIndex = 15

defDur :: String
defDur  = "dur"

defVel :: String
defVel  = "vel"

defPch :: String
defPch  = "pch"

defMod :: String
defMod  = "mod"

defBth :: String
defBth  = "bth"

defAft :: String
defAft  = "aft"

defBnd :: String
defBnd  = "bnd"
