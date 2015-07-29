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

module Mida.Midi
    ( genMidi
    , topDefs )
where

import Control.Monad.State.Strict
import Data.Foldable (foldl')
import Data.List (zipWith7)
import Data.Maybe (listToMaybe)
import Prelude hiding (mod)

import qualified Codec.Midi as M

import Mida.Language (MidaEnv, setRandGen, evalDef)

data Batch = Batch
    { btDur  :: [Int]
    , btVel  :: [Int]
    , btPch  :: [Int]
    , _btMod :: Maybe [Int]
    , _btBth :: Maybe [Int]
    , _btAft :: Maybe [Int]
    , _btBnd :: Maybe [Int] }

infixl 4 <!>

(<!>) :: ([Int] -> [Int]) -> Batch -> Batch
f <!> (Batch d v p m t a b) =
    Batch (f d) (f v) (f p) (f <$> m) (f <$> t) (f <$> a) (f <$> b)

data ModParams = ModParams
    { mpValue    :: Int
    , mpFigure   :: Int
    , mpDuration :: Int
    , mpChannel  :: Int
    , mpProducer :: Int -> Int -> M.Message
    , mpUpBounds :: (Int, Int)
    , mpDnBounds :: (Int, Int) }

modP :: ModParams
modP = ModParams
       { mpValue    = 0
       , mpFigure   = 0
       , mpDuration = 0
       , mpChannel  = 0
       , mpProducer = flip M.ControlChange 1
       , mpUpBounds = (0x0000, 0x007f)
       , mpDnBounds = (0x007f, 0x0000) }

bthP :: ModParams
bthP = modP { mpProducer = flip M.ControlChange 2 }

aftP :: ModParams
aftP = modP { mpProducer = M.ChannelPressure }

bndP :: ModParams
bndP = modP
       { mpProducer = M.PitchWheel
       , mpUpBounds = (0x2000, 0x3fff)
       , mpDnBounds = (0x2000, 0x0000) }

type Track = M.Track Int

genMidi :: Monad m => Int -> Int -> Int -> MidaEnv m M.Midi
genMidi s q b = do
  setRandGen s
  voices <- filter defined <$> mapM request [0..mvIndex]
  return M.Midi { M.fileType = M.MultiTrack
                , M.timeDiv  = M.TicksPerBeat q
                , M.tracks   = zipWith (toTrack . slice (b * q)) voices [0..] }

request :: Monad m => Int -> MidaEnv m Batch
request n = do
  dur <- eval' defDur
  vel <- eval' defVel
  pch <- eval' defPch
  mod <- eval' defMod
  bth <- eval' defBth
  aft <- eval' defAft
  bnd <- eval' defBnd
  return $ Batch dur vel pch (f mod) (f bth) (f aft) (f bnd)
    where eval' name = evalDef $ name ++ show n
          f     x    = if null x then Nothing else Just x

defined :: Batch -> Bool
defined Batch { btDur = d, btVel = v, btPch = p } = all (not . null) [d,v,p]

slice :: Int -> Batch -> Batch
slice t batch@Batch { btDur = dur } = take (f 0 0 dur) <!> batch
    where f !i _  []     = i
          f !i !a (x:xs) = if x + a >= t then succ i else f (succ i) (x + a) xs

toTrack :: Batch -> Int -> Track
toTrack (Batch d v p m t a b) i =
    concat (zipWith7 f d v p (r m) (r t) (r a) (r b)) ++ [(0, M.TrackEnd)]
    where r = maybe (repeat Nothing) (Just <$>)
          f d' v' p' m' t' a' b' =
              mixEvents
              [ figure m' d' i modP
              , figure t' d' i bthP
              , figure a' d' i aftP
              , figure b' d' i bndP
              , [(0, M.NoteOn i p' v'), (d', M.NoteOn i p' 0)] ]

mixEvents :: [Track] -> Track
mixEvents = foldl' mixPair mempty

mixPair :: Track -> Track -> Track
mixPair [] xs = xs
mixPair xs [] = xs
mixPair (x:xs) (y:ys) = r : mixPair xs' ys'
    where (r, xs', ys')
              | fst x <= fst y = (x, xs, f y (fst x) : ys)
              | otherwise      = (y, f x (fst y) : xs, ys)
          f (i, msg) c = (i - c, msg)

figure :: Maybe Int -> Int -> Int -> ModParams -> Track
figure Nothing _ _ _ = []
figure (Just raw) d ch p =
    fig p { mpValue    = v
          , mpFigure   = f
          , mpDuration = d
          , mpChannel  = ch }
    where (f, v) = quotRem raw 128

fig :: ModParams -> Track
fig ModParams { mpDuration = 0 } = []
fig (ModParams v f d ch p ub db) =
    maybe [] (zip (0 : repeat 1) . fmap (p ch)) (gen <*> return v)
    where gen = listToMaybe $ drop f -- generators:
                [ figStc ub          -- static
                , figRtn ub d        -- up & down
                , figRtn db d        -- down & up
                , figLin ub d        -- up
                , figLin db d ]      -- down

figStc :: (Int, Int) -> Int -> [Int]
figStc be x = [draw be x 1]

figRtn :: (Int, Int) -> Int -> Int -> [Int]
figRtn be q x = f <$> [0..l] ++ reverse [0..(q - l - 1)]
    where f c = draw be (x * c) l
          l   = q `div` 2

figLin :: (Int, Int) -> Int -> Int -> [Int]
figLin be q x = f <$> [0..q]
    where f c = draw be (x * c) q

draw :: (Int, Int) -> Int -> Int -> Int
draw (b, e) n d = b + (n * (e - b)) `gdiv` (127 * d)
    where x `gdiv` y = round (fromIntegral x / fromIntegral y :: Double)

topDefs :: [String]
topDefs = [x ++ show n |
           x <- [defDur,defVel,defPch,defMod,defBth,defAft,defBnd],
           n <- [0..mvIndex]]

mvIndex :: Int
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
