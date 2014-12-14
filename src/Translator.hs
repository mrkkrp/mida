-- -*- Mode: HASKELL; -*-
--
-- Translator converts series of numbers into data structure that represent
-- MIDI file.
--
-- Copyright (c) 2014 Mark Karpov
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

module Translator
    ( genMidi
    , topDefs )
where

import Data.List
import Control.Monad.State.Strict
import Codec.Midi
import System.Random.Mersenne.Pure64
import Control.Applicative ((<$>))
import Environment
import Eval

-- data types --

data Batch = Batch [Int] [Int] [Int] [Int] [Int] [Int] [Int]

-- constants --

mvIndex = 15 :: Int
defDur  = "dur"
defVel  = "vel"
defPch  = "pch"
defMod  = "mod"
defBth  = "bth"
defAft  = "aft"
defBnd  = "bnd"
topDefs = [x ++ show n | x <- [ defDur
                              , defVel
                              , defPch
                              , defMod
                              , defBth
                              , defAft
                              , defBnd ],
           n <- [0..mvIndex]]

-- translation --

request :: Monad m => Int -> StateT Env m Batch
request n =
    do dur <- evalDef $ defDur ++ i
       vel <- evalDef $ defVel ++ i
       pch <- evalDef $ defPch ++ i
       mod <- evalDef $ defMod ++ i
       bth <- evalDef $ defBth ++ i
       aft <- evalDef $ defAft ++ i
       bnd <- evalDef $ defBnd ++ i
       return $ Batch dur vel pch (f mod) (f bth) (f aft) (f bnd)
    where i = show n
          f x = if null x then repeat (-1) else x

fullyDefined :: Batch -> Bool
fullyDefined (Batch dur vel pch _ _ _ _) = f dur && f vel && f pch
    where f = not . null

slice :: Int -> Batch -> Batch
slice t (Batch dur vel pch mod bth aft bnd) =
    Batch (take n dur) (take n vel) (take n pch)
          (take n mod) (take n bth) (take n aft) (take n bnd)
    where n = case find ((>= t) . sum) (inits dur) of
                Just x  -> length x
                Nothing -> length dur

gdiv :: Int -> Int -> Int
gdiv x y = round $ fromIntegral x / fromIntegral y

figStatic :: Int -> (Int, Int) -> Int
figStatic x (n, d) = n + (x * (d - n)) `gdiv` 127

figLin :: Int -> (Int, Int) -> Int -> [Int]
figLin x (n, d) q = f <$> [0..q]
    where f c = n + (c * (d - n) * x) `gdiv` (127 * q)

figRtn :: Int -> (Int, Int) -> Int -> [Int]
figRtn x (n, d) q = f <$> [0..l] ++ reverse [0..(q - l - 1)]
    where f c = n + (c * (d - n) * x) `gdiv` (127 * l)
          l = q `div` 2

figure :: Int -> (Int, Int, Int, Int) -> Int -> (Int -> Message)
       -> [(Int, Message)]
figure (-1) _ _ _ = []
figure _ _ 0 _ = []
figure x (n0, d0, n1, d1) q f
    | x < 128   = [(0, f $ figStatic x (n0, d0))]
    | x < 256   = zipWith (,) r (map f $ figRtn (x - 128) (n0, d0) q)
    | x < 384   = zipWith (,) r (map f $ figRtn (x - 256) (n1, d1) q)
    | x < 512   = zipWith (,) r (map f $ figLin (x - 384) (n0, d0) q)
    | otherwise = zipWith (,) r (map f $ figLin (x - 512) (n1, d1) q)
    where r = 0 : repeat 1

mixPair :: [(Int, Message)] -> [(Int, Message)] -> [(Int, Message)]
mixPair [] xs = xs
mixPair xs [] = xs
mixPair (x:xs) (y:ys) = r : mixPair xs' ys'
    where (r, xs', ys') = if fst x <= fst y
                          then (x, xs, f y (fst x) : ys)
                          else (y, f x (fst y) : xs, ys)
          f (i, msg) c = (i - c, msg)

mixEvents :: [[(Int, Message)]] -> [(Int, Message)]
mixEvents xs = foldl1 mixPair xs

toTrack :: Batch -> Int -> Track Int
toTrack (Batch dur vel pch mod bth aft bnd) i =
    (concat $ zipWith7 f dur vel pch mod bth aft bnd) ++ [(0, TrackEnd)]
    where f d v p m t a b = mixEvents
                          [figure m (0, 127, 127, 0) d (ControlChange i 1),
                           figure t (0, 127, 127, 0) d (ControlChange i 2),
                           figure a (0, 127, 127, 0) d (ChannelPressure i),
                           figure b (8192, 16383, 8192, 0) d (PitchWheel i),
                           [(0, NoteOn i p v), (d, NoteOn i p 0)]]

genMidi :: Monad m => Int -> Int -> Int -> StateT Env m Midi
genMidi s q beats =
    do setRandGen $ pureMT (fromIntegral s)
       voices <- mapM request [0..mvIndex] >>= return . filter fullyDefined
       let xs = zipWith (toTrack . slice (beats * q)) voices [0..]
       return Midi { fileType = MultiTrack
                   , timeDiv  = TicksPerBeat q
                   , tracks   = xs }
