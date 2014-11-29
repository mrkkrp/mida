-- -*- Mode: HASKELL; -*-

-- Translator converts series of numbers into MIDI file.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.

module Translator
    ( genMidi
    , topDefs )
where

import Data.List
import Control.Monad.State.Strict
import Codec.Midi
import System.Random.Mersenne.Pure64
import Environment
import Eval

-- data types --

type Batch = ([Int], [Int], [Int])

-- constants --

mvIndex = 7
durName = "dur"
velName = "vel"
pchName = "pch"
topDefs = [x ++ show n | x <- [durName,velName,pchName], n <- [0..mvIndex]]

-- translation --

request :: Monad m => Int -> StateT Env m Batch
request n =
    do dur <- evalDef $ durName ++ i
       vel <- evalDef $ velName ++ i
       pch <- evalDef $ pchName ++ i
       return (dur, vel, pch)
    where i = show n

fullyDefined :: Batch -> Bool
fullyDefined (dur, vel, pch) = f dur && f vel && f pch
    where f = not . null

slice :: Int -> Batch -> Batch
slice t (dur, vel, pch) = (take n dur, take n vel, take n pch)
    where n = case find ((>= t) . sum) (inits dur) of
                (Just x) -> length x
                Nothing  -> length dur

toTrack :: Batch -> Track Int
toTrack (dur, vel, pch) = (concat $ zipWith3 f dur vel pch) ++ [(0, TrackEnd)]
    where f d v p = [ (0, NoteOn 0 p v)
                    , (d, NoteOn 0 p 0) ]

genMidi :: Monad m => Int -> Int -> Int -> StateT Env m Midi
genMidi s q beats =
    do setRandGen $ pureMT (fromIntegral s)
       voices <- mapM request [0..mvIndex] >>= return . filter fullyDefined
       let xs = map (toTrack . slice (beats * q)) voices
       return Midi { fileType = SingleTrack
                   , timeDiv  = TicksPerBeat q
                   , tracks   = xs }
