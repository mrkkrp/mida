-- -*- Mode: HASKELL; -*-

-- Translator converts series of numbers into MIDI file.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Translator
    ( saveMidi
    , topDefs )
where

-- Import Section --

import Parser
import Environment
import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Codec.Midi
import Data.List
import Data.Maybe
import System.Random.Mersenne.Pure64

-- Data Structures --

type Batch = ([Int], [Int], [Int])
type Event = (Ticks, Message)

-- Translation --

mvIndex = 7
durName = "dur"
velName = "vel"
pchName = "pch"
topDefs = [x ++ show n | x <- [durName,velName,pchName], n <- [0..mvIndex]]

request :: Int -> MidaM Batch
request n =
    do dur <- evalItem $ durName ++ i
       vel <- evalItem $ velName ++ i
       pch <- evalItem $ pchName ++ i
       return (dur, vel, pch)
    where i = show n

fullyDefined :: Batch -> Bool
fullyDefined (dur, vel, pch) = f dur && f vel && f pch
    where f = not . null

slice :: Int -> Batch -> Batch
slice t (dur, vel, pch) = (take n dur, take n vel, take n pch)
    where n = length . fromJust $ find ((>= t) . sum) (inits dur)

toTrack :: Batch -> Track Int
toTrack (dur, vel, pch) = (concat $ zipWith3 f dur vel pch) ++ [(0, TrackEnd)]
    where f d v p = [ (0, NoteOn 0 p v)
                    , (d, NoteOn 0 p 0) ]

saveMidi :: Int -> Int -> Int -> String -> MidaM ()
saveMidi s q beats fileName =
    do setRandGen $ pureMT (fromIntegral s)
       voices <- filter fullyDefined <$> mapM request [0..mvIndex]
       let xs = map (toTrack . slice (beats * q)) voices
       liftIO $ exportFile fileName Midi { fileType = MultiTrack
                                         , timeDiv  = TicksPerBeat q
                                         , tracks   = xs }
