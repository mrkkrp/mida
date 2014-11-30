-- -*- Mode: HASKELL; -*-

-- This module contains default values of some parameters.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Defaults
    ( dfltDefs
    , dfltRandGen
    , dfltPrompt
    , dfltPrvLen
    , dfltBlock
    , dfltFileName
    , dfltSeed
    , dfltQuarter
    , dfltBeats )
where

import qualified Data.Map as M
import System.Random.Mersenne.Pure64

-- constants --

dfltDefs     = M.empty
dfltRandGen  = pureMT 0
dfltPrompt   = "mida> "
dfltPrvLen   = 16 :: Int
dfltBlock    = 4096 :: Int
dfltFileName = "bar.da"
dfltSeed     = 0 :: Int
dfltQuarter  = 24 :: Int
dfltBeats    = 16 :: Int
