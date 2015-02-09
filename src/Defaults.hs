-- -*- Mode: Haskell; -*-
--
-- This module contains default values of some parameters.
--
-- Copyright (c) 2014, 2015 Mark Karpov
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

module Defaults
    ( dfltDefs
    , dfltRandGen
    , dfltPrompt
    , dfltPrvLen
    , dfltBlock
    , dfltFileName
    , dfltPrvCmd
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
dfltPrvCmd   = ""
dfltSeed     = 0 :: Int
dfltQuarter  = 24 :: Int
dfltBeats    = 16 :: Int
