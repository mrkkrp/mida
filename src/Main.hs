-- -*- Mode: HASKELL; -*-
--
-- Main module of MIDA describes logic of the program on top level.
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

module Main (main) where

import Control.Monad.State.Strict
import Options.Applicative
import Environment
import Defaults
import Interaction

-- command line processing --

data Opts = Opts Bool Int Int Int String String

opts :: ParserInfo Opts
opts =  info (helper <*> bar)
      ( fullDesc
     <> progDesc "starts MIDA interpreter or translates source into MIDI file"
     <> header "mida - interpreter for MIDA language" )
    where bar =  Opts
             <$> switch
               ( long    "interactive"
              <> short   'i'
              <> help    "Start MIDA in interactive mode" )
             <*> option  auto
               ( long    "seed"
              <> short   's'
              <> metavar "SEED"
              <> value   dfltSeed
              <> help    "Set seed for MIDI generation, default is 0" )
             <*> option  auto
               ( long    "quarter"
              <> short   'q'
              <> metavar "TICKS"
              <> value   dfltQuarter
              <> help    "Set ticks per quarter note, default is 24" )
             <*> option  auto
               ( long    "beats"
              <> short   'b'
              <> metavar "BEATS"
              <> value   dfltBeats
              <> help    "Set total time in quarter notes, default is 16" )
             <*> strOption
               ( long    "output"
              <> short   'o'
              <> metavar "OUT"
              <> value   ""
              <> help    "Set name of output file" )
             <*> argument str
               ( metavar "FILE"
              <> value   "" )

-- top level logic --

sm :: StateT Env IO () -> IO ()
sm x = void $ runStateT x Env { eDefs      = dfltDefs
                              , eRandGen   = dfltRandGen
                              , ePrompt    = dfltPrompt
                              , ePrvLength = dfltPrvLen
                              , eBlockSize = dfltBlock
                              , eFileName  = dfltFileName
                              , ePrvCmd    = dfltPrvCmd }

main :: IO ()
main = execParser opts >>= f
    where f (Opts _    _ _ _ _ "") =
              sm interLoop
          f (Opts True  _ _ _ _ n) =
              sm $ loadSrc n >> interLoop
          f (Opts False s q b o n) =
              sm $ loadSrc n >> saveMidi s q b o
