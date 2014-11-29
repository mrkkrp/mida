-- -*- Mode: HASKELL; -*-

-- This is MIDAT utility to transform MIDI files.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Main(main) where

-- import section --

import Config
import System.Random.Mersenne.Pure64
import Options.Applicative
import System.IO
import Text.Printf (printf)
import Codec.Midi (importFile, exportFile)
import Data.List
import System.Directory (doesFileExist)

-- default values --

version = "0.3.0"

-- command line processing --

data Opts = Opts
    { getOut    :: String
    , getMidi   :: String
    , getConfig :: String }

opts :: ParserInfo Opts
opts = info (helper <*> bar)
       ( fullDesc
      <> progDesc "transforms given MIDI file according to given configuration"
      <> header   "midat - transformer of MIDI files" )
    where bar = Opts
                <$> strOption
                  ( long    "output"
                 <> short   'o'
                 <> metavar "OUT"
                 <> value   ""
                 <> help    "Specify output file" )
                <*> argument str
                  ( metavar  "MIDI"
                 <> value "" )
                <*> argument str
                  ( metavar  "CONFIG"
                 <> value "" )

-- general logic --

-- top level logic --

printHeader :: IO ()
printHeader = printf "-> Loading MIDAT v%s;\n" version

main :: IO ()
main = execParser opts >>= f
    where f (Opts o m c)
              | null m || null c = printf "=> please provide more arguments; \
                                          \for more info, try midat --help.\n"
              | otherwise        = printHeader 
