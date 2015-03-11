-- -*- Mode: Haskell; -*-
--
-- Main module of MIDA describes logic of the program on top level.
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

module Main (main) where

import Control.Monad
import Options.Applicative
import System.Directory (getHomeDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath
import qualified Data.Map as M

import System.Random.Mersenne.Pure64

import Config
import Environment
import Interaction

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

data Opts = Opts Bool Int Int Int String String

----------------------------------------------------------------------------
--                               Constants                                --
----------------------------------------------------------------------------

dfltPrevLen :: Int
dfltPrevLen = 16

dfltSrcFile :: String
dfltSrcFile = "foo.da"

dfltProg :: Int
dfltProg = 0

dfltTempo :: Int
dfltTempo = 120

dfltPrompt :: String
dfltPrompt  = "? "

dfltVerbose :: Bool
dfltVerbose = False

dfltPrvCmd :: String
dfltPrvCmd  = "timidity"

dfltProgOp :: String
dfltProgOp  = "--force-program"

dfltTempoOp :: String
dfltTempoOp = "--adjust-tempo"

notice :: String
notice =
    "MIDA Copyright (c) 2014, 2015 Mark Karpov\n\n\
    \This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
    \and you are welcome to redistribute it under certain conditions; see\n\
    \GNU General Public License for details.\n"

----------------------------------------------------------------------------
--                         Top Level (Invocation)                         --
----------------------------------------------------------------------------

main :: IO ()
main = putStrLn notice >> execParser opts >>= f
    where f (Opts _ _ _ _ _ "") =
              runMida interaction
          f (Opts True  _ _ _ _ name) =
              runMida $ cmdLoad name >> interaction
          f (Opts False s q b out name) =
              runMida $ cmdLoad name >> cmdMake s q b out

runMida :: MidaEnv IO () -> IO ()
runMida e = do
  params <- loadConfig
  wdir   <- getCurrentDirectory
  void $ runMidaEnv e
       MidaState { stDefs    = M.empty
                 , stRandGen = pureMT 0
                 , stPrevLen = lookupCfg params "prvlen" dfltPrevLen
                 , stSrcFile = lookupCfg params "src"    wdir </> dfltSrcFile
                 , stProg    = lookupCfg params "prog"   dfltProg
                 , stTempo   = lookupCfg params "tempo"  dfltTempo }
       MidaConfig { cfgPrompt  = lookupCfg params "prompt"  dfltPrompt
                  , cfgVerbose = lookupCfg params "verbose" dfltVerbose
                  , cfgPrvCmd  = lookupCfg params "prvcmd"  dfltPrvCmd
                  , cfgProgOp  = lookupCfg params "progop"  dfltProgOp
                  , cfgTempoOp = lookupCfg params "tempop"  dfltTempoOp }

loadConfig :: IO Params
loadConfig = do
  home <- getHomeDirectory
  let file = home </> ".mida"
  exist <- doesFileExist file
  if exist
  then do params <- parseConfig file <$> (readFile file)
          case params of
            Right x -> return x
            Left  _ -> return M.empty
  else return M.empty

----------------------------------------------------------------------------
--                        Command Line Processing                         --
----------------------------------------------------------------------------

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
              <> help    ("Set seed for MIDI generation, default is "
                          ++ show dfltSeed))
             <*> option  auto
               ( long    "quarter"
              <> short   'q'
              <> metavar "TICKS"
              <> value   dfltQuarter
              <> help    ("Set ticks per quarter note, default is "
                          ++ show dfltQuarter))
             <*> option  auto
               ( long    "beats"
              <> short   'b'
              <> metavar "BEATS"
              <> value   dfltBeats
              <> help    ("Set total time in quarter notes, default is "
                          ++ show dfltBeats))
             <*> strOption
               ( long    "output"
              <> short   'o'
              <> metavar "OUT"
              <> value   ""
              <> help    "Set name of output file" )
             <*> argument str
               ( metavar "FILE"
              <> value   "" )
