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

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Options.Applicative
import System.Directory (getHomeDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Mida.Configuration
import Mida.Interaction

data Opts = Opts
    { opInteractive :: Bool
    , _opSeed       :: Int
    , _opQuarter    :: Int
    , _opBeats      :: Int
    , _opOutput     :: String
    , opMidaFiles   :: [String] }

main :: IO ()
main = T.putStrLn notice >> execParser opts >>= f
    where f Opts { opMidaFiles = [] } =
              runMida $ interaction version
          f Opts { opInteractive = True, opMidaFiles = names } =
              runMida $ cmdLoad names >> interaction version
          f (Opts _ s q b out names) =
              runMida $ cmdLoad names >> cmdMake s q b out
          version = "0.4.1"

notice :: T.Text
notice =
    "MIDA Copyright (c) 2014, 2015 Mark Karpov\n\n\
    \This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
    \and you are welcome to redistribute it under certain conditions; see\n\
    \GNU General Public License for details.\n"

runMida :: MidaIO () -> IO ()
runMida e = do
  params <- loadConfig
  wdir   <- getCurrentDirectory
  void $ runMidaInt e
       MidaSt { stPrevLen = lookupCfg params "prvlen" 16
              , stSrcFile = lookupCfg params "src"    wdir </> "foo.da"
              , stProg    = lookupCfg params "prog"   0
              , stTempo   = lookupCfg params "tempo"  120 }
       MidaCfg { cfgPrompt  = lookupCfg params "prompt"  "? "
               , cfgVerbose = lookupCfg params "verbose" False
               , cfgPrvCmd  = lookupCfg params "prvcmd"  "timidity"
               , cfgProgOp  = lookupCfg params "progop"  "--force-program"
               , cfgTempoOp = lookupCfg params "tempop"  "--adjust-tempo" }

loadConfig :: IO Params
loadConfig = do
  home <- getHomeDirectory
  let file = home </> ".mida"
  exist <- doesFileExist file
  if exist
  then do params <- parseConfig file <$> T.readFile file
          case params of
            Right x -> return x
            Left  _ -> return M.empty
  else return M.empty

opts :: ParserInfo Opts
opts =  info (helper <*> options)
      ( fullDesc
     <> progDesc "starts MIDA interpreter or translates source into MIDI file"
     <> header "mida - interpreter for MIDA language" )

options :: Parser Opts
options = Opts
  <$> switch
  ( long "interactive"
  <> short 'i'
  <> help "Start MIDA in interactive mode" )
  <*> option auto
  ( long "seed"
  <> short 's'
  <> metavar "SEED"
  <> value dfltSeed
  <> help ("Set seed for MIDI generation, default is " ++ show dfltSeed) )
  <*> option auto
  ( long "quarter"
  <> short 'q'
  <> metavar "TICKS"
  <> value dfltQuarter
  <> help ("Set ticks per quarter note, default is " ++ show dfltQuarter) )
  <*> option auto
  ( long "beats"
  <> short 'b'
  <> metavar "BEATS"
  <> value dfltBeats
  <> help ("Set total time in quarter notes, default is " ++ show dfltBeats) )
  <*> strOption
  ( long "output"
  <> short 'o'
  <> metavar "OUT"
  <> value ""
  <> help "Set name of output file" )
  <*> many (strArgument $ metavar "FILES")
