--
-- Main module of MIDA describes logic of the program on top level.
--
-- Copyright © 2014–2016 Mark Karpov
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

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Formatting
import Mida.Configuration
import Mida.Interaction
import Numeric.Natural
import Options.Applicative
import Path
import Path.IO
import Paths_mida (version)
import qualified Data.Map as M
import qualified Data.Text.Lazy.IO as T

-- | MIDA application command line options.

data Opts = Opts
  { opInterac :: Bool  -- ^ Do we run in interactive mode?
  , opSeed    :: Natural -- ^ Seed for random number generator
  , opQuarter :: Natural -- ^ Number of ticks per quarter note
  , opBeats   :: Natural -- ^ Duration as number of quarter notes
  , opOutput  :: FilePath -- ^ Where to save generate MIDI file
  , opLicense :: Bool  -- ^ Whether to show license
  , opVersion :: Bool  -- ^ Whether to show program's version
  , opFiles   :: [FilePath] -- ^ Source files to load
  }

-- | Entry point for the whole thing.

main :: IO ()
main = execParser opts >>= f
  where f Opts { opLicense = True } = T.putStr license
        f Opts { opVersion = True } = fprint ("MIDA " % string % "\n") ver
        f Opts { opFiles   = []   } = g interaction
        f Opts { opInterac = True
               , opFiles   = ns   } = g $ cmdLoad ns >> interaction
        f Opts { opSeed    = s
               , opQuarter = q
               , opBeats   = b
               , opOutput  = out
               , opFiles   = ns   } = g $ cmdLoad ns >> cmdMake s q b out
        g x = T.putStrLn notice >> runMida' x
        ver = showVersion version

-- | Shortish copyright notice.

notice :: Text
notice =
  "MIDA Copyright © 2014–2016 Mark Karpov\n\n\
  \This program comes with ABSOLUTELY NO WARRANTY. This is free software,\n\
  \and you are welcome to redistribute it under certain conditions; see\n\
  \GNU General Public License for details.\n"

-- | Longer copyright notice.

license :: Text
license =
  "MIDA — realization of MIDA, language for generation of MIDI files.\n\
  \Copyright © 2014–2016 Mark Karpov\n\
  \\n\
  \MIDA is free software: you can redistribute it and/or modify it under the\n\
  \terms of the GNU General Public License as published by the Free Software\n\
  \Foundation, either version 3 of the License, or (at your option) any\n\
  \later version.\n\
  \\n\
  \MIDA is distributed in the hope that it will be useful, but WITHOUT ANY\n\
  \WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS\n\
  \FOR A PARTICULAR PURPOSE. See the GNU General Public License for more\n\
  \details.\n\
  \\n\
  \You should have received a copy of the GNU General Public License along\n\
  \with this program. If not, see <http://www.gnu.org/licenses/>.\n"

-- | Read configuration file if present and run MIDA monad.

runMida' :: Mida () -> IO ()
runMida' e = do
  params <- loadConfig
  wdir   <- getCurrentDir
  dfname <- parseRelFile "foo.da"
  let dfltSrcFile = fromAbsFile (wdir </> dfname)
  srcFile <- parseAbsFile (lookupCfg params "src" dfltSrcFile)
  void $ runMida e
    MidaSt { stPrevLen = lookupCfg params "prvlen" 18
           , stSrcFile = srcFile
           , stProg    = lookupCfg params "prog"   0
           , stTempo   = lookupCfg params "tempo"  120 }
    MidaCfg { cfgPrompt  = lookupCfg params "prompt"  "> "
            , cfgVerbose = lookupCfg params "verbose" True
            , cfgPrvCmd  = lookupCfg params "prvcmd"  "timidity"
            , cfgProgOp  = lookupCfg params "progop"  "--force-program"
            , cfgTempoOp = lookupCfg params "tempop"  "--adjust-tempo" }

-- | Read configuration file.

loadConfig :: IO Params
loadConfig = do
  config <- (</> $(mkRelFile ".mida")) <$> getHomeDir
  exists <- doesFileExist config
  if exists
    then do
      let fconfig = fromAbsFile config
      params <- parseConfig fconfig <$> T.readFile fconfig
      case params of
        Right x -> return x
        Left  _ -> return M.empty
    else return M.empty

-- | Some information about the program.

opts :: ParserInfo Opts
opts =  info (helper <*> options)
      ( fullDesc
     <> progDesc "starts MIDA interpreter or translates source into MIDI file"
     <> header "mida — interpreter for MIDA language" )

-- | Description of command line options.

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
  <> value defaultSeed
  <> help ("Set seed for MIDI generation, default is " ++ show defaultSeed) )
  <*> option auto
  ( long "quarter"
  <> short 'q'
  <> metavar "TICKS"
  <> value defaultQuarter
  <> help ("Set ticks per quarter note, default is " ++ show defaultQuarter) )
  <*> option auto
  ( long "beats"
  <> short 'b'
  <> metavar "BEATS"
  <> value defaultBeats
  <> help ("Set total time in quarter notes, default is " ++ show defaultBeats) )
  <*> strOption
  ( long "output"
  <> short 'o'
  <> metavar "OUT"
  <> value ""
  <> help "Set name of output file" )
  <*> switch
  ( long "license"
  <> help "Show license of the program" )
  <*> switch
  ( long "version"
  <> help "Show version of the program" )
  <*> many (strArgument $ metavar "FILES")
