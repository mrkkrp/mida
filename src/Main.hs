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

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Formatting
import Mida.Configuration
import Mida.Interaction
import Numeric.Natural
import Options.Applicative
import Path.IO
import Paths_mida (version)
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

runMida' :: Mida a -> IO ()
runMida' e = do
  mconfig <- forgivingAbsence (resolveFile' ".mida.yaml")
  c <- case mconfig of
    Nothing -> return def
    Just file -> do
      econfig <- parseMidaConfig file
      case econfig of
        Left msg -> liftIO (putStrLn msg) >> return def
        Right val -> return val
  srcFile <- makeAbsolute (configSrcFile c)
  void $ runMida e
    MidaSt  { stPrevLen  = configPrevLen c
            , stSrcFile  = srcFile
            , stProg     = configProg    c
            , stTempo    = configTempo   c }
    MidaCfg { cfgPrompt  = configPrompt  c
            , cfgVerbose = configVerbose c
            , cfgPrvCmd  = configPrvCmd  c
            , cfgProgOp  = configProgOp  c
            , cfgTempoOp = configTempoOp c }

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
