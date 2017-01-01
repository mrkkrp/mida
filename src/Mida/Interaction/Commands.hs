--
-- This module describes all supported MIDA commands. It also provides all
-- the functionality to load source files and generate / save MIDI files.
--
-- Copyright © 2014–2017 Mark Karpov
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

{-# LANGUAGE RecordWildCards #-}

module Mida.Interaction.Commands
  ( processCmd
  , completionFunc
  , cmdLoad
  , cmdMake
  , cmdPrefix )
where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Class
import Data.Char (isSpace)
import Data.Foldable (find)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text.Lazy (Text)
import Formatting
import Mida.Interaction.Base
import Mida.Language
import Mida.Midi
import Mida.Representation
import Numeric.Natural
import Path
import Path.IO
import System.Exit (exitSuccess, ExitCode)
import System.Process
  ( shell
  , createProcess
  , waitForProcess
  , delegate_ctlc )
import qualified Codec.Midi               as Midi
import qualified Data.Text.Lazy           as T
import qualified Data.Text.Lazy.IO        as T
import qualified System.Console.Haskeline as L
import qualified System.FilePath          as FP

-- | Description of REPL command.

data Cmd = Cmd
  { cmdName :: String  -- ^ Name of command
  , cmdFunc :: String -> Mida () -- ^ Action it performs
  , cmdDesc :: Text    -- ^ Description of command
  , cmdComp :: CompletionScheme -- ^ How to complete arguments of the command
  }

-- | How to complete arguments of command.

data CompletionScheme
  = None               -- ^ Don't complete at all
  | Files              -- ^ Complete as file names
  | Names              -- ^ Complete as definition names
    deriving (Eq, Show)

-- | All defined REPL commands.

commands :: [Cmd]
commands =
  --    name      action     description                            completion
  [ Cmd "cd"      cmdCd      "Change working directory"             Files
  , Cmd "clear"   cmdClear   "Restore default state of environment" None
  , Cmd "def"     cmdDef     "Print definition of given symbol"     Names
  , Cmd "help"    cmdHelp    "Show this help text"                  None
  , Cmd "load"    cmdLoad'   "Load definitions from given file"     Files
  , Cmd "make"    cmdMake'   "Generate and save MIDI file"          Files
  , Cmd "prog"    cmdProg    "Set program for preview"              None
  , Cmd "prv"     cmdPrv     "Play the score with external program" None
  , Cmd "prvlen"  cmdLength  "Set length of displayed results"      None
  , Cmd "purge"   cmdPurge   "Remove redundant definitions"         None
  , Cmd "pwd"     cmdPwd     "Print working directory"              None
  , Cmd "quit"    cmdQuit    "Quit the interactive environment"     None
  , Cmd "save"    cmdSave    "Save current environment in file"     Files
  , Cmd "tempo"   cmdTempo   "Set tempo for preview"                None
  , Cmd "udef"    cmdUdef    "Remove definition of given symbol"    Names ]

-- | Process command (with its arguments).

processCmd :: Text -> Mida ()
processCmd txt =
  case find g commands of
    Just Cmd { cmdFunc = f } -> do
      result <- try . f . T.unpack . T.strip $ args
                :: Mida (Either SomeException ())
      case result of
        Left  e ->
          case fromException e :: Maybe ExitCode of
            Just  _ -> throwM e
            Nothing -> spitExc e
        Right _ -> return ()
    Nothing -> liftIO $
      fprint ("Unknown command, try " % string % "help\n") cmdPrefix
  where g Cmd { cmdName = c } = c == dropCmdPrefix (T.unpack cmd)
        (cmd, args)           = T.break isSpace (T.strip txt)

-- | Completion function to work with Haskeline.

completionFunc :: (HasEnv m, MonadIO m) => L.CompletionFunc m
completionFunc = L.completeWordWithPrev Nothing " " getCompletions

-- | Generate completions.

getCompletions
  :: (HasEnv m, MonadIO m)
  => String            -- ^ Contents of line before cursor, reversed
  -> String            -- ^ Contents of line after cursor
  -> m [L.Completion]  -- ^ List of completions
getCompletions prev word = do
  names <- getRefs
  files <- L.listFiles word
  let cmds    = (cmdPrefix ++) . cmdName <$> commands
      f       = fmap L.simpleCompletion . filter (word `isPrefixOf`)
      g None  = []
      g Files = files
      g Names = f names
  return $
    case words . reverse $ prev of
      []    -> f $ cmds ++ names
      (c:_) ->
        case c `elemIndex` cmds of
          Just i  -> g . cmdComp $ commands !! i
          Nothing -> f names

-- | Change working directory.

cmdCd :: (MonadIO m, MonadCatch m) => String -> m ()
cmdCd next' = do
  mnext <- forgivingAbsence (resolveDir' next')
  case mnext of
    Nothing -> liftIO $ fprint ("Cannot cd to \"" % string % "\"\n") next'
    Just next -> do
      setCurrentDir next
      liftIO $ fprint ("Changed to \"" % string % "\"\n") (fromAbsDir next)

-- | Restore default state of environment.

cmdClear :: (HasEnv m, MonadIO m) => String -> m ()
cmdClear _ = do
  clearDefs
  liftIO (T.putStrLn "Environment cleared")

-- | Print definition of given symbol.

cmdDef :: (HasEnv m, MonadIO m) => String -> m ()
cmdDef arg = mapM_ f (words arg)
  where f name = getSrc name >>= liftIO . T.putStr

-- | Show help with list of all available commands.

cmdHelp :: MonadIO m => String -> m ()
cmdHelp _ = liftIO $ do
  T.putStrLn "Available commands:"
  mapM_ f commands
  where f Cmd {..} = fprint fmt cmdPrefix cmdName cmdDesc
        fmt = "  " % string % (right 24 ' ' %. string) % text % "\n"

-- | Load definitions from given file. Note that this version of the command
-- is used in REPL, not 'cmdLoad'.

cmdLoad' :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => String -> m ()
cmdLoad' = cmdLoad . words

-- | Alternative interface to loading functionality. This one is used in
-- main module.

cmdLoad :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => [FilePath] -> m ()
cmdLoad = mapM_ loadOne

-- | Load single source file.

loadOne :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => FilePath -> m ()
loadOne given = do
  file <- output given ""
  let fpath = fromAbsFile file
  b    <- doesFileExist file
  if b
    then do
      contents <- liftIO (T.readFile fpath)
      case parseMida fpath contents of
        Right x -> do
          mapM_ f x
          setFileName file
          liftIO $ fprint
            ("\"" % string % "\" loaded successfully\n")
            fpath
        Left  x -> liftIO $ fprint (string % "\n") x
    else liftIO $ fprint ("Could not find \"" % string % "\"\n") fpath
      where f (Definition n t) = processDef n t
            f (Exposition   _) = return ()

-- | Version of 'cmdMake' used by REPL.

cmdMake' :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => String -> m ()
cmdMake' arg =
  let (s:q:b:f:_) = words arg ++ repeat ""
  in cmdMake (parseNum s defaultSeed)
             (parseNum q defaultQuarter)
             (parseNum b defaultBeats)
             f

-- | Generate and save MIDI file.

cmdMake :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => Natural           -- ^ Seed for random number generator
  -> Natural           -- ^ Q value: number of ticks per quarter note
  -> Natural           -- ^ Desired duration in number of quarter notes
  -> FilePath          -- ^ Where to save MIDI file
  -> m ()
cmdMake s q b f = do
  file <- output f "mid"
  let fpath = fromAbsFile file
  midi <- genMidi s q b
  liftIO $ Midi.exportFile fpath midi
  liftIO $ fprint ("MIDI file saved as \"" % string % "\"\n") fpath

-- | Set program for preview.

cmdProg :: MonadState MidaSt m => String -> m ()
cmdProg arg = do
  prog <- gets stProg
  modify $ \st -> st { stProg = parseNum (trim arg) prog }

-- | Preview with help of external program.

cmdPrv :: ( HasEnv m
          , MonadIO m
          , MonadReader MidaCfg m
          , MonadState MidaSt m
          , MonadMask m )
  => String -> m ()
cmdPrv arg = do
  prvcmd  <- asks cfgPrvCmd
  progOp  <- asks cfgProgOp
  prog    <- show <$> gets stProg
  tempoOp <- asks cfgTempoOp
  tempo   <- show <$> gets stTempo
  withSystemTempDir "mida" $ \tdir -> do
    f     <- filename <$> output "" "mid"
    let (s:q:b:_) = words arg ++ repeat ""
        file      = fromAbsFile (tdir </> f)
        cmd       = unwords [prvcmd, progOp, prog, tempoOp, tempo, file]
    cmdMake (parseNum s defaultSeed)
            (parseNum q defaultQuarter)
            (parseNum b defaultBeats)
            file
    (_, _, _, ph) <- liftIO $ createProcess (shell cmd) { delegate_ctlc = True }
    liftIO . void . waitForProcess $ ph

-- | Set length of displayed results.

cmdLength :: MonadState MidaSt m => String -> m ()
cmdLength arg = do
  len <- gets stPrevLen
  modify $ \st -> st { stPrevLen = parseNum (trim arg) len }

-- | Remove redundant definitions.

cmdPurge :: (HasEnv m, MonadIO m) => String -> m ()
cmdPurge _ = do
  purgeEnv topDefs
  liftIO $ T.putStrLn "Environment purged"

-- | Print working directory.

cmdPwd :: MonadIO m => String -> m ()
cmdPwd _ = liftIO (getCurrentDir >>= putStrLn . fromAbsDir)

-- | Quit the interactive environment.

cmdQuit :: MonadIO m => String -> m ()
cmdQuit _ = liftIO exitSuccess

-- | Save current environment in file.

cmdSave :: (HasEnv m, MonadIO m, MonadState MidaSt m, MonadThrow m)
  => String -> m ()
cmdSave given = do
  file   <- output given ""
  let fpath = fromAbsFile file
  src    <- fullSrc
  liftIO $ T.writeFile fpath src
  setFileName file
  liftIO $ fprint ("Environment saved as \"" % string % "\"\n") fpath

-- | Set tempo for preview.

cmdTempo :: MonadState MidaSt m => String -> m ()
cmdTempo arg = do
  tempo <- gets stTempo
  modify $ \st -> st { stTempo = parseNum (trim arg) tempo }

-- | Undefine definitions.

cmdUdef :: (HasEnv m, MonadIO m) => String -> m ()
cmdUdef arg = mapM_ f (words arg)
  where f name = do
          remDef name
          liftIO $ fprint ("Definition for ‘" % string % "’ removed\n") name

-- | Parse a number defaulting to given value.

parseNum :: Read a
  => String            -- ^ String to parse
  -> a                 -- ^ Default value
  -> a                 -- ^ Result
parseNum s x = fromMaybe x $ fst <$> listToMaybe (reads s)

-- | Generate file name from given base name and extension.

output :: (MonadIO m, MonadThrow m, MonadState MidaSt m)
  => FilePath            -- ^ Given file name
  -> String              -- ^ Extension
  -> m (Path Abs File)   -- ^ Absolute path to output file
output given' ext =
  if null given'
    then do
      actual <- fromAbsFile <$> gets stSrcFile
      parseAbsFile (if null ext then actual else FP.replaceExtension actual ext)
    else resolveFile' given'

-- | Change current file name.

setFileName :: MonadState MidaSt m => Path Abs File -> m ()
setFileName fpath = modify $ \st -> st { stSrcFile = fpath }

-- | Drop command prefix if it's present.

dropCmdPrefix :: String -> String
dropCmdPrefix arg
  | cmdPrefix `isPrefixOf` arg = drop (length cmdPrefix) arg
  | otherwise = arg

-- | All REPL commands are prefixed with this.

cmdPrefix :: String
cmdPrefix = ":"

-- | Print out an exception.

spitExc :: MonadIO m => SomeException -> m ()
spitExc = liftIO . fprint ("× " % string % "\n") . show

-- | Stupid trimming for strings.

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f
