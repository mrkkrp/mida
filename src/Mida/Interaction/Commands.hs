-- -*- Mode: Haskell; -*-
--
-- This module describes all supported MIDA commands. It also provides all
-- the functionality to load source files and generate / save MIDI files.
--
-- Copyright © 2014, 2015 Mark Karpov
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

module Mida.Interaction.Commands
  ( processCmd
  , completionFunc
  , cmdLoad
  , cmdMake
  , cmdPrefix )
where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.Foldable (find)
import Data.List (elemIndex, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getHomeDirectory
  , getTemporaryDirectory
  , setCurrentDirectory )
import System.Exit (exitSuccess)
import System.FilePath
  ( addTrailingPathSeparator
  , joinPath
  , replaceExtension
  , splitDirectories
  , takeFileName
  , (</>) )
import System.Process
  ( shell
  , createProcess
  , waitForProcess
  , delegate_ctlc )
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Formatting
import qualified Codec.Midi as Midi
import qualified System.Console.Haskeline as L

import Mida.Interaction.Base
import Mida.Language
import Mida.Midi
import Mida.Representation

data Cmd = Cmd
  { cmdName :: String
  , cmdFunc :: String -> MidaIO ()
  , cmdDesc :: T.Text
  , cmdComp :: CompletionScheme }

data CompletionScheme = None | Files | Names deriving (Eq, Show)

commands :: [Cmd]
commands =
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

processCmd :: T.Text -> MidaIO ()
processCmd txt =
  case find g commands of
    Just Cmd { cmdFunc = f } -> f . T.unpack . T.strip $ args
    Nothing -> liftIO $
               fprint ("Unknown command, try " % string % "help.\n") cmdPrefix
  where g Cmd { cmdName = c } = c == dropCmdPrefix (T.unpack cmd)
        (cmd, args)           = T.break isSpace (T.strip txt)

completionFunc :: L.CompletionFunc MidaIO
completionFunc = L.completeWordWithPrev Nothing " " getCompletions

getCompletions :: String -> String -> MidaIO [L.Completion]
getCompletions prev word = do
  names <- liftEnv getRefs
  files <- L.listFiles word
  let cmds    = (cmdPrefix ++) . cmdName <$> commands
      g None  = []
      g Files = files
      g Names = f names
  return $ case words . reverse $ prev of
             []    -> f $ cmds ++ names
             (c:_) -> case c `elemIndex` cmds of
                        Just i  -> g . cmdComp $ commands !! i
                        Nothing -> f names
    where f = fmap L.simpleCompletion . filter (word `isPrefixOf`)

cmdCd :: String -> MidaIO ()
cmdCd path = liftIO $ do
  new     <- addTrailingPathSeparator . (</> path) <$> getCurrentDirectory
  present <- doesDirectoryExist new
  if present
  then do corrected <- canonicalizePath new
          setCurrentDirectory corrected
          fprint ("Changed to \"" % string % "\".\n") corrected
  else fprint ("Cannot cd to \"" % string % "\".\n") new

cmdClear :: String -> MidaIO ()
cmdClear _ = liftEnv clearDefs >> liftIO (T.putStrLn "Environment cleared.")

cmdDef :: String -> MidaIO ()
cmdDef arg = mapM_ f (words arg)
  where f name = liftEnv (getSrc name) >>= liftIO . T.putStr

cmdHelp :: String -> MidaIO ()
cmdHelp _ = liftIO (T.putStrLn "Available commands:") >> mapM_ f commands
  where f Cmd { cmdName = c, cmdDesc = d } = liftIO $ fprint fmt cmdPrefix c d
        fmt = ("  " % string % (right 24 ' ' %. string) % text % "\n")

cmdLoad' :: String -> MidaIO ()
cmdLoad' = cmdLoad . words

cmdLoad :: [String] -> MidaIO()
cmdLoad = mapM_ loadOne

loadOne :: String -> MidaIO ()
loadOne given = do
  file <- output given ""
  b    <- liftIO $ doesFileExist file
  if b
  then do contents <- liftIO $ T.readFile file
          case parseMida (takeFileName file) contents of
            Right x -> do mapM_ f x
                          setFileName file
                          liftIO $ fprint
                                 ("\"" % string % "\" loaded successfully.\n")
                                 file
            Left  x -> liftIO $ fprint (string % "\n") x
  else liftIO $ fprint ("Could not find \"" % string % "\".\n") file
    where f (Definition n t) = processDef n t
          f (Exposition   _) = return ()

cmdMake' :: String -> MidaIO ()
cmdMake' arg =
  let (s:q:b:f:_) = words arg ++ repeat ""
  in cmdMake (parseNum s dfltSeed)
             (parseNum q dfltQuarter)
             (parseNum b dfltBeats)
             f

cmdMake :: Int -> Int -> Int -> String -> MidaIO ()
cmdMake s q b f = do
  file   <- output f "mid"
  midi   <- liftEnv $ genMidi s q b
  result <- liftIO $ try (Midi.exportFile file midi)
  case result of
    Right _ -> liftIO $ fprint ("MIDI file saved as \"" % string % "\".\n") file
    Left  e -> spitExc e

cmdProg :: String -> MidaIO ()
cmdProg arg = do
  prog <- getProg
  setProg $ parseNum (trim arg) prog

cmdPrv :: String -> MidaIO ()
cmdPrv arg = do
  prvcmd  <- getPrvCmd
  progOp  <- getProgOp
  prog    <- show <$> getProg
  tempoOp <- getTempoOp
  tempo   <- show <$> getTempo
  temp    <- liftIO getTemporaryDirectory
  f       <- output "" "mid"
  let (s:q:b:_) = words arg ++ repeat ""
      file      = temp </> takeFileName f
      cmd       = unwords [prvcmd, progOp, prog, tempoOp, tempo, file]
  cmdMake (parseNum s dfltSeed)
          (parseNum q dfltQuarter)
          (parseNum b dfltBeats)
          file
  (_, _, _, ph) <- liftIO $ createProcess (shell cmd) { delegate_ctlc = True }
  liftIO . void $ waitForProcess ph

cmdLength :: String -> MidaIO ()
cmdLength x = getPrevLen >>= setPrevLen . parseNum x

cmdPurge :: String -> MidaIO ()
cmdPurge _ = do
  liftEnv $ purgeEnv topDefs
  liftIO $ T.putStrLn "Environment purged."

cmdPwd :: String -> MidaIO ()
cmdPwd _ = liftIO (getCurrentDirectory >>= putStrLn)

cmdQuit :: String -> MidaIO ()
cmdQuit _ = liftIO exitSuccess

cmdSave :: String -> MidaIO ()
cmdSave given = do
  file   <- output given ""
  src    <- liftEnv fullSrc
  result <- liftIO (try (T.writeFile file src) :: IO (Either SomeException ()))
  let fmt = "Environment saved as \"" % string % "\".\n"
  case result of
    Right _ -> do setFileName file
                  liftIO $ fprint fmt file
    Left  e -> spitExc e

cmdTempo :: String -> MidaIO ()
cmdTempo arg = do
  tempo <- getTempo
  setTempo $ parseNum (trim arg) tempo

cmdUdef :: String -> MidaIO ()
cmdUdef arg = mapM_ f (words arg)
  where f name = do liftEnv $ remDef name
                    liftIO $ fprint fmt name
        fmt = "Definition for «" % string % "» removed.\n"

parseNum :: (Num a, Read a) => String -> a -> a
parseNum s x = fromMaybe x $ fst <$> listToMaybe (reads s)

output :: String -> String -> MidaIO String
output given ext = do
  actual <- getSrcFile
  home   <- liftIO getHomeDirectory
  let a = if null ext then actual else replaceExtension actual ext
      g = joinPath . fmap f . splitDirectories $ given
      f x = if x == "~" then home else x
  return $ if null given then a else g

setFileName :: FilePath -> MidaIO ()
setFileName path = (</> path) <$> liftIO getCurrentDirectory >>= setSrcFile

dropCmdPrefix :: String -> String
dropCmdPrefix arg
  | cmdPrefix `isPrefixOf` arg = drop (length cmdPrefix) arg
  | otherwise = arg

cmdPrefix :: String
cmdPrefix = ":"

spitExc :: SomeException -> MidaIO ()
spitExc = liftIO . fprint ("× " % string % ".\n") . show

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
