-- -*- Mode: Haskell; -*-
--
-- This modules describes all supported MIDA commands. It also provides all
-- the functionality to load source files and generate / save MIDI files.
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

module Mida.Interaction.Commands
    ( processCmd
    , completionFunc
    , cmdLoad
    , cmdMake
    , cmdPrefix )
where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char (isDigit, isSpace)
import Data.List (find, isPrefixOf)
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
import Text.Printf (printf)

import qualified Codec.Midi as Midi
import qualified System.Console.Haskeline as L

import Mida.Interaction.Base
import Mida.Language
import Mida.Midi
import Mida.Representation

data Cmd = Cmd
    { cmdName :: String
    , cmdFunc :: String -> MidaIO ()
    , cmdDesc :: String
    , cmdComp :: CompletionScheme }

data CompletionScheme = None | Files | Names deriving (Eq, Show)

commands :: [Cmd]
commands =
    [ Cmd "cd"      cmdCd      "Change working directory."             Files
    , Cmd "clear"   cmdClear   "Restore default state of environment." None
    , Cmd "def"     cmdDef     "Print definition of given symbol."     Names
    , Cmd "help"    cmdHelp    "Show this help text."                  None
    , Cmd "license" cmdLicense "Show license."                         None
    , Cmd "load"    cmdLoad    "Load definitions from given file."     Files
    , Cmd "make"    cmdMake'   "Generate and save MIDI file."          Files
    , Cmd "prog"    cmdProg    "Set program for preview."              None
    , Cmd "prv"     cmdPrv     "Play the score with external program." None
    , Cmd "prvlen"  cmdLength  "Set length of displayed results."      None
    , Cmd "purge"   cmdPurge   "Remove redundant definitions."         None
    , Cmd "pwd"     cmdPwd     "Print working directory."              None
    , Cmd "quit"    cmdQuit    "Quit the interactive environment."     None
    , Cmd "save"    cmdSave    "Save current environment in file."     Files
    , Cmd "tempo"   cmdTempo   "Set tempo for preview."                None
    , Cmd "udef"    cmdUdef    "Remove definition of given symbol."    Names ]

processCmd :: String -> MidaIO ()
processCmd input =
    case find g commands of
      Just Cmd { cmdFunc = f } -> f (trim args)
      Nothing -> liftIO $ printf "Unknown command, try %shelp.\n" cmdPrefix
    where g Cmd { cmdName = c } = c == dropCmdPrefix cmd
          (cmd, args)           = break isSpace input

completionFunc :: L.CompletionFunc MidaIO
completionFunc = L.completeWordWithPrev Nothing " " getCompletions

getCompletions :: String -> String -> MidaIO [L.Completion]
getCompletions prev word = do
  names <- liftEnv getRefs
  files <- L.listFiles word
  let cmds  = (cmdPrefix ++) . cmdName <$> commands
      g None  = []
      g Files = files
      g Names = f names
  return $ case words . reverse $ prev of
             []    -> f $ cmds ++ names
             (c:_) -> if c `elem` cmds
                      then maybe [] (g . cmdComp) $
                           find ((== dropCmdPrefix c) . cmdName) commands
                      else f names
    where f = map L.simpleCompletion . filter (word `isPrefixOf`)

cmdCd :: String -> MidaIO ()
cmdCd path = liftIO $ do
  new     <- addTrailingPathSeparator . (</> path) <$> getCurrentDirectory
  present <- doesDirectoryExist new
  if present
  then do corrected <- canonicalizePath new
          setCurrentDirectory corrected
          printf "Changed to \"%s\".\n" corrected
  else printf "Cannot cd to \"%s\".\n" new

cmdClear :: String -> MidaIO ()
cmdClear _ = liftEnv clearDefs >> liftIO (printf "Environment cleared.\n")

cmdDef :: String -> MidaIO ()
cmdDef arg = mapM_ f (words arg)
    where f name = liftEnv (getSrc name) >>= liftIO . putStr

cmdHelp :: String -> MidaIO ()
cmdHelp _ = liftIO (printf "Available commands:\n") >> mapM_ f commands
    where f Cmd { cmdName = c, cmdDesc = d } =
              liftIO $ printf "  %s%-24s%s\n" cmdPrefix c d

cmdLicense :: String -> MidaIO ()
cmdLicense _ = liftIO $ putStr
    "MIDA - realization of MIDA, language for generation of MIDI files.\n\
    \Copyright (c) 2014, 2015 Mark Karpov\n\
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

cmdLoad :: String -> MidaIO ()
cmdLoad given = do
  file <- output given ""
  b    <- liftIO $ doesFileExist file
  if b
  then do contents <- liftIO $ readFile file
          case parseMida (takeFileName file) contents of
            Right x -> do mapM_ f x
                          setFileName file
                          liftIO $ printf "\"%s\" loaded successfully.\n" file
            Left  x -> liftIO $ printf "Parse error in %s.\n" x
  else liftIO $ printf "Could not find \"%s\".\n" file
    where f (Definition n t) = processDef n t
          f (Exposition   _) = return ()

cmdMake' :: String -> MidaIO ()
cmdMake' arg =
    let (s:q:b:f:_) = words arg ++ repeat ""
    in cmdMake (parseInt s dfltSeed)
               (parseInt q dfltQuarter)
               (parseInt b dfltBeats)
               f

cmdMake :: Int -> Int -> Int -> String -> MidaIO ()
cmdMake s q b f = do
  file <- output f "mid"
  midi <- liftEnv $ genMidi s q b
  result <- liftIO $ try (Midi.exportFile file midi)
  case result of
    Right _ -> liftIO $ printf "MIDI file saved as \"%s\".\n" file
    Left e -> spitExc e

cmdProg :: String -> MidaIO ()
cmdProg arg = do
  prog <- getProg
  setProg $ parseInt (trim arg) prog

cmdPrv :: String -> MidaIO ()
cmdPrv arg = do
  prvcmd  <- getPrvCmd
  progOp  <- getProgOp
  prog    <- show <$> getProg
  tempoOp <- getTempoOp
  tempo   <- show <$> getTempo
  temp <- liftIO getTemporaryDirectory
  f    <- output "" "mid"
  let (s:q:b:_) = words arg ++ repeat ""
      file      = temp </> takeFileName f
      cmd       = unwords [prvcmd, progOp, prog, tempoOp, tempo, file]
  cmdMake (parseInt s dfltSeed)
          (parseInt q dfltQuarter)
          (parseInt b dfltBeats)
          file
  (_, _, _, ph) <- liftIO $ createProcess (shell cmd) { delegate_ctlc = True }
  liftIO . void $ waitForProcess ph

cmdLength :: String -> MidaIO ()
cmdLength x = getPrevLen >>= setPrevLen . parseInt x

cmdPurge :: String -> MidaIO ()
cmdPurge _ = do
  liftEnv $ purgeEnv topDefs
  liftIO $ printf "Environment purged.\n"

cmdPwd :: String -> MidaIO ()
cmdPwd _ = liftIO (getCurrentDirectory >>= putStrLn)

cmdQuit :: String -> MidaIO ()
cmdQuit _ = liftIO (printf "Goodbye.\n") >> liftIO exitSuccess

cmdSave :: String -> MidaIO ()
cmdSave given = do
  file   <- output given ""
  src    <- liftEnv fullSrc
  result <- liftIO (try (writeFile file src) :: IO (Either SomeException ()))
  case result of
    Right _ -> setFileName file >>
               liftIO (printf "Environment saved as \"%s\".\n" file)
    Left  e -> spitExc e

cmdTempo :: String -> MidaIO ()
cmdTempo arg = do
  tempo <- getTempo
  setTempo $ parseInt (trim arg) tempo

cmdUdef :: String -> MidaIO ()
cmdUdef arg = mapM_ f (words arg)
    where f name = liftEnv (remDef name) >>
                   liftIO (printf "Definition for '%s' removed.\n" name)

parseInt :: String -> Int -> Int
parseInt s x
    | null s        = x
    | all isDigit s = read s :: Int
    | otherwise     = x

output :: String -> String -> MidaIO String
output given ext = do
  actual <- getSrcFile
  home   <- liftIO getHomeDirectory
  let a = if null ext then actual else replaceExtension actual ext
      g = joinPath . map f . splitDirectories $ given
      f x = if x == "~" then home else x
  return $ if null given then a else g

setFileName :: FilePath -> MidaIO ()
setFileName path = (</> path) <$> liftIO getCurrentDirectory >>= setSrcFile

dropCmdPrefix :: String -> String
dropCmdPrefix arg
    | cmdPrefix `isPrefixOf` arg = drop (length cmdPrefix) arg
    | otherwise                  = arg

cmdPrefix :: String
cmdPrefix = ":"

spitExc :: SomeException -> MidaIO ()
spitExc = liftIO . putStrLn . printf "(!) %s." . show
