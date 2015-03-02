-- -*- Mode: Haskell; -*-
--
-- This module describes how MIDA processes commands in interactive
-- mode. These commands are also used in batch mode.
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

{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interaction
    ( cmdLoad
    , cmdMake
    , interaction
    , dfltSeed
    , dfltQuarter
    , dfltBeats )
where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Char (isDigit, isSpace)
import Data.List
import System.Directory (getHomeDirectory, doesFileExist, getTemporaryDirectory)
import System.Exit
import System.FilePath
import System.IO
import System.Process (shell, createProcess, waitForProcess, delegate_ctlc)
import Text.Printf (printf)

import Codec.Midi (exportFile)
import qualified System.Console.Haskeline as L

import Environment
import Eval
import Parser
import Translator

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

type MidaIO = MidaEnv IO

deriving instance L.MonadException m => L.MonadException (MidaEnv m)

data CompletionScheme = Files | Names deriving (Eq, Show)

----------------------------------------------------------------------------
--                               Constants                                --
----------------------------------------------------------------------------

version     = "0.4.0"
cmdPrefix   = ":"
dfltSeed    = 0  :: Int
dfltQuarter = 24 :: Int
dfltBeats   = 16 :: Int

----------------------------------------------------------------------------
--                         Top Level Interaction                          --
----------------------------------------------------------------------------

interaction :: MidaIO ()
interaction = do
  liftIO $ hSetBuffering stdin LineBuffering
  liftIO $ printf "MIDA Interactive Environment %s\n" version
  L.runInputT (L.setComplete completionFunc L.defaultSettings) midaRepl

midaRepl :: L.InputT MidaIO ()
midaRepl = do
  input <- getMultiline ""
  case input of
    Just x  -> do if cmdPrefix `isPrefixOf` (trim x)
                  then lift $ processCmd x
                  else lift $ processExpr x
                  midaRepl
    Nothing -> return ()

getMultiline :: String -> L.InputT MidaIO (Maybe String)
getMultiline prv = do
  prompt <- lift getPrompt
  input  <- L.getInputLine $ if null prv
                             then prompt
                             else replicate (length prompt) ' '
  case (prv ++) . (++ "\n") <$> input of
    Just x -> if incompleteInput x
              then getMultiline x
              else return (Just x)
    Nothing -> return Nothing

processCmd :: String -> MidaIO ()
processCmd input =
    case find f commands of
      Just (_, x, _) -> x args
      Nothing -> liftIO $ printf "Unknown command, try %shelp.\n" cmdPrefix
    where f (x, _, _)   = x == cmd
          (cmd', args') = break isSpace (trim input)
          cmd           = drop (length cmdPrefix) cmd'
          args          = trim args'

processExpr :: String -> MidaIO ()
processExpr expr = do
  file <- getSrcFile
  case parseMida file expr of
    Right x -> mapM_ f x
    Left  x -> liftIO $ printf "Parse error in %s.\n" x
    where f (Definition n e s) = processDef n e s
          f (Exposition e) = do length  <- getPrevLen
                                verbose <- getVerbose
                                result  <- eval e
                                built   <- build e
                                when verbose $ spitPrin built
                                spitList $ take length result

----------------------------------------------------------------------------
--                                Commands                                --
----------------------------------------------------------------------------

commands =
    [ ("clear",   cmdClear,   "Restore default state of environment.")
    , ("def",     cmdDef,     "Print definition of given symbol."    )
    , ("help",    cmdHelp,    "Show this help text."                 )
    , ("license", cmdLicense, "Show license."                        )
    , ("load",    cmdLoad,    "Load definitions from given file."    )
    , ("make",    cmdMake',   "Generate and save MIDI file."         )
    , ("prog",    cmdProg,    "Set program for preview."             )
    , ("prv",     cmdPrv,     "Play the score with external program.")
    , ("prvlen",  cmdLength,  "Set length of displayed results."     )
    , ("purge",   cmdPurge,   "Remove redundant definitions."        )
    , ("quit",    cmdQuit,    "Quit the interactive environment."    )
    , ("save",    cmdSave,    "Save current environment in file."    )
    , ("tempo",   cmdTempo,   "Set tempo for preview."               ) ]

cmdClear :: String -> MidaIO ()
cmdClear _ = clearDefs >> (liftIO $ printf "Environment cleared.\n")

cmdDef :: String -> MidaIO ()
cmdDef name = getSrc name >>= liftIO . putStr

cmdHelp :: String -> MidaIO ()
cmdHelp _ = (liftIO $ printf "Available commands:\n") >> mapM_ f commands
    where f (cmd, _, text) = liftIO $ printf "  %s%-24s%s\n" cmdPrefix cmd text

cmdLicense :: String -> MidaIO ()
cmdLicense _ = liftIO $ putStr
    "MIDA - realization of MIDA, language for generation of MIDI files.\n\
    \Copyright (c) 2014, 2015 Mark Karpov\n\
    \\n\
    \This program is free software: you can redistribute it and/or modify\n\
    \it under the terms of the GNU General Public License as published by\n\
    \the Free Software Foundation, either version 3 of the License, or\n\
    \(at your option) any later version.\n\
    \\n\
    \This program is distributed in the hope that it will be useful,\n\
    \but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the\n\
    \GNU General Public License for more details.\n\
    \\n\
    \You should have received a copy of the GNU General Public License\n\
    \along with this program. If not, see <http://www.gnu.org/licenses/>.\n"

cmdLoad :: String -> MidaIO ()
cmdLoad given = do
  file <- output given ""
  b    <- liftIO $ doesFileExist file
  if b
  then do contents <- liftIO $ readFile file
          case parseMida (takeFileName file) contents of
            Right x -> do mapM_ f x
                          setSrcFile file
                          liftIO $ printf "\"%s\" loaded successfully.\n" file
            Left  x -> liftIO $ printf "Parse error in %s.\n" x
  else liftIO $ printf "Could not find \"%s\".\n" file
    where f (Definition n e s) = processDef n e s
          f (Exposition     _) = return ()

cmdMake' :: String -> MidaIO ()
cmdMake' arg =
    let (s:q:b:f:_) = words arg ++ repeat ""
    in cmdMake (parseInt s dfltSeed)
               (parseInt q dfltQuarter)
               (parseInt b dfltBeats)
               f

cmdMake :: Int -> Int -> Int -> String -> MidaIO ()
cmdMake s q b f = do
  file   <- output f "mid"
  midi   <- genMidi s q b
  result <- liftIO (try (exportFile file midi) :: IO (Either SomeException ()))
  case result of
    Right _ -> liftIO $ printf "MIDI file saved as \"%s\".\n" file
    Left  e -> spitExc e

cmdProg :: String -> MidaIO ()
cmdProg arg = do
  prog <- getProg
  setProg $ parseInt (head $ words arg) prog

cmdPrv :: String -> MidaIO ()
cmdPrv arg = do
  prvcmd  <- getPrvCmd
  progOp  <- getProgOp
  prog    <- show <$> getProg
  tempoOp <- getTempoOp
  tempo   <- show <$> getTempo
  temp <- liftIO $ getTemporaryDirectory
  f    <- output "" "mid"
  let (s:q:b:_) = words arg ++ repeat ""
      file      = combine temp (takeFileName f)
      cmd       = intercalate " " [prvcmd,progOp,prog,tempoOp,tempo,file]
  cmdMake (parseInt s dfltSeed)
          (parseInt q dfltQuarter)
          (parseInt b dfltBeats)
          file
  (_, _, _, ph) <- liftIO $ createProcess (shell cmd) { delegate_ctlc = True }
  liftIO . void $ waitForProcess ph

cmdLength :: String -> MidaIO ()
cmdLength x = getPrevLen >>= setPrevLen . parseInt x

cmdPurge :: String -> MidaIO ()
cmdPurge _ = purgeEnv topDefs >> (liftIO $ printf "Environment purged.\n")

cmdQuit :: String -> MidaIO ()
cmdQuit _ = (liftIO $ printf "Goodbye.\n") >> (liftIO $ exitSuccess)

cmdSave :: String -> MidaIO ()
cmdSave given = do
  file   <- output given ""
  src    <- fullSrc
  result <- liftIO (try (writeFile file src) :: IO (Either SomeException ()))
  case result of
    Right _ -> setSrcFile file >>
               (liftIO $ printf "Environment saved as \"%s\".\n" file)
    Left  e -> spitExc e

cmdTempo :: String -> MidaIO ()
cmdTempo arg = do
  tempo <- getTempo
  setTempo $ parseInt (head $ words arg) tempo

----------------------------------------------------------------------------
--                                  Misc                                  --
----------------------------------------------------------------------------

processDef :: String -> SyntaxTree -> String -> MidaIO ()
processDef n e s = do
  b <- checkRecur n e
  if b
  then liftIO $ printf "Rejected recursive definition for '%s'.\n" n
  else addDef n e s >> (liftIO $ printf "Defined '%s'.\n" n)

incompleteInput :: String -> Bool
incompleteInput arg = or [isSuffixOf "," s, f "[]", f "{}", f "<>", f "()"]
    where s       = trim arg
          g x     = length $ filter (== x) s
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)

completionFunc :: L.CompletionFunc MidaIO
completionFunc = L.completeWordWithPrev Nothing " " getCompletions

cmdCompletion =
    [ ("def",  Names)
    , ("load", Files)
    , ("make", Files)
    , ("save", Files) ]

getCompletions :: String -> String -> MidaIO [L.Completion]
getCompletions prev word = do
  names <- getRefs
  files <- L.listFiles word
  let prev' = words . reverse $ prev
      cmds  = map (\(x, _, _) -> cmdPrefix ++ x) commands
      cmpl  = map (\(x, y) -> (cmdPrefix ++ x, y)) cmdCompletion
      g Files = files
      g Names = f names
  return $ case prev' of
             []    -> f $ cmds ++ names
             (c:_) -> if c `elem` cmds
                      then maybe [] (g . snd) (find ((== c) . fst) cmpl)
                      else f names
    where f = map L.simpleCompletion . filter (word `isPrefixOf`)

output :: String -> String -> MidaIO String
output given ext = do
  actual <- getSrcFile
  home   <- liftIO getHomeDirectory
  let a = if null ext then actual else replaceExtension actual ext
      g = joinPath . map f . splitDirectories $ given
      f x = if x == "~" then home else x
  return $ if null given then a else g

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

parseInt :: String -> Int -> Int
parseInt s x
    | null s        = x
    | all isDigit s = read s :: Int
    | otherwise     = x

spitExc :: SomeException -> MidaIO ()
spitExc = liftIO . putStrLn . printf "(!) %s." . show

spitList :: Show a => [a] -> MidaIO ()
spitList [] = liftIO $ printf "none\n"
spitList xs = liftIO $ printf "%s...\n" $ intercalate " " (show <$> xs)

spitPrin :: Principle -> MidaIO ()
spitPrin = liftIO . putStrLn . ("= "++) . cm "" "" f
    where cm b e g xs   = b ++ (intercalate " " $ map g xs) ++ e
          f (Value   x) = show x
          f (Section x) = cm "[" "]" f x
          f (Multi   x) = cm "{" "}" f x
          f (CMulti  x) = cm "{" "}" ((++) <$> (c . fst) <*> (v . snd)) x
          c (Multi   x) = cm "<" "> " f x
          v (Multi   x) = cm "" "" f x
