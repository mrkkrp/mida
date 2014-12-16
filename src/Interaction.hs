-- -*- Mode: HASKELL; -*-
--
-- This module describes how MIDA processes commands in interactive
-- mode. These commands are also used in batch mode.
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

module Interaction
    ( loadSrc
    , saveMidi
    , interLoop )
where

import Data.Char (isSpace)
import Data.List
import Control.Exception (SomeException, try)
import Control.Monad.State.Strict
import System.FilePath
import System.Directory (getHomeDirectory, getTemporaryDirectory, doesFileExist)
import System.Process (shell, createProcess, waitForProcess, delegate_ctlc)
import Control.Applicative ((<$>), (<*>))
import System.IO
import Text.Printf (printf)
import Codec.Midi (exportFile)
import qualified System.Console.Haskeline as L
import Parser
import Environment
import Eval
import Translator
import Config
import Defaults

-- constants --

version   = "0.3.0"
cmdPrefix = ":"

-- misc --

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

aCmd :: String -> Bool
aCmd = isPrefixOf cmdPrefix . trim

isCmd :: String -> String -> Bool
isCmd x = (isPrefixOf $ cmdPrefix ++ x) . trim

fancyPrint :: String -> IO ()
fancyPrint = putStr . unlines . map ("=> " ++) . lines

printExc :: SomeException -> IO ()
printExc e = hPutStr stderr $ printf "-> %s;\n" (show e)

output :: String -> String -> StateT Env IO String
output given ext =
    do actual <- getFileName
       home   <- liftIO $ getHomeDirectory
       let a = if null ext then actual else replaceExtension actual ext
           g = joinPath . map f . splitDirectories $ given
               where f x = if x == "~" then home else x
       return $ if null given then a else g

processDef :: String -> Principle -> String -> StateT Env IO ()
processDef n e s =
    do b <- checkRecursion n e
       if b
       then liftIO $ printf "-> rejected recursive definition for '%s';\n" n
       else addDef n e s >> (liftIO $ printf "-> defined '%s';\n" n)

loadSrc :: String -> StateT Env IO ()
loadSrc file =
    do contents <- liftIO $ readFile file
       case parseMida (takeFileName file) contents of
         Right x -> do mapM_ f x
                       setFileName file
                       liftIO $ printf "-> \"%s\" loaded successfully;\n" file
         Left  x -> liftIO $ fancyPrint $ printf "parse error in %s;\n" x
       where f (Definition n e s) = processDef n e s
             f (Exposition     _) = return ()

saveMidi :: Int -> Int -> Int -> String -> StateT Env IO ()
saveMidi s q b given =
    do midi <- genMidi s q b
       file <- output given "mid"
       result <- liftIO (try (exportFile file midi)
                         :: IO (Either SomeException ()))
       case result of
         Right _ -> liftIO $ printf "-> MIDI file saved as \"%s\";\n" file
         Left  e -> liftIO $ printExc e

prettyList :: [Int] -> String
prettyList [] = "=> none"
prettyList xs = printf "=> %s..." $ intercalate " " (map show xs)

unfin :: String -> Bool
unfin arg = or [isSuffixOf "," s, f "[]", f "{}", f "<>", f "()"]
    where s = trim arg
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)
          g x     = length $ filter (== x) s

loadConfig :: String -> StateT Env IO ()
loadConfig file =
    do params <- parseConfig file <$> liftIO (readFile file)
       case params of
         (Right x) -> do prompt <- getPrompt
                         setPrompt    $ lookupStr x "prompt" prompt
                         prvlen <- getPrvLength
                         setPrvLength $ lookupInt x "prvlen" prvlen
                         block  <- getBlockSize
                         setBlockSize $ lookupInt x "block"  block
                         prvcmd <- getPrvCmd
                         setPrvCmd    $ lookupStr x "prvcmd" prvcmd
                         liftIO $ printf prvcmd
         (Left  _) -> return ()

-- interaction --

commands = [ ("block",   cmdBlock,   "Set size of block")
           , ("clear",   cmdClear,   "Restore default state of environment")
           , ("def",     cmdDef,     "Print definition of given symbol")
           , ("help",    cmdHelp,    "Show this help text")
           , ("license", cmdLicense, "Show license")
           , ("load",    cmdLoad,    "Load definitions from given file.")
           , ("make",    cmdMake,    "Generate and save MIDI file")
           , ("prv",     cmdPreview, "Listen to score using external program")
           , ("prvlen",  cmdLength,  "Set length of displayed results")
           , ("prompt",  cmdPrompt,  "Set MIDA prompt")
           , ("purge",   cmdPurge,   "Remove redundant definitions")
           , ("quit",    undefined,  "Quit the interactive environment")
           , ("save",    cmdSave,    "Save current environment in file") ]

processCmd :: String -> StateT Env IO ()
processCmd input =
    case find f commands of
      (Just (_, x, _)) -> x args
      Nothing  -> liftIO $ printf "-> unknown command, try %shelp;\n" cmdPrefix
    where f (x, _, _)    = x == cmd
          (cmd', args')  = break isSpace (trim input)
          cmd            = drop (length cmdPrefix) cmd'
          args           = trim args'

processExpr :: String -> StateT Env IO ()
processExpr expr =
    do file    <- getFileName
       case parseMida file expr of
         (Right x) -> mapM_ f x
         (Left  x) -> liftIO $ fancyPrint $ printf "parse error in %s;\n" x
       where f (Definition n e s) = processDef n e s
             f (Exposition e) =
                 do l <- getPrvLength
                    r <- eval e l
                    liftIO . putStrLn . prettyList $ take l r

getMultiline :: String -> L.InputT (StateT Env IO) (Maybe String)
getMultiline prv =
    do prompt <- lift getPrompt
       input  <- L.getInputLine $ if null prv
                                  then prompt
                                  else replicate (length prompt) ' '
       case (prv ++) . (++ "\n") <$> input of
         (Just x) -> if unfin x
                     then getMultiline x
                     else return (Just x)
         Nothing  -> return Nothing

interaction :: L.InputT (StateT Env IO) ()
interaction =
    do input <- getMultiline ""
       case input of
         (Just x) -> if isCmd "quit" x
                     then return ()
                     else do if aCmd x
                             then lift $ processCmd x
                             else lift $ processExpr x
                             interaction
         Nothing    -> return ()

getCompletions :: MonadIO m => String -> StateT Env m [L.Completion]
getCompletions arg =
    do names <- getNames
       files <- lift $ L.listFiles arg
       return $ files ++ (map L.simpleCompletion $
                              filter (arg `isPrefixOf`) (names ++ cmds))
       where cmds = map (\(x, _, _) -> cmdPrefix ++ x) commands

completionFunc :: MonadIO m => L.CompletionFunc (StateT Env m)
completionFunc = L.completeWord Nothing " " getCompletions

interLoop :: StateT Env IO ()
interLoop =
    do liftIO $ hSetBuffering stdin LineBuffering
       home <- liftIO $ getHomeDirectory
       let file = home </> ".mida"
       exist <- liftIO $ doesFileExist file
       when exist (loadConfig file)
       liftIO $ printf "-> Loading MIDA Interactive Environment v%s;\n" version
       L.runInputT (L.setComplete completionFunc L.defaultSettings) interaction
       liftIO $ printf "-> Goodbye.\n"

-- commands --

cmdHelp :: String -> StateT Env IO ()
cmdHelp _ = liftIO (printf "Available commands:\n") >> mapM_ f commands
    where f (cmd, _, text) = liftIO $ printf "  %s%-24s%s\n" cmdPrefix cmd text

cmdLicense :: String -> StateT Env IO ()
cmdLicense _ = liftIO $ fancyPrint
    "MIDA - realization of MIDA, language for generation of MIDI files.\n\
    \Copyright (c) 2014 Mark Karpov\n\
    \\n\
    \This program is free software: you can redistribute it and/or modify\n\
    \it under the terms of the GNU General Public License as published by\n\
    \the Free Software Foundation, either version 3 of the License, or\n\
    \(at your option) any later version.\n\
    \\n\
    \This program is distributed in the hope that it will be useful,\n\
    \but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    \MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
    \GNU General Public License for more details.\n\
    \\n\
    \You should have received a copy of the GNU General Public License\n\
    \along with this program.  If not, see <http://www.gnu.org/licenses/>.\n"

cmdLoad :: String -> StateT Env IO ()
cmdLoad given =
    do file <- output given ""
       b    <- liftIO $ doesFileExist file
       if b
       then (loadSrc file)
       else liftIO $ printf "-> could not find \"%s\";\n" file

cmdSave :: String -> StateT Env IO ()
cmdSave given =
    do file   <- output given ""
       src    <- fullSrc
       result <- liftIO (try (writeFile file src)
                         :: IO (Either SomeException ()))
       case result of
         Right _ -> do setFileName file
                       liftIO $ printf "-> environment saved as \"%s\".\n" file
         Left  e -> liftIO $ printExc e

cmdPurge :: String -> StateT Env IO ()
cmdPurge _ =
    do purgeEnv topDefs
       liftIO $ printf "-> environment purged;\n"

cmdClear :: String -> StateT Env IO ()
cmdClear _ =
    do setDefs dfltDefs
       liftIO $ printf "-> environment cleared;\n"

cmdMake :: String -> StateT Env IO ()
cmdMake arg =
    do let (s:q:b:f:_) = words arg ++ repeat ""
       file <- output f "mid"
       saveMidi (parseInt s dfltSeed)
                (parseInt q dfltQuarter)
                (parseInt b dfltBeats)
                file

cmdPreview :: String -> StateT Env IO ()
cmdPreview arg =
    do prvcmd <- getPrvCmd
       if null prvcmd
       then liftIO $ printf
                "-> please set variable 'prvcmd' in \".mida\" file;\n"
       else do temp <- liftIO $ getTemporaryDirectory
               f    <- output "" "mid"
               let (s:q:b:_) = words arg ++ repeat ""
                   file      = combine temp (takeFileName f)
               saveMidi (parseInt s dfltSeed)
                        (parseInt q dfltQuarter)
                        (parseInt b dfltBeats)
                        file
               (_, _, _, ph) <- liftIO $ createProcess
                                (shell $ prvcmd ++ " " ++ file)
                                { delegate_ctlc = True }
               liftIO $ waitForProcess ph
               liftIO $ printf "-> done;\n"

cmdDef :: String -> StateT Env IO ()
cmdDef name =
    do src <- getSrc name
       liftIO $ fancyPrint src

cmdPrompt :: String -> StateT Env IO ()
cmdPrompt x = setPrompt (x ++ " ")

cmdLength :: String -> StateT Env IO ()
cmdLength x =
    do old <- getPrvLength
       setPrvLength $ parseInt x old

cmdBlock :: String -> StateT Env IO ()
cmdBlock x =
    do old <- getBlockSize
       setBlockSize $ parseInt x old
