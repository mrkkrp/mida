-- -*- Mode: HASKELL; -*-

-- Main module of MIDA interpreter.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Main where

-- Import Section --

import Parser
import Environment
import Translator
import Config
import Control.Monad.State
import qualified Data.Map.Lazy as M
import System.Random.Mersenne.Pure64
import Options.Applicative
import System.FilePath (takeFileName, replaceExtension, combine)
import System.IO
import Text.Printf (printf)
import Codec.Midi (exportFile)
import Data.Char (isSpace, isDigit)
import Data.List
import Control.Exception
import System.Directory (doesFileExist, getHomeDirectory)

-- Default Values --

version         = "0.1.0"
cmdChar         = ':'
dfltDefinitions = M.empty
dfltRandGen     = pureMT 0
dfltPrompt      = "mida> "
dfltPrv         = 16
dfltFileName    = "interactive"
dfltSeed        = 0
dfltQuarter     = 24
dfltBeats       = 16

-- Commnad Line Processing --

data Opts = Opts
    { getIntr :: Bool
    , getSeed :: Int
    , getQ    :: Int
    , getBars :: Int
    , getOut  :: String
    , getFile :: String }

opts :: ParserInfo Opts
opts =  info (helper <*> bar)
      ( fullDesc
     <> progDesc "starts MIDA interpreter or translates source into MIDI file"
     <> header "mida - interpreter for MIDA language" )
    where bar =  Opts
             <$> switch
               ( long    "interactive"
              <> short   'i'
              <> help    "Enable interactive session" )
             <*> option  auto
               ( long    "seed"
              <> short   's'
              <> metavar "SEED"
              <> value   dfltSeed
              <> help    "Set seed for MIDI generation, default is 0" )
             <*> option  auto
               ( long    "quarter"
              <> short   'q'
              <> metavar "TICKS"
              <> value   dfltQuarter
              <> help    "Set ticks per quarter note, default is 24" )
             <*> option  auto
               ( long    "beats"
              <> short   'b'
              <> metavar "BEATS"
              <> value   dfltBeats
              <> help    "Set total time in quarter notes, default is 16" )
             <*> strOption
               ( long    "output"
              <> short   'o'
              <> metavar "OUT"
              <> value   ""
              <> help    "Specify non-standard output filename" )
             <*> argument str
               ( metavar "FILE"
              <> value   "" )

-- Interactive Environment --

trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

aCmd :: String -> Bool
aCmd = isPrefixOf [cmdChar] . trim

isCmd :: String -> String -> Bool
isCmd x = (isPrefixOf $ cmdChar : x) . trim

commands = [ ("help",   cmdHelp,   "Show this help text")
           , ("save",   cmdSave,   "Save current enviroment in specified file")
           , ("purge",  cmdPurge,  "Remove redundant definitions")
           , ("make",   cmdMake,   "Generate MIDI file in current environment")
           , ("def",    cmdDef,    "Print definition of given symbol")
           , ("prompt", cmdPrompt, "Set MIDA prompt")
           , ("length", cmdLength, "Set length of displayed results")
           , ("quit",   undefined, "Quit the interactive environment") ]

printExc :: SomeException -> IO ()
printExc e = hPutStr stderr $ printf "-> %s;\n" (show e)

cmdHelp :: String -> StateT Env IO ()
cmdHelp _ = liftIO (printf "Available commands:\n") >> mapM_ f commands
    where f (cmd, _, text) = liftIO $ printf "  %c%-24s%s\n" cmdChar cmd text

cmdSave :: String -> StateT Env IO ()
cmdSave given =
    do actual <- getFileName
       let file = if null given then actual else given
       source <- getSource
       setFileName file
       liftIO $ catch (do writeFile file source
                          printf "-> environment saved as \"%s\".\n" file)
                      printExc

cmdPurge :: String -> StateT Env IO ()
cmdPurge _ =
    do purgeEnv topDefs
       liftIO $ printf "-> environment purged;\n"

safeParseInt :: String -> Int -> Int
safeParseInt str x
    | all isDigit str = read str :: Int
    | otherwise       = x

cmdMake :: String -> StateT Env IO ()
cmdMake str =
    do file <- getFileName
       saveMidi (safeParseInt s dfltSeed)
                (safeParseInt q dfltQuarter)
                (safeParseInt b dfltBeats)
                (output f file)
    where (s:q:b:f:_) = (words str) ++ repeat ""

cmdDef :: String -> StateT Env IO ()
cmdDef name =
    do def <- getSrc name
       liftIO $ putStr . unlines . map ("=> " ++) . lines $ def

cmdPrompt :: String -> StateT Env IO ()
cmdPrompt x = setPrompt (x ++ " ")

cmdLength :: String -> StateT Env IO ()
cmdLength x =
    do old <- getPrvLength
       setPrvLength $ safeParseInt x old

processCmd :: String -> StateT Env IO ()
processCmd input =
    case find f commands of
      (Just (_, x, _)) -> x args
      Nothing  -> liftIO $ printf "-> unknown command, try %chelp;\n" cmdChar
    where f (x, _, _)    = x == cmd
          (cmd', args')  = break isSpace input
          cmd            = filter (/= cmdChar) cmd'
          args           = trim args'

prettyList :: [Int] -> String
prettyList [] = "=> none"
prettyList xs = printf "=> %s..." $ intercalate " " (map show xs) 

processExpr :: String -> StateT Env IO ()
processExpr expr =
    do file    <- getFileName
       case parseMida file expr of
         (Right x) -> mapM_ f x
         (Left  x) -> liftIO $ printf "parse error in %s\n" x
       where f (Definition n e s) =
                 do addDef n e s
                    liftIO $ printf "-> defined '%s'\n" n
             f (Exposition e) =
                 do preview <- getPrvLength
                    result  <- eval e
                    liftIO $ putStrLn $ (prettyList . take preview) result

unfin :: String -> Bool
unfin str = or [isSuffixOf "," s, f "[]", f "{}", f "<>", f "()"]
    where s = trim str
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)
          g x     = length $ filter (== x) s

getMultiline :: String -> StateT Env IO String
getMultiline prv =
    do str <- (prv ++) . (++ "\n") <$> liftIO getLine
       len <- length <$> getPrompt
       if unfin str
       then do liftIO $ putStr (replicate len ' ')
               liftIO $ hFlush stdout
               getMultiline str
       else return str

interaction :: StateT Env IO ()
interaction =
    do prompt <- getPrompt
       liftIO $ putStr prompt
       liftIO $ hFlush stdout
       eof    <- liftIO isEOF
       if eof
       then liftIO (putChar '\n') >> return ()
       else do str <- getMultiline ""
               if isCmd "quit" str
               then return ()
               else if aCmd str
                    then processCmd  str >> interaction
                    else processExpr str >> interaction

loadConfig :: String -> StateT Env IO ()
loadConfig file =
    do params <- parseConfig file <$> liftIO (readFile file)
       case params of
         (Right p) -> do case lookup "prompt" p of
                           (Just x) -> setPrompt x
                           Nothing  -> return ()
                         case lookup "length" p of
                           (Just x) -> setPrvLength (safeParseInt x dfltPrv)
                           Nothing  -> return ()
         (Left  p) -> return ()

interLoop :: StateT Env IO ()
interLoop =
    do liftIO $ hSetBuffering stdin LineBuffering
       home <- liftIO $ getHomeDirectory
       let file = combine home ".mida"
       exist <- liftIO $ doesFileExist file
       when exist (loadConfig file)
       liftIO $ printf "-> Loading MIDA Interactive Environment v%s;\n" version
       interaction
       liftIO $ printf "-> Goodbye.\n"

-- Top Level Logic --

loadFile :: String -> StateT Env IO ()
loadFile file =
    do contents <- liftIO $ readFile file
       case parseMida (takeFileName file) contents of
         (Right x) -> mapM_ f x
         (Left  x) -> error $ "parse error in " ++ x
       liftIO $ printf "-> \"%s\" loaded successfully;\n" file
       where f (Definition n e s) = addDef n e s
             f (Exposition e)     =
                 error "source file does not contain valid definitions"

output :: String -> String -> String
output out file = if null out then f file else out
    where f x = replaceExtension x ".mid"

saveMidi :: Int -> Int -> Int -> String -> StateT Env IO ()
saveMidi s q b file =
    do midi <- getMidi s q b
       liftIO $ exportFile file midi
       liftIO $ printf "-> MIDI file saved as \"%s\".\n" file

sm :: StateT Env IO () -> IO ()
sm x = void $ runStateT x Env { eDefinitions  = dfltDefinitions
                              , eRandGen      = dfltRandGen
                              , ePrompt       = dfltPrompt
                              , ePrvLength    = dfltPrv
                              , eFileName     = dfltFileName }

main :: IO ()
main = execParser opts >>= f
    where f (Opts _    _ _ _ _ "") =
              sm interLoop
          f (Opts True  _ _ _ _ n) =
              sm $ loadFile n >> setFileName n >> interLoop
          f (Opts False s q b o n) =
              sm $ loadFile n >> saveMidi s q b (output o n)
