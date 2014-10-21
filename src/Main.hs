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
import Control.Monad.State
import qualified Data.Map.Lazy as M
import System.Random.Mersenne.Pure64
import Options.Applicative
import System.FilePath (takeFileName, replaceExtension)
import System.IO
import Text.Printf (printf)
import Codec.Midi (exportFile)
import Data.Char (isSpace, isDigit)
import Data.List
import Control.Exception

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
              <> value   0
              <> help    "Set seed for MIDI generation, default is 0" )
             <*> option  auto
               ( long    "quarter"
              <> short   'q'
              <> metavar "TICKS"
              <> value   24
              <> help    "Set ticks per quarter note, default is 24" )
             <*> option  auto
               ( long    "bars"
              <> short   'b'
              <> metavar "BARS"
              <> value   16
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

chop :: String -> [String]
chop = map (++ "\n") . lines

cmdChar = ':'

aCmd :: String -> Bool
aCmd = (== cmdChar) . head . trim

isCmd :: String -> String -> Bool
isCmd x = (== cmdChar : x) . head . words . trim

commands = [ ("help",   cmdHelp,   "display this help text")
           , ("save",   cmdSave,   "save current enviroment in specified file")
           , ("purge",  cmdPurge,  "remove redundant definitions")
           , ("make",   cmdMake,   "generate MIDI file in current environment")
           , ("def",    cmdDef,    "print definition of given symbol")
           , ("prompt", cmdPrompt, "set MIDA prompt")
           , ("length", cmdLength, "set length of result of evaluation") ]

printExc :: SomeException -> IO ()
printExc e = hPutStr stderr $ printf "-> %s;\n" (show e)

cmdHelp :: String -> StateT Env IO ()
cmdHelp _ = liftIO (putStrLn "Available commands:") >> mapM_ f commands
    where f (cmd, _, text) = liftIO $ printf "  %c%s\t\t%s\n" cmdChar cmd text

cmdSave :: String -> StateT Env IO ()
cmdSave given =
    do actual <- getFileName
       let file = if null given then actual else given
       source <- getSource
       liftIO $ catch (writeFile file source >>
                       putStrLn (printf "-> environment saved as \"%s\"." file))
                      printExc

cmdPurge :: String -> StateT Env IO ()
cmdPurge _ =
    do purgeEnv topDefs
       liftIO $ putStrLn "-> environment purged;"

safeParseInt :: String -> Int -> Int
safeParseInt str x
    | all isDigit str = read str :: Int
    | otherwise       = x

cmdMake :: String -> StateT Env IO ()
cmdMake str =
    do file <- getFileName
       saveMidi (safeParseInt s 0)
                (safeParseInt q 24)
                (safeParseInt b 16)
                (output f file)
    where (s:q:b:f:_) = (words str) ++ repeat ""

cmdDef :: String -> StateT Env IO ()
cmdDef name =
    do def <- getSrc name
       liftIO $ putStr $ printf "=> %s" def

cmdPrompt :: String -> StateT Env IO ()
cmdPrompt x = setPrompt (x ++ "> ")

cmdLength :: String -> StateT Env IO ()
cmdLength x =
    do old <- getPrvLength
       setPrvLength $ safeParseInt x old

processCmd :: String -> StateT Env IO ()
processCmd input =
    case find f commands of
      (Just (_, x, _)) -> x args
      Nothing  -> liftIO $ putStrLn $ printf
                  "-> unknown command, try %chelp;" cmdChar
    where f (x, _, _) = x == cmd
          (cmd' : args') = words input
          cmd = filter (/= cmdChar) cmd'
          args = unwords args'

prettyList :: [Int] -> String
prettyList [] = "=> none"
prettyList xs = (printf "=> %s...") $ intercalate " " (map show xs) 

processExpr :: String -> StateT Env IO ()
processExpr expr =
    do file    <- getFileName
       case parseMida file expr of
         (Right x) -> mapM_ f x
         (Left  x) -> liftIO $ putStrLn ("parse error in " ++ x)
       where f (Definition n e s) =
                 do addDef n e s
                    liftIO $ putStrLn (printf "-> defined '%s'" n)
             f (Exposition e) =
                 do preview <- getPrvLength
                    result  <- eval e
                    liftIO $ putStrLn $ (prettyList . take preview) result

interLoop :: StateT Env IO ()
interLoop =
    do liftIO $ putStrLn "-> Loading MIDA Interactive Environment v0.1.0"
       putPrompt
       liftIO getContents >>= mapM_ prc . takeWhile (not . isCmd "quit") . chop
       where prc str =
                 do if aCmd str
                    then processCmd  str
                    else processExpr str
                    putPrompt
             putPrompt =
                 do prompt <- getPrompt
                    liftIO $ putStr prompt
                    liftIO $ hFlush stdout

-- Top Level Logic --

loadFile :: String -> StateT Env IO ()
loadFile file =
    do contents <- liftIO $ readFile file
       case parseMida (takeFileName file) contents of
         (Right x) -> mapM_ f x
         (Left  x) -> error $ "parse error in " ++ x
       liftIO $ putStrLn $ printf "-> \"%s\" loaded successfully;" file
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
       liftIO $ putStrLn $ printf "-> MIDI file saved as \"%s\"." file

sm :: StateT Env IO () -> IO ()
sm x = void $ runStateT x Env { eDefinitions  = M.empty
                              , eRandGen      = pureMT 0
                              , ePrompt       = "mida> "
                              , ePrvLength    = 16
                              , eFileName     = "interactive" }

main :: IO ()
main = execParser opts >>= f
    where f (Opts _    _ _ _ _ "") =
              sm interLoop
          f (Opts True  _ _ _ _ n) =
              sm $ loadFile n >> setFileName n >> interLoop
          f (Opts False s q b o n) =
              sm $ loadFile n >> saveMidi s q b (output o n)
