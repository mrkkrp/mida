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
import System.IO (readFile)
import Text.Printf (printf)
import Codec.Midi (exportFile)

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

-- Main --

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

interLoop :: StateT Env IO ()
interLoop =
    do liftIO $ putStrLn "MIDA interactive environment v0.1.0"
       putPrompt
       mapM_ process . takeWhile (/= "quit") . lines =<< liftIO getContents
       where process str = liftIO (putStrLn str) >> putPrompt
             putPrompt = getPrompt >>= \x -> liftIO $ putStr x

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
                              , eFileName     = "" }

main :: IO ()
main = execParser opts >>= f
    where f (Opts _    _ _ _ _ "") =
              sm interLoop
          f (Opts True  _ _ _ _ n) =
              sm $ loadFile n >> setFileName n >> interLoop
          f (Opts False s q b o n) =
              sm $ loadFile n >> setFileName n >> saveMidi s q b (output o n)

{-
repl :: MidaM ()
repl =
    do prompt <- getPrompt
       str <- liftIO $ putStr prompt >> getLine
       when (str == "quit") (saveMidi 0 24 16 "test.midi")
       result <- case parseMida "interactive" (str ++ "\n") of
                   (Right x) -> case (x !! 0) of
                                  (Definition n e s) -> addDef n e s >> (return $ "defined " ++ n)
                                  (Exposition e) -> show . take 10 <$> eval e
                   (Left  x) -> return $ "parse error: " ++ x
       liftIO $ putStrLn $ result
--       purgeEnv ["foo"]
--       src <- source
--       liftIO $ putStrLn $ src
       repl
-}
