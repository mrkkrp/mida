-- -*- Mode: HASKELL; -*-

-- Main module of MIDA interpreter / compiler.

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
import Control.Applicative ((<$>))
import qualified Data.Map.Lazy as Map
import System.Random.Mersenne.Pure64

-- Testing --

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

main :: IO ()
main = void $ runStateT repl $ Env { eDefinitions  = Map.empty
                                   , eRandGen      = pureMT 0
                                   , ePrompt       = "mida> "
                                   , ePrvLength    = 16
                                   , eFileName     = "test.da" }
