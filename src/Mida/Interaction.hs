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

module Mida.Interaction
    ( MidaIO
    , MidaInt
    , runMidaInt
    , MidaSt (..)
    , MidaCfg (..)
    , cmdLoad
    , cmdMake
    , interaction
    , dfltSeed
    , dfltQuarter
    , dfltBeats )
where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.List
import System.IO
import Text.Printf (printf)

import qualified System.Console.Haskeline as L

import Mida.Interaction.Base
import Mida.Interaction.Commands
import Mida.Language
import Mida.Representation

interaction :: String -> MidaIO ()
interaction version = do
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
    Just x -> if probeMida x
              then return (Just x)
              else getMultiline x
    Nothing -> return Nothing

processExpr :: String -> MidaInt IO ()
processExpr expr = do
  file <- getSrcFile
  case parseMida file expr of
    Right x -> mapM_ f x
    Left  x -> liftIO $ printf "Parse error in %s.\n" x
    where f (Definition n e s) = processDef n e s
          f (Exposition e) =
              do len     <- getPrevLen
                 verbose <- getVerbose
                 result  <- liftEnv $ eval e
                 prin    <- liftEnv $ toPrin e
                 liftIO $ when verbose (putStrLn $ "= " ++ showPrinciple prin)
                 spitList $ take len result

spitList :: Show a => [a] -> MidaIO ()
spitList [] = liftIO $ printf "none\n"
spitList xs = liftIO $ printf "%s...\n" $ intercalate " " (show <$> xs)
