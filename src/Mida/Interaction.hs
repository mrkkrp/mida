-- -*- Mode: Haskell; -*-
--
-- This module describes how MIDA processes commands in interactive
-- mode. These commands are also used in batch mode.
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

import Control.Monad.Reader
import Prelude hiding (mapM_)
import System.IO
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import qualified Data.Text.Format as F
import qualified System.Console.Haskeline as L

import Mida.Interaction.Base
import Mida.Interaction.Commands
import Mida.Language
import Mida.Representation

interaction :: String -> MidaIO ()
interaction version = do
  liftIO $ hSetBuffering stdin LineBuffering
  liftIO $ F.print "MIDA Interactive Environment {}\n" (F.Only version)
  L.runInputT (L.setComplete completionFunc L.defaultSettings) midaRepl

midaRepl :: L.InputT MidaIO ()
midaRepl = do
  input <- getMultiline ""
  case input of
    Just x  -> do if T.pack cmdPrefix `T.isPrefixOf` T.strip x
                  then lift $ processCmd x
                  else lift $ processExpr x
                  midaRepl
    Nothing -> return ()

getMultiline :: T.Text -> L.InputT MidaIO (Maybe T.Text)
getMultiline prv = do
  prompt <- lift getPrompt
  input  <- L.getInputLine $
            if T.null prv then prompt else replicate (length prompt) ' '
  case input of
    Just x -> let r = prv `T.append` T.pack x `T.append` "\n"
              in if probeMida r
                 then return (Just r)
                 else getMultiline r
    Nothing -> return Nothing

processExpr :: T.Text -> MidaIO ()
processExpr expr = do
  file <- getSrcFile
  case parseMida file expr of
    Right x -> mapM_ f x
    Left  x -> liftIO $ F.print "Parse error in {}.\n" (F.Only x)
    where f (Definition n t) = processDef n t
          f (Exposition   t) =
              do len     <- getPrevLen
                 verbose <- getVerbose
                 result  <- liftEnv $ eval t
                 prin    <- liftEnv $ toPrin t
                 liftIO $ when verbose
                            (F.print "≡ {}" (F.Only $ showPrinciple prin))
                 spitList $ take len result

spitList :: [Int] -> MidaIO ()
spitList [] = liftIO $ T.putStrLn "⇒ ⊥"
spitList xs = liftIO $ F.print "⇒ {}…\n" (F.Only $ unwords (show <$> xs))
