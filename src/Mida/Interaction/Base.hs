-- -*- Mode: Haskell; -*-
--
-- This module describes monad for interactive REPL and some basic
-- functions.
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS  -fno-warn-orphans          #-}

module Mida.Interaction.Base
    ( MidaIO
    , MidaInt
    , runMidaInt
    , MidaSt (..)
    , MidaCfg (..)
    , lift
    , liftEnv
    , getPrevLen
    , setPrevLen
    , getSrcFile
    , setSrcFile
    , getProg
    , setProg
    , getTempo
    , setTempo
    , getPrompt
    , getVerbose
    , getPrvCmd
    , getProgOp
    , getTempoOp
    , dfltSeed
    , dfltQuarter
    , dfltBeats
    , processDef )
where

import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Text.Format as F
import qualified System.Console.Haskeline as L

import Mida.Language

type MidaIO = MidaInt IO

newtype MidaInt m a = MidaInt
    { unMidaInt :: StateT MidaSt (ReaderT MidaCfg (MidaEnv m)) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState MidaSt
             , MonadReader MidaCfg
             , MonadIO )

instance MonadTrans MidaInt where
    lift = MidaInt . lift . lift . lift

liftEnv :: (Monad m) => MidaEnv m a -> MidaInt m a
liftEnv = MidaInt . lift . lift

deriving instance L.MonadException m => L.MonadException (MidaEnv m)
deriving instance L.MonadException m => L.MonadException (MidaInt m)

data MidaSt = MidaSt
    { stPrevLen :: Int
    , stSrcFile :: String
    , stProg    :: Int
    , stTempo   :: Int }

data MidaCfg = MidaCfg
    { cfgPrompt  :: String
    , cfgVerbose :: Bool
    , cfgPrvCmd  :: String
    , cfgProgOp  :: String
    , cfgTempoOp :: String }

runMidaInt :: Monad m => MidaInt m a -> MidaSt -> MidaCfg -> m a
runMidaInt m st cfg =
    runMidaEnv (runReaderT (evalStateT (unMidaInt m) st) cfg)

getPrevLen :: MidaIO Int
getPrevLen = gets stPrevLen

setPrevLen :: Int -> MidaIO ()
setPrevLen x = modify $ \e -> e { stPrevLen = x }

getSrcFile :: MidaIO String
getSrcFile = gets stSrcFile

setSrcFile :: String -> MidaIO ()
setSrcFile x = modify $ \e -> e { stSrcFile = x }

getProg :: MidaIO Int
getProg = gets stProg

setProg :: Int -> MidaIO ()
setProg x = modify $ \e -> e { stProg = x }

getTempo :: MidaIO Int
getTempo = gets stTempo

setTempo :: Int -> MidaIO ()
setTempo x = modify $ \e -> e { stTempo = x }

getPrompt :: MidaIO String
getPrompt = asks cfgPrompt

getVerbose :: MidaIO Bool
getVerbose = asks cfgVerbose

getPrvCmd :: MidaIO String
getPrvCmd = asks cfgPrvCmd

getProgOp :: MidaIO String
getProgOp = asks cfgProgOp

getTempoOp :: MidaIO String
getTempoOp = asks cfgTempoOp

dfltSeed :: Int
dfltSeed = 0

dfltQuarter :: Int
dfltQuarter = 24

dfltBeats :: Int
dfltBeats = 16

processDef :: String -> SyntaxTree -> MidaIO ()
processDef n t = do
  recursive <- liftEnv $ checkRecur n t
  if recursive
  then liftIO $ F.print "Rejected recursive definition for «{}».\n" (F.Only n)
  else liftEnv (addDef n t) >> liftIO (F.print "• «{}»\n" (F.Only n))
