-- -*- Mode: Haskell; -*-
--
-- Environment is formed via evaluation of definitions. For practical
-- reasons environment also contains a number of useful variables. This
-- module provides interface for MIDA environment.
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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Environment
    ( MidaState  (..)
    , MidaConfig (..)
    , MidaEnv    (..)
    , runMidaEnv
    , clearDefs
    , setRandGen
    , newRandGen
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
    , addDef
    , remDef
    , getPrin
    , getSrc
    , fullSrc
    , getRefs
    , purgeEnv
    , checkRecur )
where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M

import System.Random.Mersenne.Pure64

import Parser

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

data MidaState = MidaState
    { stDefs    :: Defs
    , stRandGen :: PureMT
    , stPrevLen :: Int
    , stSrcFile :: String
    , stProg    :: Int
    , stTempo   :: Int }

type Defs = M.Map String (SyntaxTree, String)

data MidaConfig = MidaConfig
    { cfgPrompt  :: String
    , cfgVerbose :: Bool
    , cfgPrvCmd  :: String
    , cfgProgOp  :: String
    , cfgTempoOp :: String }

newtype MidaEnv m a = MidaEnv
    { unMidaEnv :: StateT MidaState (ReaderT MidaConfig m) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState MidaState
             , MonadReader MidaConfig
             , MonadIO )

instance MonadTrans MidaEnv where
    lift = MidaEnv . lift . lift

----------------------------------------------------------------------------
--                              Environment                               --
----------------------------------------------------------------------------

runMidaEnv :: Monad m => MidaEnv m a -> MidaState -> MidaConfig -> m a
runMidaEnv e st cfg = runReaderT (fst `liftM` runStateT (unMidaEnv e) st) cfg

getDefs :: Monad m => MidaEnv m Defs
getDefs = stDefs `liftM` get

setDefs :: Monad m => Defs -> MidaEnv m ()
setDefs x = modify $ \e -> e { stDefs = x }

clearDefs :: Monad m => MidaEnv m ()
clearDefs = setDefs M.empty

setRandGen :: Monad m => Int -> MidaEnv m ()
setRandGen x = modify $ \e -> e { stRandGen = pureMT (fromIntegral x) }

newRandGen :: Monad m => MidaEnv m PureMT
newRandGen = do
  (n, g) <- (randomWord64 . stRandGen) `liftM` get
  modify $ \e -> e { stRandGen = pureMT n }
  return . pureMT . fst . randomWord64 $ g

getPrevLen :: Monad m => MidaEnv m Int
getPrevLen = stPrevLen `liftM` get

setPrevLen :: Monad m => Int -> MidaEnv m ()
setPrevLen x = modify $ \e -> e { stPrevLen = x }

getSrcFile :: Monad m => MidaEnv m String
getSrcFile = stSrcFile `liftM` get

setSrcFile :: Monad m => String -> MidaEnv m ()
setSrcFile x = modify $ \e -> e { stSrcFile = x }

getProg :: Monad m => MidaEnv m Int
getProg = stProg `liftM` get

setProg :: Monad m => Int -> MidaEnv m ()
setProg x = modify $ \e -> e { stProg = x }

getTempo :: Monad m => MidaEnv m Int
getTempo = stTempo `liftM` get

setTempo :: Monad m => Int -> MidaEnv m ()
setTempo x = modify $ \e -> e { stTempo = x }

getPrompt :: Monad m => MidaEnv m String
getPrompt = cfgPrompt `liftM` ask

getVerbose :: Monad m => MidaEnv m Bool
getVerbose = cfgVerbose `liftM` ask

getPrvCmd :: Monad m => MidaEnv m String
getPrvCmd = cfgPrvCmd `liftM` ask

getProgOp :: Monad m => MidaEnv m String
getProgOp = cfgProgOp `liftM` ask

getTempoOp :: Monad m => MidaEnv m String
getTempoOp = cfgTempoOp `liftM` ask

addDef :: Monad m => String -> SyntaxTree -> String -> MidaEnv m ()
addDef name img src = M.insert name (img, src) `liftM` getDefs >>= setDefs

remDef :: Monad m => String -> MidaEnv m ()
remDef name = M.delete name `liftM` getDefs >>= setDefs

getPrin :: Monad m => String -> MidaEnv m SyntaxTree
getPrin name = (maybe [] fst . M.lookup name) `liftM` getDefs

getSrc :: Monad m => String -> MidaEnv m String
getSrc name = (maybe "no definition\n" snd . M.lookup name) `liftM` getDefs

fullSrc :: Monad m => MidaEnv m String
fullSrc = (concat . filter (not . null) . map snd . M.elems) `liftM` getDefs

getRefs :: Monad m => MidaEnv m [String]
getRefs = getDefs >>= return . M.keys

tDefs :: String -> Defs -> [String]
tDefs name defs = maybe [] (cm . fst) $ M.lookup name defs
    where cm                = concatMap f
          f (Value'    _  ) = []
          f (Section'  xs ) = cm xs
          f (Multi'    xs ) = cm xs
          f (CMulti'   xs ) = concatMap ((++) <$> f . fst <*> f . snd) xs
          f (Reference x  ) = x : tDefs x defs
          f (Range     _ _) = []
          f (Product   x y) = f x ++ f y
          f (Sum       x y) = f x ++ f y
          f (Loop      x y) = f x ++ f y
          f (Rotation  x y) = f x ++ f y
          f (Reverse   x  ) = f x

purgeEnv :: Monad m => [String] -> MidaEnv m ()
purgeEnv tops = f `liftM` getDefs >>= setDefs
    where f defs = foldr M.delete defs $ M.keys defs \\ (musts defs ++ tops)
          musts  = nub . concat . zipWith tDefs tops . repeat

checkRecur :: Monad m => String -> SyntaxTree -> MidaEnv m Bool
checkRecur name img = check `liftM` getDefs
    where check = elem name . tDefs name . M.insert name (img, "")
