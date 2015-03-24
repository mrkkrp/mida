-- -*- Mode: Haskell; -*-
--
-- Environment is formed via evaluation of definitions. This module
-- describes minimal MIDA environment in form of monad transformer.
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
{-# LANGUAGE TupleSections              #-}

module Mida.Language.Environment
    ( MidaEnv (..)
    , runMidaEnv
    , addDef
    , remDef
    , clearDefs
    , getPrin
    , getSrc
    , fullSrc
    , getRefs
    , purgeEnv
    , checkRecur
    , setRandGen
    , newRandGen )
where

import Control.Applicative (Applicative, (<$>))
import Control.Arrow ((***), (>>>))
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import System.Random.Mersenne.Pure64

import Mida.Language.SyntaxTree
import Mida.Representation.Base (noteAlias, figures)
import Mida.Representation.Show (showDefinition)

data MidaEnvSt = MidaEnvSt
    { stDefs    :: Defs
    , stRandGen :: PureMT }

type Defs = M.Map String SyntaxTree

newtype MidaEnv m a = MidaEnv
    { unMidaEnv :: StateT MidaEnvSt m a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState MidaEnvSt
             , MonadTrans
             , MonadIO )

runMidaEnv :: Monad m => MidaEnv m a -> m a
runMidaEnv e = evalStateT (unMidaEnv e) MidaEnvSt
               { stDefs    = defaultDefs
               , stRandGen = pureMT 0 }

defaultDefs :: Defs
defaultDefs = M.fromList $
              zip noteAlias (makeS <$> [0..]) ++
                  zip figures   (makeS <$> [128,256..])
    where makeS x = [Value x]

getDefs :: Monad m => MidaEnv m Defs
getDefs = stDefs `liftM` get

setDefs :: Monad m => Defs -> MidaEnv m ()
setDefs x = modify $ \e -> e { stDefs = x }

addDef :: Monad m => String -> SyntaxTree -> MidaEnv m ()
addDef name tree = M.insert name tree `liftM` getDefs >>= setDefs

remDef :: Monad m => String -> MidaEnv m ()
remDef name = M.delete name `liftM` getDefs >>= setDefs

clearDefs :: Monad m => MidaEnv m ()
clearDefs = setDefs defaultDefs

getPrin :: Monad m => String -> MidaEnv m SyntaxTree
getPrin name = (fromMaybe [] . M.lookup name) `liftM` getDefs

getSrc :: Monad m => String -> MidaEnv m String
getSrc name = showDefinition name `liftM` getPrin name

fullSrc :: Monad m => MidaEnv m String
fullSrc = (M.foldr (++) ""             .
           M.mapWithKey showDefinition .
           flip M.difference defaultDefs) `liftM` getDefs

getRefs :: Monad m => MidaEnv m [String]
getRefs = M.keys `liftM` getDefs

tDefs :: String -> Defs -> [String]
tDefs name defs = maybe [] cm $ M.lookup name defs
    where cm               = concatMap f
          f (Value      _) = []
          f (Section    x) = cm x
          f (Multi      x) = cm x
          f (CMulti     x) = concatMap (cm *** cm >>> uncurry (++)) x
          f (Reference  x) = x : tDefs x defs
          f (Range    _ _) = []
          f (Product  x y) = f x ++ f y
          f (Division x y) = f x ++ f y
          f (Sum      x y) = f x ++ f y
          f (Diff     x y) = f x ++ f y
          f (Loop     x y) = f x ++ f y
          f (Rotation x y) = f x ++ f y
          f (Reverse    x) = f x

purgeEnv :: Monad m => [String] -> MidaEnv m ()
purgeEnv tops = f `liftM` getDefs >>= setDefs
    where f defs = M.intersection defs $ M.unions [ts, ms defs, defaultDefs]
          ms     = M.unions . map toDefs . zipWith tDefs tops . repeat
          ts     = toDefs tops

checkRecur :: Monad m => String -> SyntaxTree -> MidaEnv m Bool
checkRecur name tree = check `liftM` getDefs
    where check = elem name . tDefs name . M.insert name tree

setRandGen :: Monad m => Int -> MidaEnv m ()
setRandGen x = modify $ \e -> e { stRandGen = pureMT (fromIntegral x) }

newRandGen :: Monad m => MidaEnv m PureMT
newRandGen = do
  (n, g) <- (randomWord64 . stRandGen) `liftM` get
  modify $ \e -> e { stRandGen = pureMT n }
  return . pureMT . fst . randomWord64 $ g

toDefs :: [String] -> Defs
toDefs = M.fromList . map (, [])
