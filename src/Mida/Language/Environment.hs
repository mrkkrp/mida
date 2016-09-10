--
-- Environment is formed via evaluation of definitions. This module
-- describes minimal MIDA environment in form of monad transformer.
--
-- Copyright © 2014–2016 Mark Karpov
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

module Mida.Language.Environment
  ( MidaEnv
  , HasEnv (..)
  , runMidaEnv
  , addDef
  , remDef
  , clearDefs
  , getPrin
  , getSrc
  , fullSrc
  , getRefs
  , purgeEnv
  , checkRecur )
where

import Control.Applicative (empty)
import Control.Arrow ((***), (>>>))
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text)
import Mida.Language.SyntaxTree
import Mida.Representation.Base (noteAlias, modifiers)
import Mida.Representation.Show (showDefinition)
import Numeric.Natural
import System.Console.Haskeline.MonadException
import System.Random (split)
import System.Random.TF (TFGen, mkTFGen)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict    as M

-- | MIDA environment state. Basically this amounts to collection of
-- definitions and random number generator.

data MidaEnvSt = MidaEnvSt
  { meDefs    :: Defs  -- ^ Collection of definitions
  , meRandGen :: TFGen -- ^ Random generator
  } deriving Show

-- | Type synonym for collection of definitions, where a definition is a
-- pair of variable name and corresponding AST.

type Defs = Map String SyntaxTree

-- | Monad that implements MIDA environment.

newtype MidaEnv m a = MidaEnv
  { unMidaEnv :: StateT MidaEnvSt m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState MidaEnvSt
           , MonadException
           , MonadThrow
           , MonadCatch
           , MonadMask )

-- | Type class for things that can be considered MIDA environment.

class Monad m => HasEnv m where
  -- | Get collection of all definitions.
  getDefs :: m Defs
  -- | Update definitions with given ones.
  setDefs :: Defs -> m ()
  -- | Set random generator seed.
  setRandGen :: Natural -> m ()
  -- | Split current random generator, update it, and return new one.
  newRandGen :: m TFGen

instance Monad m => HasEnv (MidaEnv m) where
  getDefs = gets meDefs
  setDefs defs = modify $ \env -> env { meDefs = defs }
  setRandGen gen = modify $ \e -> e { meRandGen = mkTFGen (fromIntegral gen) }
  newRandGen = do
    (g, g') <- split <$> gets meRandGen
    modify $ \e -> e { meRandGen = g' }
    return g

instance HasEnv m => HasEnv (StateT e m) where
  getDefs    = lift getDefs
  setDefs    = lift . setDefs
  setRandGen = lift . setRandGen
  newRandGen = lift newRandGen

instance HasEnv m => HasEnv (ReaderT e m) where
  getDefs    = lift getDefs
  setDefs    = lift . setDefs
  setRandGen = lift . setRandGen
  newRandGen = lift newRandGen

-- | Run state monad with MIDA environment.

runMidaEnv :: Monad m => MidaEnv m a -> m a
runMidaEnv m = evalStateT (unMidaEnv m) MidaEnvSt
  { meDefs    = defaultDefs
  , meRandGen = mkTFGen 0 }

-- | Default definitions in MIDA environment.

defaultDefs :: Defs
defaultDefs = M.fromList $
  zip noteAlias (f <$> [0..]) <>
  zip modifiers (f <$> [128,256..])
  where f = pure . Value

-- | Add a new definition to the environment.

addDef :: HasEnv m
  => String            -- ^ Reference name
  -> SyntaxTree        -- ^ AST of its principle
  -> m ()
addDef name tree = getDefs >>= setDefs . M.insert name tree

-- | Remove definition given its name.

remDef :: HasEnv m
  => String            -- ^ Reference name
  -> m ()
remDef name = getDefs >>= setDefs . M.delete name

-- | Remove all definitions, restoring default state of environment.

clearDefs :: HasEnv m => m ()
clearDefs = setDefs defaultDefs

-- | Get principle corresponding to given variable name.

getPrin :: HasEnv m
  => String            -- ^ Reference name
  -> m SyntaxTree      -- ^ Syntax tree
getPrin name = (fromMaybe [] . M.lookup name) <$> getDefs

-- | Get source code of definition given its name.

getSrc :: HasEnv m
  => String            -- ^ Reference name
  -> m Text            -- ^ Textual representation of source code
getSrc name = showDefinition name <$> getPrin name

-- | Reconstruct source code for all existing definitions.

fullSrc :: HasEnv m => m Text
fullSrc = (M.foldMapWithKey showDefinition . (M.\\ defaultDefs)) <$> getDefs

-- | Get all reference names defined at the moment.

getRefs :: HasEnv m => m [String]
getRefs = M.keys <$> getDefs

-- | This performs “definition traversal” and returns collection of
-- definition names that given reference name depends on.

tDefs
  :: String            -- ^ Reference name
  -> Defs              -- ^ Definitions
  -> [String]          -- ^ Collection of definition names
tDefs name defs = maybe empty cm (name `M.lookup` defs)
  where cm               = (>>= f)
        f (Value      _) = mempty
        f (Section    x) = cm x
        f (Multi      x) = cm x
        f (CMulti     x) = NE.toList x >>= (cm *** cm >>> uncurry (<>))
        f (Reference  x) = return x <> tDefs x defs
        f (Range    _ _) = mempty
        f (Product  x y) = f x <> f y
        f (Division x y) = f x <> f y
        f (Sum      x y) = f x <> f y
        f (Diff     x y) = f x <> f y
        f (Loop     x y) = f x <> f y
        f (Rotation x y) = f x <> f y
        f (Reverse    x) = f x

-- | Purge environment removing definitions that are not used in
-- construction of “top-level” definitions.

purgeEnv :: HasEnv m
  => [String]          -- ^ Top-level definitions
  -> m ()
purgeEnv tops = getDefs >>= setDefs . f
  where f defs = M.intersection defs (M.unions [ts, ms defs, defaultDefs])
        ms     = M.unions . fmap toDefs . zipWith tDefs tops . repeat
        ts     = toDefs tops

-- | Check if definition with given name is depends on itself.

checkRecur :: HasEnv m
  => String            -- ^ Reference name
  -> SyntaxTree        -- ^ Its syntax tree
  -> m Bool
checkRecur name tree = check <$> getDefs
  where check = elem name . tDefs name . M.insert name tree

-- | Turn collection of definition names into collection of empty
-- definitions.

toDefs :: [String] -> Defs
toDefs = M.fromList . fmap (, [])
