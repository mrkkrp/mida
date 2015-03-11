-- -*- Mode: Haskell; -*-
--
-- This module describes process of principle evaluation.
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

module Eval
    ( Principle
    , Elt
    , Element (..)
    , evalDef
    , eval
    , build )
where

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Arrow ((***), (>>>))
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Monoid (mappend, mconcat)
import qualified Data.Foldable as F

import System.Random.Mersenne.Pure64

import Environment
import Parser

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

type Principle = [Elt]
type Elt       = Element Int

data Element a
    = Value     a
    | Section   [Element a]
    | Multi     [Element a]
    | CMulti    [(Element a, Element a)]
      deriving (Show)

instance Functor Element where
    fmap f (Value   x) = Value   $ f x
    fmap f (Section x) = Section $ (f <$>) <$> x
    fmap f (Multi   x) = Multi   $ (f <$>) <$> x
    fmap f (CMulti  x) = CMulti  $ ((f <$>) *** (f <$>)) <$> x

instance Applicative Element where
    pure = Value
    (Value   f) <*> x = f <$> x
    (Section f) <*> x = Section $ (<*> x) <$> f
    (Multi   f) <*> x = Multi   $ (<*> x) <$> f
    (CMulti  f) <*> x = CMulti  $ ((<*> x) *** (<*> x)) <$> f

instance F.Foldable Element where
    foldMap f (Value   x) = f x
    foldMap f (Section x) = mconcat $ (F.foldMap f) <$> x
    foldMap f (Multi   x) = mconcat $ (F.foldMap f) <$> x
    foldMap f (CMulti  x) = mconcat $ (F.foldMap f *** F.foldMap f >>>
                                        uncurry mappend) <$> x

data CalcState = CalcState
    { clcRandGen :: PureMT
    , clcHistory :: [Int] }

newtype Calc a = Calc
    { unCalc :: State CalcState a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState CalcState)

----------------------------------------------------------------------------
--                               Evaluation                               --
----------------------------------------------------------------------------

evalDef :: Monad m => String -> MidaEnv m [Int]
evalDef name = getPrin name >>= eval

eval :: Monad m => SyntaxTree -> MidaEnv m [Int]
eval prin = do
  p <- build prin
  g <- newRandGen
  return $ runCalc (resolve $ if none p then [] else cycle p) g
    where none = null . F.foldr (:) [] . Section

resolve :: Principle -> Calc [Int]
resolve [] = return []
resolve xs = concat <$> mapM f xs
    where f (Value   x) = addHistory x >> return [x]
          f (Section x) = resolve x
          f (Multi   x) = choice x >>= maybe (return []) f
          f (CMulti  x) = listToMaybe <$> filterM (matchHistory . fst) x
                          >>= maybe (f . Multi . map snd $ x) (f . snd)

----------------------------------------------------------------------------
--                          Calc Monad Interface                          --
----------------------------------------------------------------------------

runCalc :: Calc a -> PureMT -> a
runCalc clc gen = evalState (unCalc clc)
                  CalcState { clcRandGen = gen
                            , clcHistory = [] }

choice :: [a] -> Calc (Maybe a)
choice [] = return Nothing
choice xs = do
  (n, g) <- randomInt . clcRandGen <$> get
  modify $ \c -> c { clcRandGen = g }
  return . Just $ xs !! mod (abs n) (length xs)

condMatch :: [Int] -> Elt -> Bool
condMatch []    _           = False
condMatch (h:_) (Value   x) = h == x
condMatch hs    (Section x) = and $ zipWith condMatch (tails hs) (reverse x)
condMatch hs    (Multi   x) = or  $ zipWith condMatch (repeat hs) x
condMatch hs    (CMulti  x) = condMatch hs (Multi $ map snd x)

matchHistory :: Elt -> Calc Bool
matchHistory x = do
  hs <- clcHistory <$> get
  return $ condMatch hs x

addHistory :: Int -> Calc ()
addHistory x = modify $ \c -> c { clcHistory = x : clcHistory c }

----------------------------------------------------------------------------
--                             Simplification                             --
----------------------------------------------------------------------------

build :: Monad m => SyntaxTree -> MidaEnv m Principle
build = liftM concat . mapM f
    where
      r                = return . return
      fPair (c, x)     = liftM2 (,) (head `liftM` f c) (head `liftM` f x)
      f (Value'     x) = r . Value $ x
      f (Section'  xs) = build xs >>= r . Section
      f (Multi'    xs) = build xs >>= r . Multi
      f (CMulti'   xs) = mapM fPair xs >>= r . CMulti
      f (Reference  x) = getPrin x >>= build
      f (Range    x y) = return $ Value <$> if x > y then [x,x-1..y] else [x..y]
      f (Product  x y) = liftM2 (adb (\a b -> [(*) <$> a <*> b])) (f x) (f y)
      f (Division x y) = liftM2 (adb (\a b -> [sdiv <$> a <*> b])) (f x) (f y)
      f (Sum      x y) = liftM2 (adb (\a b -> [(+) <$> a <*> b])) (f x) (f y)
      f (Diff     x y) = liftM2 (adb (\a b -> [sdif <$> a <*> b])) (f x) (f y)
      f (Loop     x y) = liftM2 (adb loop) (f x) (f y)
      f (Rotation x y) = liftM2 (adb (\a b -> [rotate a b])) (f x) (f y)
      f (Reverse    x) = liftM  (adu reverse') (f x)
      adb _ [] _       = []
      adb _ xs []      = xs
      adb g xs (y:ys)  = init xs ++ g (last xs) y ++ ys
      adu _ []         = []
      adu g (x:xs)     = g x : xs

sdiv :: Int -> Int -> Int
sdiv x 0 = x
sdiv x y = x `div` y

sdif :: Int -> Int -> Int
sdif x y
    | x < y     = 0
    | otherwise = x - y

loop :: Elt -> Elt -> Principle
loop x           (Value   y) = replicate y x
loop x           (Multi   y) = [Multi $ Section . loop x <$> y]
loop (Section x) (Section y) = [Section . concat $ zipWith loop x (cycle y)]
loop x           _           = [x]

rotate :: Elt -> Elt -> Elt
rotate (Section   x) (Value   y) = Section $ zipWith const (drop y (cycle x)) x
rotate x@(Section _) (Multi   y) = Multi $ rotate x <$> y
rotate (Section   x) (Section y) = Section $ zipWith rotate x (cycle y)
rotate x             _           = x

reverse' :: Elt -> Elt
reverse' x@(Value  _) = x
reverse' (Multi    x) = Multi   $ reverse' <$> x
reverse' (Section  x) = Section $ reverse $ reverse' <$> x
reverse' (CMulti   x) = CMulti $ (reverse' *** reverse') <$> x
