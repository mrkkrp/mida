-- -*- Mode: Haskell; -*-
--
-- This module describes process of evaluation of definitions and arbitrary
-- principles. Result of evaluation is infinite list of integers or empty
-- list.
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

module Mida.Language.Eval
    ( evalDef
    , eval
    , toPrin )
where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Arrow ((***))
import Control.Monad.State.Lazy
import Data.List (tails)
import Data.Maybe (listToMaybe)
import qualified Data.Foldable as F

import System.Random.Mersenne.Pure64

import Mida.Language.SyntaxTree
import Mida.Language.Element
import Mida.Language.Environment

data CalcSt = CalcSt
    { clcHistory :: [Int]
    , clcRandGen :: PureMT }

newtype Calc a = Calc
    { unCalc :: State CalcSt a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState CalcSt )

evalDef :: Monad m => String -> MidaEnv m [Int]
evalDef name = getPrin name >>= eval

eval :: Monad m => SyntaxTree -> MidaEnv m [Int]
eval tree = do
  p <- toPrin tree
  g <- newRandGen
  return $ runCalc (resolve $ if none p then [] else cycle p) g
      where none = null . F.foldr (:) [] . Sec

resolve :: Principle -> Calc [Int]
resolve [] = return []
resolve xs = concat <$> mapM f xs
    where f (Val  x) = addHistory x >> return [x]
          f (Sec  x) = resolve x
          f (Mul  x) = choice x >>= maybe (return []) f
          f (CMul x) = listToMaybe <$> filterM (matchHistory . fst) x
                       >>= maybe (f . Mul . concatMap snd $ x) (resolve . snd)

runCalc :: Calc a -> PureMT -> a
runCalc clc gen = evalState (unCalc clc)
                  CalcSt { clcHistory = []
                         , clcRandGen = gen }

choice :: [a] -> Calc (Maybe a)
choice [] = return Nothing
choice xs = do
  (n, g) <- randomInt . clcRandGen <$> get
  modify $ \c -> c { clcRandGen = g }
  return . Just $ xs !! mod (abs n) (length xs)

condMatch :: [Int] -> Elt -> Bool
condMatch []    _        = False
condMatch (h:_) (Val  x) = h == x
condMatch hs    (Sec  x) = and $ zipWith condMatch (tails hs) (reverse x)
condMatch hs    (Mul  x) = or  $ condMatch hs <$> x
condMatch hs    (CMul x) = condMatch hs (Mul $ concatMap snd x)

matchHistory :: [Elt] -> Calc Bool
matchHistory x = do
  hs <- clcHistory <$> get
  return . or $ condMatch hs <$> x

addHistory :: Int -> Calc ()
addHistory x = modify $ \c -> c { clcHistory = x : clcHistory c }

toPrin :: Monad m => SyntaxTree -> MidaEnv m Principle
toPrin = liftM concat . mapM f
    where
      r                = return . return
      fPair (c, x)     = liftM2 (,) (toPrin c) (toPrin x)
      f (Value      x) = r . Val $ x
      f (Section   xs) = toPrin xs >>= r . Sec
      f (Multi     xs) = toPrin xs >>= r . Mul
      f (CMulti    xs) = mapM fPair xs >>= r . CMul
      f (Reference  x) = getPrin x >>= toPrin
      f (Range    x y) = return $ Val <$> if x > y then [x,x-1..y] else [x..y]
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
loop x       (Val y) = replicate y x
loop x       (Mul y) = [Mul $ Sec . loop x <$> y]
loop (Sec x) (Sec y) = [Sec . concat $ zipWith loop x (cycle y)]
loop x       _       = [x]

rotate :: Elt -> Elt -> Elt
rotate (Sec   x) (Val y) = Sec $ zipWith const (drop y (cycle x)) x
rotate x@(Sec _) (Mul y) = Mul $ rotate x <$> y
rotate (Sec   x) (Sec y) = Sec $ zipWith rotate x (cycle y)
rotate x         _       = x

reverse' :: Elt -> Elt
reverse' x@(Val _) = x
reverse' (Mul   x) = Mul  $ reverse' <$> x
reverse' (Sec   x) = Sec  $ reverse $ reverse' <$> x
reverse' (CMul  x) = CMul $ ((reverse' <$>) *** (reverse' <$>)) <$> x
