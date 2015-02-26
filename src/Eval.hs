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
    ( evalDef
    , eval )
where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe

import System.Random.Mersenne.Pure64

import Environment
import Parser

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

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

eval :: Monad m => Principle -> MidaEnv m [Int]
eval prin =
    do p <- simplify prin
       g <- newRandGen
       return $ runCalc (resolve $ f p) g
    where f [] = []
          f x  = cycle x

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

condMatch :: [Int] -> Element -> Bool
condMatch []    _           = False
condMatch (h:_) (Value   x) = h == x
condMatch hs    (Section x) = and $ zipWith condMatch (tails hs) (reverse x)
condMatch hs    (Multi   x) = or  $ zipWith condMatch (repeat hs) x
condMatch hs    (CMulti  x) = condMatch hs (Multi $ map snd x)

matchHistory :: [Element] -> Calc Bool
matchHistory e = do
  hs <- clcHistory <$> get
  return $ any (condMatch hs) e

addHistory :: Int -> Calc ()
addHistory x = modify $ \c -> c { clcHistory = x : clcHistory c }

----------------------------------------------------------------------------
--                             Simplification                             --
----------------------------------------------------------------------------

simplify :: Monad m => Principle -> MidaEnv m Principle
simplify = liftM concat . mapM f
    where
      r                = return . return
      fPair (c, x)     = liftM2 (,) (simplify c) (head `liftM` f x)
      f x@(Value    _) = r x
      f (Reference  x) = getPrin x >>= simplify
      f (Section   xs) = simplify xs >>= r . Section
      f (Range    x y) = return $ Value <$> if x > y then [x,x-1..y] else [x..y]
      f (Multi     xs) = simplify xs >>= r . Multi
      f (CMulti    xs) = mapM fPair xs >>= r . CMulti
      f (Product  x y) = liftM2 (adb $ liftP (*)) (f x) (f y)
      f (Sum      x y) = liftM2 (adb $ liftP (+)) (f x) (f y)
      f (Loop     x y) = liftM2 (adb   loop     ) (f x) (f y)
      f (Rotation x y) = liftM2 (adb   rotate'  ) (f x) (f y)
      f (Reverse    x) = liftM  (adu   reverse' ) (f x)
      adb _ [] []      = []
      adb _ xs []      = xs
      adb _ [] ys      = ys
      adb g xs (y:ys)  = init xs ++ g (last xs) y ++ ys
      adu _ []         = []
      adu g (x:xs)     = g x : xs

liftP :: (Int -> Int -> Int) -> Element -> Element -> Principle
liftP f x y = [liftE f x y]

liftE :: (Int -> Int -> Int) -> Element -> Element -> Element
liftE f  (Value   x) (Value   y) = Value   $ f x y
liftE f  (Multi   x) y           = Multi   $ map (flip (liftE f) y) x
liftE f  x           (Multi   y) = Multi   $ map (liftE f x) y
liftE f x@(CMulti _) y           = mapCond (flip (liftE f) y) x
liftE f  x          y@(CMulti _) = mapCond (liftE f x) y
liftE f  (Section x) (Section y) = Section $ zipWith (liftE f) x (cycle y)
liftE f  (Section x) y           = Section $ map (flip (liftE f) y) x
liftE f  x           (Section y) = Section $ map (liftE f x) y

loop :: Element -> Element -> Principle
loop x           (Value   y) = replicate y x
loop x           (Multi   y) = [Multi   $ map (Section . loop x) y]
loop (Section x) (Section y) = [Section $ concat $ zipWith loop x (cycle y)]
loop x           (Section y) = [Section $ concat $ map (loop x) y]
loop x         y@(CMulti  _) = [mapCond (Section . loop x) y]

rotate' :: Element -> Element -> Principle
rotate' x y = [rotate x y]

rotate :: Element -> Element -> Element
rotate  (Section  x)  (Value   y) = Section $ zipWith const (drop y (cycle x)) x
rotate x@(Section _)  (Multi   y) = Multi   $ map (rotate x) y
rotate  (Section  x)  (Section y) = Section $ zipWith rotate x (cycle y)
rotate x@(Section _) y@(CMulti _) = mapCond (rotate x) y
rotate  x             _           = x

reverse' :: Element -> Element
reverse' x@(Value _)  = x
reverse' (Multi x)    = Multi   $ map reverse' x
reverse' (Section x)  = Section $ reverse $ map reverse' x
reverse' x@(CMulti _) = mapCond reverse' x

mapCond :: (Element -> Element) -> Element -> Element
mapCond f (CMulti xs) = CMulti $ map ((,) <$> (map f . fst) <*> (f . snd)) xs
