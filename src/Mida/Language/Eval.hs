-- -*- Mode: Haskell; -*-
--
-- This module describes process of evaluation of definitions and arbitrary
-- principles. Result of evaluation is infinite list of integers or empty
-- list.
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

module Mida.Language.Eval
  ( evalDef
  , eval
  , toPrin )
where

import Control.Arrow ((***))
import Control.Monad.State.Lazy
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import System.Random (next)

import System.Random.TF (TFGen)

import Mida.Language.SyntaxTree
import Mida.Language.Element
import Mida.Language.Environment

data CalcSt = CalcSt
  { clcHistory :: [Int]
  , clcRandGen :: TFGen }

newtype Calc a = Calc
  { unCalc :: State CalcSt a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState CalcSt )

evalDef :: Monad m => String -> MidaEnv m [Int]
evalDef name = getPrin name >>= eval

eval :: Monad m => SyntaxTree -> MidaEnv m [Int]
eval tree = liftM2 runCalc (resolve . cycle' <$> toPrin tree) newRandGen
  where cycle' p = if null $ foldMap (:[]) (Sec p) then [] else cycle p

resolve :: Principle -> Calc [Int]
resolve [] = return []
resolve xs = concat <$> mapM f xs
  where f (Val  x) = addHistory x >> return [x]
        f (Sec  x) = resolve x
        f (Mul  x) = choice x >>= maybe (return []) f
        f (CMul x) = listToMaybe <$> filterM (matchHistory . fst) x >>=
                     maybe (f . toMul $ x) (f . Mul . snd)

runCalc :: Calc a -> TFGen -> a
runCalc clc gen = evalState (unCalc clc)
                  CalcSt { clcHistory = mempty
                         , clcRandGen = gen }

choice :: [a] -> Calc (Maybe a)
choice [] = return Nothing
choice xs = do
  (n, g) <- next <$> gets clcRandGen
  modify $ \c -> c { clcRandGen = g }
  return . Just $ xs !! (abs n `rem` length xs)

condMatch :: [Int] -> Elt -> Bool
condMatch []    _        = False
condMatch (h:_) (Val  x) = h == x
condMatch hs    (Sec  x) = and $ zipWith condMatch (tails hs) (reverse x)
condMatch hs    (Mul  x) = or  $ condMatch hs <$> x
condMatch hs    (CMul x) = condMatch hs (toMul x)

toMul :: [([Elt], [Elt])] -> Elt
toMul xs = Mul (xs >>= snd)

matchHistory :: [Elt] -> Calc Bool
matchHistory x = do
  hs <- gets clcHistory
  return . or $ condMatch hs <$> x

addHistory :: Int -> Calc ()
addHistory x = modify $ \c -> c { clcHistory = return x <> clcHistory c }

toPrin :: Monad m => SyntaxTree -> MidaEnv m Principle
toPrin = fmap simplifySec . toPrin'

simplifySec :: Principle -> Principle
simplifySec = (>>= f)
  where f (Sec xs) = simplifySec xs
        f x        = simplifyElt x

simplify :: Principle -> Principle
simplify = (>>= simplifyElt)

simplifyElt :: Elt -> Principle
simplifyElt x@(Val _)        = [x]
simplifyElt (Sec  [x])       = simplify [x]
simplifyElt (Mul  [x])       = simplify [x]
simplifyElt (CMul [(_, xs)]) = simplifyElt (Mul xs)
simplifyElt (Sec  xs)        = [Sec (simplifySec xs)]
simplifyElt (Mul  xs)        = [Mul (simplify xs)]
simplifyElt (CMul xs)        = [CMul ((simplify *** simplify) <$> xs)]

toPrin' :: Monad m => SyntaxTree -> MidaEnv m Principle
toPrin' = fmap concat . mapM f
  where
    fPair (c, x)     = (,) <$> toPrin' c <*> toPrin' x
    f (Value      x) = return . Val <$> return x
    f (Section   xs) = return . Sec <$> toPrin' xs
    f (Multi     xs) = return . Mul <$> toPrin' xs
    f (CMulti    xs) = return . CMul <$> mapM fPair xs
    f (Reference  x) = getPrin x >>= toPrin'
    f (Range    x y) = return $ Val <$> if x > y then [x,x-1..y] else [x..y]
    f (Product  x y) = adb (\a b -> [(*) <$> a <*> b]) <$> f x <*> f y
    f (Division x y) = adb (\a b -> [sdiv <$> a <*> b]) <$> f x <*> f y
    f (Sum      x y) = adb (\a b -> [(+) <$> a <*> b]) <$> f x <*> f y
    f (Diff     x y) = adb (\a b -> [sdif <$> a <*> b]) <$> f x <*> f y
    f (Loop     x y) = adb loop <$> f x <*> f y
    f (Rotation x y) = adb (\a b -> [rotate a b]) <$> f x <*> f y
    f (Reverse    x) = adu reverse' <$> f x
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
loop (Mul x) (Sec y) = [Mul . concat $ zipWith loop x (cycle y)]
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
