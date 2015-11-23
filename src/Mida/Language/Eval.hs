-- -*- Mode: Haskell; -*-
--
-- This module describes process of evaluation of definitions and arbitrary
-- principles. Result of evaluation is infinite list of natural numbers or
-- empty list.
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

import Control.Applicative (empty)
import Control.Arrow ((***))
import Control.Monad.State.Class
import Control.Monad.State.Lazy
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Mida.Language.Element
import Mida.Language.Environment
import Mida.Language.SyntaxTree
import Numeric.Natural
import System.Random (next)
import System.Random.TF (TFGen)

-- | State record used for calculation\/evaluation of principles.

data CalcSt = CalcSt
  { clHistory :: [Natural] -- ^ Recently evaluated values
  , clRandGen :: TFGen     -- ^ Local random generator
  } deriving Show

-- | Evaluate definition given its name.

evalDef :: HasEnv m
  => String            -- ^ Reference name
  -> m [Natural]       -- ^ Infinite stream of naturals or empty list
evalDef name = getPrin name >>= eval

-- | Evaluate given syntax tree.

eval :: HasEnv m
  => SyntaxTree        -- ^ Syntax tree
  -> m [Natural]       -- ^ Infinite stream of naturals or empty list
eval tree = liftM2 runCalc (resolve . cycle' <$> toPrin tree) newRandGen
  where cycle' p = if null $ foldMap (:[]) (Sec p) then [] else cycle p

-- | Resolve principle into stream of naturals.

resolve :: MonadState CalcSt m
  => Principle         -- ^ Principle in question
  -> m [Natural]       -- ^ Stream of naturals
resolve [] = return []
resolve xs = concat <$> mapM f xs
  where f (Val  x) = addHistory x >> return [x]
        f (Sec  x) = resolve x
        f (Mul  x) = choice x >>= maybe (return []) f
        f (CMul x) = listToMaybe <$> filterM (matchHistory . fst) x >>=
          maybe (f . toMul $ x) (f . Mul . snd)

-- | Run lazy state monad with 'CalcSt' state.

runCalc
  :: State CalcSt a    -- ^ Monad to run
  -> TFGen             -- ^ Initial random generator
  -> a                 -- ^ Result
runCalc m gen = evalState m CalcSt
  { clHistory = empty
  , clRandGen = gen }

-- | Random choice between given options.

choice :: MonadState CalcSt m
  => [a]               -- ^ Options to choose from
  -> m (Maybe a)       -- ^ Result
choice [] = return Nothing
choice xs = do
  (n, g) <- next <$> gets clRandGen
  modify $ \c -> c { clRandGen = g }
  return . Just $ xs !! (abs n `rem` length xs)

-- | Check if given elements “matches” history of generated values. This
-- is for conditional multivalues, see manual for more information.
--
-- Note: head of history is the most recently evaluated element.

condMatch
  :: [Natural]         -- ^ History of already evaluated values
  -> Element Natural   -- ^ Element to test
  -> Bool              -- ^ Does it match the history?
condMatch []    _        = False
condMatch (h:_) (Val  x) = h == x
condMatch hs    (Sec  x) = and $ zipWith condMatch (tails hs) (reverse x)
condMatch hs    (Mul  x) = or  $ condMatch hs <$> x
condMatch hs    (CMul x) = condMatch hs (toMul x)

-- | Convert internals of conditional multivalue into plain multivalue.

toMul
  :: [([Element Natural], [Element Natural])] -- ^ Pattern\/result pairs
  -> Element Natural   -- ^ Internals of plain multivalue
toMul xs = Mul (xs >>= snd)

-- | A monadic wrapper around 'condMatch'.

matchHistory :: MonadState CalcSt m
  => [Element Natural] -- ^ Stream of elements to test
  -> m Bool            -- ^ Do they match history?
matchHistory xs = do
  hs <- gets clHistory
  return $ or (condMatch hs <$> xs)

-- | Add evaluated value to history.

addHistory :: MonadState CalcSt m => Natural -> m ()
addHistory x = modify $ \c -> c { clHistory = return x <> clHistory c }

-- | Transform 'SyntaxTree' into 'Principle' applying all necessary
-- transformations and resolving references.

toPrin :: HasEnv m
  => SyntaxTree        -- ^ Syntax tree to transform
  -> m Principle       -- ^ Resulting principle
toPrin = fmap simplifySec . toPrin'

-- | Simplify section. There are several simple transformations that are
-- proven to preserve the same resulting stream of naturals.

simplifySec :: Principle -> Principle
simplifySec = (>>= f)
  where f (Sec xs) = simplifySec xs
        f x        = simplifyElt x

-- | Basic simplification of principles.

simplify :: Principle -> Principle
simplify = (>>= simplifyElt)

-- | Simplification of single element. Note that single element can produce
-- several elements after simplification.

simplifyElt :: Element Natural -> Principle
simplifyElt x@(Val _)        = [x]
simplifyElt (Sec  [x])       = simplify [x]
simplifyElt (Mul  [x])       = simplify [x]
simplifyElt (CMul [(_, xs)]) = simplifyElt (Mul xs)
simplifyElt (Sec  xs)        = [Sec (simplifySec xs)]
simplifyElt (Mul  xs)        = [Mul (simplify xs)]
simplifyElt (CMul xs)        = [CMul ((simplify *** simplify) <$> xs)]

-- | The meat of the algorithm that transforms 'SyntaxTree' into 'Principle'.

toPrin' :: HasEnv m
  => SyntaxTree        -- ^ Syntax tree to transform
  -> m Principle       -- ^ Resulting principle
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

-- | Saturated division.

sdiv :: Natural -> Natural -> Natural
sdiv x 0 = x
sdiv x y = x `div` y

-- | Saturated subtraction.

sdif :: Natural -> Natural -> Natural
sdif x y
  | x < y     = 0
  | otherwise = x - y

-- | Concept of looping.

loop :: Element Natural -> Element Natural -> Principle
loop x       (Val y) = replicate (fromIntegral y) x
loop x       (Mul y) = [Mul $ Sec . loop x <$> y]
loop (Sec x) (Sec y) = [Sec . concat $ zipWith loop x (cycle y)]
loop (Mul x) (Sec y) = [Mul . concat $ zipWith loop x (cycle y)]
loop x       _       = [x]

-- | Concept of rotation.

rotate :: Element Natural -> Element Natural -> Element Natural
rotate (Sec   x) (Val y) =
  Sec $ zipWith const (drop (fromIntegral y) (cycle x)) x
rotate x@(Sec _) (Mul y) = Mul $ rotate x <$> y
rotate (Sec   x) (Sec y) = Sec $ zipWith rotate x (cycle y)
rotate x         _       = x

-- | Concept of reversion for elements.

reverse' :: Element Natural -> Element Natural
reverse' x@(Val _) = x
reverse' (Mul   x) = Mul  $ reverse' <$> x
reverse' (Sec   x) = Sec  $ reverse $ reverse' <$> x
reverse' (CMul  x) = CMul $ ((reverse' <$>) *** (reverse' <$>)) <$> x
