-- -*- Mode: HASKELL; -*-
--
-- This module describes process of principle evaluation.
--
-- Copyright (c) 2014 Mark Karpov
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.
--
-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

module Eval
    ( eval
    , evalDef )
    where

import Data.List
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import Control.Applicative ((<$>), (<*>))
import Parser
import Environment

-- data types --

data Temp = Temp
    { tBadRow  :: Int
    , tCounter :: Int
    , tLimit   :: Int
    , tHistory :: [Int] }

-- evaluation --

getBadRow :: Monad m => StateT Temp m Int
getBadRow = get >>= return . tBadRow

addBadRow :: Monad m => StateT Temp m ()
addBadRow = modify (\e -> e { tBadRow = succ (tBadRow e)})

getCounter :: Monad m => StateT Temp m Int
getCounter = get >>= return . tCounter

getLimit :: Monad m => StateT Temp m Int
getLimit = get >>= return . tLimit

getHistory :: Monad m => StateT Temp m [Int]
getHistory = get >>= return . tHistory

addHistory :: Monad m => Int -> StateT Temp m ()
addHistory x = modify (\e -> e { tBadRow  = 0
                               , tCounter = succ (tCounter e)
                               , tHistory = x : tHistory e})

done :: Monad m => StateT Temp m Bool
done =
    do b <- getBadRow
       c <- getCounter
       l <- getLimit
       return (c >= l || b >= l)

getResult :: Monad m => StateT Temp m [Int]
getResult =
    do h <- getHistory
       c <- getCounter
       l <- getLimit
       return $ if (c >= l) then reverse $ take c h else []

mapCond :: (Element -> Element) -> Element -> Element
mapCond f (CMulti xs) = CMulti $ map ((,) <$> (map f . fst) <*> (f . snd)) xs

fuop :: (Int -> Int -> Int) -> Element -> Element -> Element
fuop f = flip (uop f)

uop :: (Int -> Int -> Int) -> Element -> Element -> Element
uop f (Value x) (Value y) = Value $ f x y
uop f (Multi x) y = Multi $ map (fuop f y) x
uop f x (Multi y) = Multi $ map (uop f x) y
uop f x@(CMulti _) y = mapCond (fuop f y) x
uop f x y@(CMulti _) = mapCond (uop f x) y
uop f (Section x) (Section y) = Section $ zipWith (uop f) x (cycle y)
uop f (Section x) y = Section $ map (fuop f y) x
uop f x (Section y) = Section $ map (uop f x) y

loop :: Element -> Element -> [Element]
loop x (Value y) = replicate y x
loop x (Multi y) = [Multi $ map (Section . loop x) y]
loop (Section x) (Section y) = [Section $ concat $ zipWith loop x (cycle y)]
loop x (Section y) = [Section $ concat $ map (loop x) y]
loop x y@(CMulti _) = [mapCond (Section . loop x) y]

rotate :: Element -> Element -> Element
rotate (Section x) (Value y) = Section $ zipWith const (drop y (cycle x)) x
rotate x@(Section _) (Multi y) = Multi $ map (rotate x) y
rotate (Section x) (Section y) = Section $ zipWith rotate x (cycle y)
rotate x@(Section _) y@(CMulti _) = mapCond (rotate x) y
rotate x _ = x

rvrs :: Element -> Element
rvrs x@(Value _) = x
rvrs (Multi x) = Multi $ map rvrs x
rvrs (Section x) = Section $ reverse $ map rvrs x
rvrs x@(CMulti _) = mapCond rvrs x

osc :: (Element -> Element -> [Element]) -> Principle -> Principle -> Principle
osc _ [] [] = []
osc _ xs [] = xs
osc _ [] ys = ys
osc f xs ys = init xs ++ (f (last xs) (head ys)) ++ tail ys

osd :: (Element -> Element) -> Principle -> Principle
osd _ [] = []
osd f xs = f (head xs) : tail xs

simplify :: Monad m => Principle -> StateT Env m Principle
simplify = liftM concat . mapM f
    where r x              = return [x]
          f x@(Value    _) = r x
          f (Reference  x) = getPrin x >>= simplify
          f (Section   xs) = simplify xs >>= r . Section
          f (Range    x y) = return . map Value $
                            if x > y then [x,x-1..y] else [x..y]
          f (Multi     xs) = simplify xs >>= r . Multi
          f (CMulti    xs) =
              let g (c, x) =
                      do rc <- simplify c
                         rx <- f x
                         return (rc, head rx)
              in mapM g xs >>= r . CMulti
          f (Product  x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc (\a b -> [uop (*) a b]) rx ry
          f (Sum      x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc (\a b -> [uop (+) a b]) rx ry
          f (Loop     x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc loop rx ry
          f (Rotation x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc (\a b -> [rotate a b]) rx ry
          f (Reverse    x) = f x >>= return . osd rvrs

choice :: Monad m => [a] -> StateT Env m (Maybe a)
choice [] = return Nothing
choice xs =
    do old <- getRandGen
       let (n, new) = randomInt old
       setRandGen new
       return $ Just $ xs !! mod (abs n) (length xs)

condTest :: [Int] -> Element -> Bool
condTest (h:_) (Value   x) = h == x
condTest hs    (Section x) = and $ zipWith condTest (tails hs) (reverse x)
condTest hs    (Multi   x) = or  $ zipWith condTest (repeat hs) x
condTest hs    (CMulti  x) = condTest hs . Multi . map snd $ x

resolve :: Monad m => Principle -> StateT Temp (StateT Env m) [Int]
resolve []     = return []
resolve (y:ys) =
    do f y
       addBadRow
       b <- done
       if b then getResult else resolve ys
    where f (Value   x) = addHistory x >> return [x]
          f (Section x) = mapM f x >>= return . concat
          f (Multi   x) =
              do p <- lift $ choice x
                 case p of
                   Just r  -> f r
                   Nothing -> return []
          f (CMulti  x) =
              do hs <- getHistory
                 case find (any (condTest hs) . fst) x of
                   Just (_, r) -> f r
                   Nothing     -> f . Multi . map snd $ x

eval :: Monad m => Principle -> Int -> StateT Env m [Int]
eval prin n =
    do p <- simplify prin
       let result = runStateT (resolve . f $ p)
                    Temp { tBadRow  = 0
                         , tCounter = 0
                         , tLimit   = n
                         , tHistory = repeat (-1) }
       result >>= return . fst
    where f [] = []
          f x  = cycle x

evalDef :: Monad m => String -> StateT Env m [Int]
evalDef name =
    do prin <- getPrin name
       n    <- getBlockSize
       eval prin n
