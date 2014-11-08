-- -*- Mode: HASKELL; -*-

-- Environment is formed via evaluation of definitions. Envionment can
-- be changed with a number of different methods.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Environment
    ( Env (..)
    , getRandGen
    , setRandGen
    , getPrompt
    , setPrompt
    , getPrvLength
    , setPrvLength
    , getFileName
    , setFileName
    , purgeEnv
    , getSrc
    , addDef
    , remDef
    , getSource
    , getNames
    , eval
    , evalItem )
where

-- Import Section --

import Parser
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import qualified Data.Map.Lazy as M
import Data.List

-- Data Structures --

data Env = Env
    { eDefinitions  :: Definitions
    , eRandGen      :: PureMT
    , ePrompt       :: String
    , ePrvLength    :: Int
    , eFileName     :: String }

type Definitions = M.Map String DefRep

data DefRep = DefRep
    { drPrinciple :: Principle
    , drSource    :: String }

-- Environment API --

getDefs :: Monad m => StateT Env m Definitions
getDefs = get >>= return . eDefinitions

setDefs :: Monad m => Definitions -> StateT Env m ()
setDefs x = modify (\e -> e { eDefinitions = x })

getRandGen :: Monad m => StateT Env m PureMT
getRandGen = get >>= return . eRandGen

setRandGen :: Monad m => PureMT -> StateT Env m ()
setRandGen x = modify (\e -> e { eRandGen = x })

getPrompt :: Monad m => StateT Env m String
getPrompt = get >>= return . ePrompt

setPrompt :: Monad m => String -> StateT Env m ()
setPrompt x = modify (\e -> e { ePrompt = x })

getPrvLength :: Monad m => StateT Env m Int
getPrvLength = get >>= return . ePrvLength

setPrvLength :: Monad m => Int -> StateT Env m ()
setPrvLength x = modify (\e -> e { ePrvLength = x })

getFileName :: Monad m => StateT Env m String
getFileName = get >>= return . eFileName

setFileName :: Monad m => String -> StateT Env m ()
setFileName x = modify (\e -> e { eFileName = x })

traverseDefs :: String -> M.Map String Principle -> [String]
traverseDefs name env =
    case M.lookup name env of
      (Just x) -> name : concatMap f x
      Nothing  -> [name]
    where f (Value          x  ) = []
          f (Reference      x  ) = traverseDefs x env
          f (Replication    x _) = concatMap f x
          f (Multiplication x _) = concatMap f x
          f (Addition       x _) = concatMap f x
          f (Rotation       x _) = concatMap f x
          f (Reverse        x  ) = concatMap f x
          f (Range          _ _) = []
          f (Random         x  ) = concatMap f x
          f (CondRandom     x  ) = concatMap (concatMap f . snd) x

badItems :: [String] -> Definitions -> [String]
badItems given defs = filter (\x -> not $ elem x goodItems) $ M.keys defs
    where goodItems = nub . concat $ zipWith traverseDefs given naked
          naked     = repeat $ M.map drPrinciple defs

purgeEnv :: Monad m => [String] -> StateT Env m ()
purgeEnv given = getDefs >>= return . deleteItems >>= setDefs
    where deleteItems defs = foldr M.delete defs $ badItems given defs

getPrinciple :: Monad m => String -> StateT Env m Principle
getPrinciple name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ drPrinciple x
         Nothing -> return []

getSrc :: Monad m => String -> StateT Env m String
getSrc name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ drSource x
         Nothing -> return "cannot find the definition\n"

addDef :: Monad m => String -> Principle -> String -> StateT Env m ()
addDef name exp src = getDefs >>=
                      return . M.insert name (DefRep exp src) >>= setDefs

remDef :: Monad m => String -> StateT Env m ()
remDef name = getDefs >>= return . M.delete name >>= setDefs

getSource :: Monad m => StateT Env m String
getSource = return . concat . map drSource . M.elems . M.filter f =<< getDefs
    where f (DefRep x _) = not $ null x

getNames :: Monad m => StateT Env m [String]
getNames = getDefs >>= return . M.keys

-- Evaluation --

data Elt
    = Vl Int
    | Rn [Int]
    | Cr [(Int, [Int])]
      deriving (Show)

resolve :: Monad m => Principle -> StateT Env m [Elt]
resolve = liftM concat . mapM f
    where f (Value          x  ) = return [Vl x]
          f (Reference      x  ) = getPrinciple x >>= resolve
          f (Replication    x n) = resolve x >>= return . concat . replicate n
          f (Multiplication x n) = resolve x >>= return . map (multiply n)
          f (Addition       x n) = resolve x >>= return . map (add n)
          f (Rotation       x n) = resolve x >>= return . rotate n
          f (Reverse        x  ) = resolve x >>= return . reverse
          f (Range          x y) = return $ map Vl $
                                   if x <= y then [x..y] else [x,x-1..y]
          f (Random x)           =
              do r <- resolve x
                 if null r then return [] else return [Rn (derand r)]
          f (CondRandom x)       =
              do r <- mapM g x
                 return [Cr $ filter (not . null . snd) r]
          g (k, x)               = resolve x >>= \r -> return (k, derand r)

multiply :: Int -> Elt -> Elt
multiply n (Vl x) = Vl $ x * n
multiply n (Rn x) = Rn $ map (* n) x
multiply n (Cr x) = Cr $ map (\(k, y) -> (k * n, map (* n) y)) x

add :: Int -> Elt -> Elt
add n (Vl x) = Vl $ x + n
add n (Rn x) = Rn $ map (+ n) x
add n (Cr x) = Cr $ map (\(k, y) -> (k * n, map (+ n) y)) x

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith const (drop n (cycle xs)) xs

norm :: [[Int]] -> [Int]
norm xs = concatMap f xs
    where f x = concatMap (replicate $ n `div` length x) x
          n = foldr lcm 1 $ map length xs

derand :: [Elt] -> [Int]
derand = norm . map f
    where f (Vl x) = [x]
          f (Rn x) = x
          f (Cr x) = norm $ map snd x

randoms :: PureMT -> [Int]
randoms gen = x : randoms g
    where (x, g) = randomInt gen

choice :: [Int] -> Int -> Int
choice xs v = xs !! mod (abs v) (length xs)

resElt :: Monad m => [Elt] -> StateT Env m [Int]
resElt xs =
    do gen <- getRandGen
       setRandGen $ pureMT . fromIntegral . fst $ randomInt gen
       let r = zipWith3 f xs (randoms gen) ((-1) : r)
       return r
    where f (Vl x) _ _ = x
          f (Rn x) v _ = choice x v
          f (Cr x) v p = let g (Just y) = y
                             g Nothing  = concatMap snd x
                         in choice (g $ lookup p x) v

eval :: Monad m => Principle -> StateT Env m [Int]
eval expr = resolve expr >>= return . f >>= resElt
    where f [] = []
          f x  = cycle x

evalItem :: Monad m => String -> StateT Env m [Int]
evalItem name = getPrinciple name >>= eval
