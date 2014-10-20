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
    , eval
    , evalItem )
where

-- Import Section --

import Parser
import Control.Monad.State
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
    { dpExpression :: Expression
    , dpSource     :: String }

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

traverseDefs :: String -> M.Map String Expression -> [String]
traverseDefs name env =
    case M.lookup name env of
      (Just x) -> name : concatMap f x
      Nothing  -> [name]
    where f (Value x)         = []
          f (Reference x)     = traverseDefs x env
          f (Replication x _) = concatMap f x
          f (Range _ _)       = []
          f (Random x)        = concatMap f x

badItems :: [String] -> Definitions -> [String]
badItems given defs = filter (\x -> not $ elem x goodItems) $ M.keys defs
    where goodItems = nub . concat $ zipWith traverseDefs given naked
          naked     = repeat $ M.map dpExpression defs

purgeEnv :: Monad m => [String] -> StateT Env m ()
purgeEnv given = getDefs >>= return . deleteItems >>= setDefs
    where deleteItems defs = foldr M.delete defs $ badItems given defs

getExp :: Monad m => String -> StateT Env m Expression
getExp name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ dpExpression x
         Nothing -> return []

getSrc :: Monad m => String -> StateT Env m String
getSrc name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ dpSource x
         Nothing -> return []

addDef :: Monad m => String -> Expression -> String -> StateT Env m ()
addDef name exp src = getDefs >>=
                      return . M.insert name (DefRep exp src) >>= setDefs

remDef :: Monad m => String -> StateT Env m ()
remDef name = getDefs >>= return . M.delete name >>= setDefs

getSource :: Monad m => StateT Env m String
getSource = return . concat . map dpSource . M.elems . M.filter f =<< getDefs
    where f (DefRep x _) = not $ null x

-- Evaluation --

data Elt
    = Vl Int
    | Rn [Int]
      deriving (Show)

resolve :: Monad m => Expression -> StateT Env m [Elt]
resolve = liftM concat . mapM f
    where f (Value       x)   = return [Vl x]
          f (Reference   x)   = getExp x >>= resolve
          f (Replication x n) = resolve x >>= return . concat . replicate n
          f (Range       x y) = return $ map Vl $
                                if x <= y then [x..y] else [x,x-1..y]
          f (Random      x)   = resolve x >>= return . (: []) . Rn . derand

derand :: [Elt] -> [Int]
derand xs = concatMap r xs
    where r (Vl x) = replicate n x
          r (Rn x) = concatMap (replicate $ n `div` length x) x
          n = foldr1 lcm $ map f xs
          f (Vl _) = 1
          f (Rn x) = length x

randoms :: PureMT -> [Int]
randoms gen = x : randoms g
    where (x, g) = randomInt gen

resElt :: Monad m => [Elt] -> StateT Env m [Int]
resElt xs =
    do gen <- getRandGen
       setRandGen $ pureMT . fromIntegral . fst $ randomInt gen
       return $ zipWith f xs $ randoms gen
    where f (Vl x) _ = x
          f (Rn x) v = x !! mod (abs v) (length x)

eval :: Monad m => Expression -> StateT Env m [Int]
eval expr = resolve expr >>= return . f >>= resElt
    where f [] = []
          f x  = cycle x

evalItem :: Monad m => String -> StateT Env m [Int]
evalItem name = getExp name >>= eval
