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
    , MidaM
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
import Control.Applicative ((<$>))
import System.Random.Mersenne.Pure64
import qualified Data.Map.Lazy as Map
import Data.List

-- Data Structures --

data Env = Env { eDefinitions  :: Definitions
               , eRandGen      :: PureMT
               , ePrompt       :: String
               , ePrvLength    :: Int
               , eFileName     :: String }

type Definitions = Map.Map String DefRep

data DefRep = DefRep { dpExpression :: Expression
                     , dpSource     :: String }

type MidaM = StateT Env IO

-- Environment API --

getDefs :: MidaM Definitions
getDefs = eDefinitions <$> get
setDefs :: Definitions -> MidaM ()
setDefs x = modify (\e -> e { eDefinitions = x })
getRandGen :: MidaM PureMT
getRandGen = eRandGen <$> get
setRandGen :: PureMT -> MidaM ()
setRandGen x = modify (\e -> e { eRandGen = x })
getPrompt :: MidaM String
getPrompt = ePrompt <$> get
setPrompt :: String -> MidaM ()
setPrompt x = modify (\e -> e { ePrompt = x })
getPrvLength :: MidaM Int
getPrvLength = ePrvLength <$> get
setPrvLength :: Int -> MidaM ()
setPrvLength x = modify (\e -> e { ePrvLength = x })
getFileName :: MidaM String
getFileName = eFileName <$> get
setFileName :: String -> MidaM ()
setFileName x = modify (\e -> e { eFileName = x })

traverseDefs :: String -> Map.Map String Expression -> [String]
traverseDefs name env =
    case Map.lookup name env of
      (Just x) -> name : concatMap f x
      Nothing  -> [name]
    where f (Value x)         = []
          f (Reference x)     = traverseDefs x env
          f (Replication x _) = concatMap f x
          f (Range _ _)       = []
          f (Random x)        = concatMap f x

badItems :: [String] -> Definitions -> [String]
badItems given defs = filter (\x -> not $ elem x goodItems) $ Map.keys defs
    where goodItems = nub . concat $ zipWith traverseDefs given naked
          naked     = repeat $ Map.map dpExpression defs

purgeEnv :: [String] -> MidaM ()
purgeEnv given = deleteItems <$> getDefs >>= setDefs
    where deleteItems env = foldr Map.delete env $ badItems given env

getExp :: String -> MidaM Expression
getExp name =
    do defs <- getDefs
       case Map.lookup name defs of
         Just x  -> return $ dpExpression x
         Nothing -> return []

getSrc :: String -> MidaM String
getSrc name =
    do defs <- getDefs
       case Map.lookup name defs of
         Just x  -> return $ dpSource x
         Nothing -> return []

addDef :: String -> Expression -> String -> MidaM ()
addDef name exp src = Map.insert name (DefRep exp src) <$> getDefs >>= setDefs

remDef :: String -> MidaM ()
remDef name = Map.delete name <$> getDefs >>= setDefs

getSource :: MidaM String
getSource = concat . map dpSource . Map.elems . Map.filter f <$> getDefs
    where f (DefRep x _) = length x > 0

-- Evaluation --

data Elt = Vl Int
         | Rn [Int]
           deriving (Show)

resolve :: Expression -> MidaM [Elt]
resolve = liftM concat . mapM f
    where f (Value       x)   = return $ [Vl x]
          f (Reference   x)   = getExp x >>= resolve
          f (Replication x n) = concat . replicate n <$> resolve x
          f (Range       x y) = return $ map Vl $
                                if x <= y then [x..y] else [x,x-1..y]
          f (Random      x)   = (: []) . Rn . derand <$> resolve x

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

resElt :: [Elt] -> MidaM [Int]
resElt xs =
    do gen <- getRandGen
       setRandGen $ pureMT . fromIntegral . fst $ randomInt gen
       return $ zipWith f xs $ randoms gen
    where f (Vl x) _ = x
          f (Rn x) v = x !! mod (abs v) (length x)

eval :: Expression -> MidaM [Int]
eval expr = f <$> resolve expr >>= resElt
    where f [] = []
          f x  = cycle x

evalItem :: String -> MidaM [Int]
evalItem name = getExp name >>= eval
