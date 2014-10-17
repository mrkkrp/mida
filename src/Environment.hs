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

module Environment where

-- Import Section --

import Parser
import Control.Monad.State
import Control.Applicative ((<$>))
import System.Random
import qualified Data.Map.Lazy as Map
import Data.List

-- Data Structures --

type Env = Map.Map String DefRep

data DefRep = DefRep { getExpression :: Expression,
                       getSource     :: String }

data Result = Defined String
            | Evaluated [Int]
              deriving (Show)

type MidaM = StateT Env IO

-- Environment API --

getEnv :: MidaM Env
getEnv = get

setEnv :: Env -> MidaM ()
setEnv x = modify (\_ -> x)

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

badItems :: [String] -> Env -> [String]
badItems given env = filter (\x -> not $ elem x goodItems) $ Map.keys env
    where goodItems = nub . concat $ zipWith traverseDefs given naked
          naked     = repeat $ Map.map getExpression env

purgeEnv :: [String] -> MidaM ()
purgeEnv given = deleteItems <$> getEnv >>= setEnv
    where deleteItems env = foldr Map.delete env $ badItems given env

getExp :: String -> MidaM Expression
getExp name =
    do env <- getEnv
       case Map.lookup name env of
         Just x  -> return $ getExpression x
         Nothing -> return []

getSrc :: String -> MidaM String
getSrc name =
    do env <- getEnv
       case Map.lookup name env of
         Just x  -> return $ getSource x
         Nothing -> return []

addDef :: Statement -> MidaM ()
addDef (Definition name exp src) =
    Map.insert name (DefRep exp src) <$> getEnv >>= setEnv

remDef :: String -> MidaM ()
remDef name = Map.delete name <$> getEnv >>= setEnv

completeSource :: MidaM String
completeSource = concatMap (++ "\n\n") . map getSource . Map.elems <$> getEnv

-- Evaluation --

eval :: Statement -> MidaM Result
eval d@(Definition name _ _) = addDef d >> return (Defined name)
eval d@(Exposition expr)     = undefined

evalSource :: [Statement] -> MidaM ()
evalSource = mapM_ eval
