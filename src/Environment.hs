-- -*- Mode: HASKELL; -*-

-- Environment is formed via evaluation of definitions. Envionment can be
-- changed with a number of different methods.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.

module Environment
    ( Env (..)
--    , getRandGen
    , setRandGen
    , getPrompt
    , setPrompt
    , getPrvLength
    , setPrvLength
    , getPrdLength
    , setPrdLength
    , getFileName
    , setFileName
    , purgeEnv
    , getSrc
    , addDef
    , remDef
    , getSource
    , getNames
    , eval
    , evalDef )
where

-- Import Section --

import Parser
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import qualified Data.Map.Lazy as M
import Data.List
import Control.Applicative ((<$>), (<*>))

-- Data Structures --

data Env = Env
    { eDefinitions :: Definitions
    , eRandGen     :: PureMT
    , ePrompt      :: String
    , ePrvLength   :: Int
    , ePrdLength   :: Int
    , eFileName    :: String
    , eHistory     :: [Int] }

type Definitions = M.Map String DefRep

data DefRep = DefRep
    { drPrin :: Principle
    , drSrc  :: String }

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

getPrdLength :: Monad m => StateT Env m Int
getPrdLength = get >>= return . ePrdLength

setPrdLength :: Monad m => Int -> StateT Env m ()
setPrdLength x = modify (\e -> e { ePrdLength = x })

getFileName :: Monad m => StateT Env m String
getFileName = get >>= return . eFileName

setFileName :: Monad m => String -> StateT Env m ()
setFileName x = modify (\e -> e { eFileName = x })

resetHistory :: Monad m => StateT Env m ()
resetHistory = modify (\e -> e { eHistory = repeat (-1) })

--checkHistory :: Monad m => StateT Env m Bool

--addToHistory

tDefs :: String -> M.Map String Principle -> [String]
tDefs name env =
    case M.lookup name env of
      (Just x) -> name : cm x
      Nothing  -> [name]
    where cm                     = concatMap f
          f (Value          x  ) = []
          f (Reference      x  ) = tDefs x env
          f (Section        xs ) = cm xs
--          f (Replication    x _) = concatMap f x
          f (Multiplication x y) = f x ++ f y
          f (Addition       x y) = f x ++ f y
--          f (Rotation       x _) = concatMap f x
          f (Reverse        xs ) = cm xs
--          f (Range          _ _) = []
          f (Multivalue     xs ) = cm xs
          f (CondMultivalue xs ) = concatMap ((++) <$> cm . fst <*> f . snd) xs


purgeEnv :: Monad m => [String] -> StateT Env m ()
purgeEnv tops = getDefs >>= return . f >>= setDefs
    where f defs = foldr M.delete defs $ M.keys defs \\ musts defs
          musts  = nub . concat . zipWith tDefs tops . repeat . M.map drPrin

getPrinciple :: Monad m => String -> StateT Env m Principle
getPrinciple name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ drPrin x
         Nothing -> return []

getSrc :: Monad m => String -> StateT Env m String
getSrc name =
    do defs <- getDefs
       case M.lookup name defs of
         Just x  -> return $ drSrc x
         Nothing -> return "cannot find the definition\n"

addDef :: Monad m => String -> Principle -> String -> StateT Env m ()
addDef name prin src = getDefs >>=
                       return . M.insert name (DefRep prin src) >>= setDefs

remDef :: Monad m => String -> StateT Env m ()
remDef name = getDefs >>= return . M.delete name >>= setDefs

getSource :: Monad m => StateT Env m String
getSource = getDefs >>= return . concat . map drSrc . M.elems . M.filter f
    where f (DefRep x _) = not $ null x

getNames :: Monad m => StateT Env m [String]
getNames = getDefs >>= return . M.keys

-- Evaluation --

bop :: (Int -> Int -> Int) -> Element -> Element -> Element
bop f (Value x)      (Value y)      = Value      $ f x y
bop f (Section x)    (Section y)    = Section    $ zipWith (bop f) a (cycle b)
    where (a, b) = if length x >= length y then (x, y) else (y, x)
bop f (Multivalue x) y              = Multivalue $ map (bop f y) x
bop f x              (Multivalue y) = Multivalue $ map (bop f x) y
bop f (Section x)    y              = Section    $ map (bop f y) x
bop f x              (Section y)    = Section    $ map (bop f x) y

osc :: (Element -> Element -> Element) -> Principle -> Principle -> Principle
osc f xs ys = init xs ++ [f (last xs) (head ys)] ++ tail ys

simplify :: Monad m => Principle -> StateT Env m Principle
simplify = liftM concat . mapM f
    where f x@(Value          _) = r x
          f (Reference        x) = getPrinciple x >>= simplify
          f (Section         xs) = simplify xs >>= r . Section
          f (Reverse         xs) = simplify xs >>= r . Section . reverse
          f (Multivalue      xs) = simplify xs >>= r . Multivalue
--          f (CondMultivalue )
          f (Multiplication x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc (bop (*)) rx ry
          f (Addition       x y) =
              do rx <- f x
                 ry <- f y
                 return $ osc (bop (+)) rx ry
          r x                    = return [x]

choice :: Monad m => Principle -> StateT Env m Element
choice xs =
    do old <- getRandGen
       let (i, new) = randomInt old
       setRandGen new
       return $ xs !! mod (abs i) (length xs)

resolve :: Monad m => Principle -> StateT Env m [Int]
resolve = liftM concat . mapM f
    where f (Value      x) = return [x]
          f (Section    x) = resolve x
          f (Multivalue x) = choice x >>= f
          f x              = error $ "cannot resolve: " ++ show x

eval :: Monad m => Principle -> Int -> StateT Env m [Int]
eval prin n = simplify prin >>= return . take n . f >>= resolve
    where f [] = []
          f x  = cycle x

evalDef :: Monad m => String -> StateT Env m [Int]
evalDef name =
    do prin <- getPrinciple name
       n    <- getPrdLength
       eval prin n
