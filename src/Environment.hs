-- -*- Mode: HASKELL; -*-

-- Environment is formed via evaluation of definitions. For practical
-- reasons environment also contains a number of useful values.

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
    , getRandGen
    , setRandGen
    , getPrompt
    , setPrompt
    , getPrvLength
    , setPrvLength
    , getBlockSize
    , setBlockSize
    , getFileName
    , setFileName
    , getPrvCmd
    , setPrvCmd
    , addDef
    , remDef
    , purgeEnv
    , getPrin
    , getSrc
    , fullSrc
    , getNames )
where

import Data.List
import qualified Data.Map as M
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64
import Control.Applicative ((<$>), (<*>))
import Parser

-- data types --

data Env = Env
    { eDefs      :: Defs
    , eRandGen   :: PureMT
    , ePrompt    :: String
    , ePrvLength :: Int
    , eBlockSize :: Int
    , eFileName  :: String
    , ePrvCmd    :: String }

type Defs = M.Map String DefRep

data DefRep = DefRep
    { drPrin :: Principle
    , drSrc  :: String }

-- environment api --

getDefs :: Monad m => StateT Env m Defs
getDefs = get >>= return . eDefs

setDefs :: Monad m => Defs -> StateT Env m ()
setDefs x = modify (\e -> e { eDefs = x })

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

getBlockSize :: Monad m => StateT Env m Int
getBlockSize = get >>= return . eBlockSize

setBlockSize :: Monad m => Int -> StateT Env m ()
setBlockSize x = modify (\e -> e { eBlockSize = x })

getFileName :: Monad m => StateT Env m String
getFileName = get >>= return . eFileName

setFileName :: Monad m => String -> StateT Env m ()
setFileName x = modify (\e -> e { eFileName = x })

getPrvCmd :: Monad m => StateT Env m String
getPrvCmd = get >>= return . ePrvCmd

setPrvCmd :: Monad m => String -> StateT Env m ()
setPrvCmd x = modify (\e -> e { ePrvCmd = x })

addDef :: Monad m => String -> Principle -> String -> StateT Env m ()
addDef name p s = getDefs >>= return . M.insert name (DefRep p s) >>= setDefs

remDef :: Monad m => String -> StateT Env m ()
remDef name = getDefs >>= return . M.delete name >>= setDefs

tDefs :: String -> M.Map String Principle -> [String]
tDefs name defs =
    case M.lookup name defs of
      Just x  -> name : cm x
      Nothing -> [name]
    where cm                = concatMap f
          f (Value     _  ) = []
          f (Reference x  ) = tDefs x defs
          f (Section   xs ) = cm xs
          f (Product   x y) = f x ++ f y
          f (Sum       x y) = f x ++ f y
          f (Loop      x y) = f x ++ f y
          f (Rotation  x y) = f x ++ f y
          f (Reverse   x  ) = f x
          f (Range     _ _) = []
          f (Multi     xs ) = cm xs
          f (CMulti    xs ) = concatMap ((++) <$> cm . fst <*> f . snd) xs

purgeEnv :: Monad m => [String] -> StateT Env m ()
purgeEnv tops = getDefs >>= return . f >>= setDefs
    where f defs = foldr M.delete defs $ M.keys defs \\ musts defs
          musts  = nub . concat . zipWith tDefs tops . repeat . M.map drPrin

getPrin :: Monad m => String -> StateT Env m Principle
getPrin name =
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


fullSrc :: Monad m => StateT Env m String
fullSrc = getDefs >>= return . concat . map drSrc . M.elems . M.filter f
    where f (DefRep x _) = not $ null x

getNames :: Monad m => StateT Env m [String]
getNames = getDefs >>= return . M.keys
