-- -*- Mode: HASKELL; -*-

-- Parser module provides means of translation input in form of
-- strings into predefined data structures that represent statements
-- in MIDA.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.

module Parser
    ( Statement (..)
    , Principle
    , Element   (..)
    , parseMida )
where

-- Import Section --

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.List
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))

-- Data Structures --

data Statement
    = Definition String Principle String
    | Exposition Principle
      deriving (Show)

type Principle = [Element]

data Element
    = Value          Int
    | Reference      String
    | Replication    Principle Int
    | Multiplication Principle Int
    | Addition       Principle Int
    | Rotation       Principle Int
    | Reverse        Principle
    | Range          Int Int
    | Random         Principle
    | CondRandom     [(Int, Principle)]
      deriving (Show)

-- Parsing --

notes :: [String]
notes = [n ++ show i | i <- [0..8],
         n <- ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"]]

language = emptyDef { T.commentStart    = "/*"
                    , T.commentEnd      = "*/"
                    , T.commentLine     = "//"
                    , T.nestedComments  = True
                    , T.identStart      = letter
                    , T.identLetter     = alphaNum
                    , T.reservedNames   = notes
                    , T.reservedOpNames = [ "*"
                                          , "+"
                                          , "$"
                                          , "^"
                                          , ".."
                                          , "=" ]
                    , T.caseSensitive   = True }

lexer = T.makeTokenParser language

identifier = T.identifier lexer
reserved   = T.reserved   lexer
reservedOp = T.reservedOp lexer
parens     = T.parens     lexer
natural    = T.natural    lexer
whiteSpace = T.whiteSpace lexer
comma      = T.comma      lexer
braces     = T.braces     lexer
brackets   = T.brackets   lexer
angles     = T.angles     lexer

pSource :: Parser [Statement]
pSource = whiteSpace >> many pDefinition

pDefinition :: Parser Statement
pDefinition =
    do x    <- getInput
       name <- identifier
       reservedOp "="
       prin <- pPrinciple
       y    <- getInput
       return $ Definition name prin $ f x y
    where f x y = take (length x - length y) x

pExposition :: Parser [Statement]
pExposition = whiteSpace >> pPrinciple >>= return . (: []) . Exposition

pPrinciple :: Parser Principle
pPrinciple = sepBy pElement (optional comma)

pElement :: Parser Element
pElement
    =  try pRange
   <|> try pValue
   <|> try pReference
   <|> try pMultiplication
   <|> try pAddition
   <|> try pReplication
   <|> try pRotation
   <|> pReverse
   <|> try pRandom
   <|> pCondRandom
   <?> "element of expression"

pRange :: Parser Element
pRange =
    do (Value x) <- pValue
       reservedOp ".."
       (Value y) <- pValue
       return $ Range (fromIntegral x) (fromIntegral y)

pValue :: Parser Element
pValue = pNatural <|> pNote <?> "number or note alias"

pNatural :: Parser Element
pNatural = natural >>= return . Value . fromIntegral

pNote :: Parser Element
pNote =
    do note <- choice $ map (try . string) notes
       whiteSpace
       return . Value . fromJust $ elemIndex note notes

pReference :: Parser Element
pReference =
    do name <- identifier
       notFollowedBy $ reservedOp "="
       return $ Reference name

pBracketsOp :: String -> (Principle -> Int -> Element) -> Parser Element
pBracketsOp op f =
    do prin <- brackets pPrinciple
       reservedOp op
       n    <- fromIntegral <$> natural
       return $ f prin n

pMultiplication = pBracketsOp "*" Multiplication
pAddition       = pBracketsOp "+" Addition
pReplication    = pBracketsOp "$" Replication
pRotation       = pBracketsOp "^" Rotation

pReverse :: Parser Element
pReverse = angles pPrinciple >>= return . Reverse

pRandom :: Parser Element
pRandom = braces pPrinciple >>= return . Random

pCondElt :: Parser (Int, Principle)
pCondElt =
    do (Value v) <- parens pValue
       expr      <- pPrinciple
       return (v, expr)

pCondRandom :: Parser Element
pCondRandom = braces (many pCondElt) >>= return . CondRandom

parseMida :: String -> String -> Either String [Statement]
parseMida file str =
    case parse parser file str of
      (Right x) -> if null x
                   then Left "invalid definition syntax"
                   else Right x
      (Left  x) -> Left $ show x
    where parser = if elem '=' str then pSource else pExposition
