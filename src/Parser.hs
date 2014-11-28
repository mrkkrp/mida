-- -*- Mode: HASKELL; -*-

-- Parser module provides means of translation input in form of raw textual
-- data into predefined data structures that represent statements in MIDA.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.

module Parser
    ( Statement (..)
    , Principle
    , Element   (..)
    , parseMida )
where

-- Import Section --

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as T
import Data.List
import Data.Maybe (fromJust)

-- Data Structures --

data Statement
    = Definition String Principle String
    | Exposition Principle
      deriving (Show)

type Principle  = [Element]

data Element
    = Value     Int
    | Reference String
    | Section   Principle
    | Product   Element Element
    | Sum       Element Element
    | Loop      Element Element
    | Rotation  Element Element
    | Reverse   Element
    | Range     Int Int
    | Multi     Principle
    | CMulti    [(Principle, Element)]
      deriving (Show)

-- Parsing --

notes :: [String]
notes = [n ++ show i | i <- [0..9] :: [Int],
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
                                          , "@"
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
--angles     = T.angles     lexer

pSource :: Parser [Statement]
pSource = whiteSpace >> many pDefinition

pDefinition :: Parser Statement
pDefinition =
    do x <- getInput
       n <- identifier
       reservedOp "="
       p <- pPrinciple
       y <- getInput
       return $ Definition n p $ f x y
    where f x y = take (length x - length y) x

pExposition :: Parser [Statement]
pExposition = whiteSpace >> pPrinciple >>= return . (: []) . Exposition

pPrinciple :: Parser Principle
pPrinciple = sepBy (try pExpression <|> pElement) (optional comma)
          <?> "principle"

pElement :: Parser Element
pElement
    =  try pRange
   <|> try pValue
   <|> try pReference
   <|> pSection
   <|> try pMulti
   <|> pCMulti
   <?> "element"

pRange :: Parser Element
pRange =
    do (Value x) <- pValue
       reservedOp ".."
       (Value y) <- pValue
       return $ Range (fromIntegral x) (fromIntegral y)

pValue :: Parser Element
pValue = pNatural <|> pNote <?> "literal value"

pNatural :: Parser Element
pNatural = natural >>= return . Value . fromIntegral

pNote :: Parser Element
pNote =
    do note <- choice $ map (try . string) notes
       whiteSpace
       return . Value . fromJust $ elemIndex note notes

pReference :: Parser Element
pReference =
    do n <- identifier
       notFollowedBy $ reservedOp "="
       return $ Reference n

pSection :: Parser Element
pSection = brackets pPrinciple >>= return . Section

pMulti :: Parser Element
pMulti = braces pPrinciple >>= return . Multi

pCMulti :: Parser Element
pCMulti = braces (many f) >>= return . CMulti
    where f = do c <- parens pPrinciple
                 r <- pPrinciple
                 return (c, Multi r)

pExpression :: Parser Element
pExpression = buildExpressionParser pOperators pElement

pOperators :: [[Operator Char st Element]]
pOperators =
    [[ Prefix (reservedOp "@" >> return Reverse )           ]
     , [ Infix  (reservedOp "*" >> return Product ) AssocLeft
       , Infix  (reservedOp "+" >> return Sum     ) AssocLeft
       , Infix  (reservedOp "$" >> return Loop    ) AssocLeft
       , Infix  (reservedOp "^" >> return Rotation) AssocLeft ]]

parseMida :: String -> String -> Either String [Statement]
parseMida file str =
    case parse parser file str of
      (Right x) -> if null x
                   then Left "invalid definition syntax"
                   else Right x
      (Left  x) -> Left $ show x
    where parser = if elem '=' str then pSource else pExposition
