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
    ( Statement  (..)
    , Expression
    , Element    (..)
    , parseMida )
where

-- Import Section --

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.List
import Control.Applicative ((<$>))

-- Data Structures --

data Statement = Definition String Expression String
               | Exposition Expression
                 deriving (Show)

type Expression = [Element]

data Element = Value Int
             | Reference String
             | Replication Expression Int
             | Range Int Int
             | Random Expression
               deriving (Show)

-- Parsing --

notes :: [String]
notes = [n ++ show i | i <- [0..8],
         n <- ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"]]

language =
    emptyDef { Token.commentStart    = "/*",
               Token.commentEnd      = "*/",
               Token.commentLine     = "//",
               Token.nestedComments  = True,
               Token.identStart      = letter,
               Token.identLetter     = alphaNum,
               Token.reservedNames   = notes,
               Token.reservedOpNames = ["*", "..","="],
               Token.caseSensitive   = True }

lexer = Token.makeTokenParser language

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
natural    = Token.natural    lexer
whiteSpace = Token.whiteSpace lexer
comma      = Token.comma      lexer
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer

pSource :: Parser [Statement]
pSource = whiteSpace >> many pDefinition

pDefinition :: Parser Statement
pDefinition =
    do x    <- getInput
       name <- identifier
       reservedOp "="
       expr <- pExpression
       y    <- getInput
       return $ Definition name expr $ consumed x y
    where consumed x y = take (length x - length y) x

pExposition :: Parser [Statement]
pExposition = whiteSpace >> pExpression >>= return . (: []) . Exposition

pExpression :: Parser Expression
pExpression = sepBy pElement (optional comma)

pElement :: Parser Element
pElement =  try pRange
        <|> try pValue
        <|> try pReference
        <|> try pReplication
        <|> pRandom
        <?> "element of expression"

pValue :: Parser Element
pValue = pNatural <|> pNote <?> "number or note alias"

pNatural :: Parser Element
pNatural = natural >>= return . Value . fromIntegral

pNote :: Parser Element
pNote =
    do note <- choice $ (try . string) <$> notes
       whiteSpace
       return $ Value $ simplify . toNumber $ note
       where toNumber x = (+ 12) <$> elemIndex x notes
             simplify (Just x) = x
             simplify Nothing  = 0

pReference :: Parser Element
pReference =
    do name <- identifier
       notFollowedBy $ reservedOp "="
       return $ Reference name

pReplication :: Parser Element
pReplication =
    do expr <- brackets pExpression
       reservedOp "*"
       n    <- fromIntegral <$> natural
       return $ Replication expr n

pRange :: Parser Element
pRange =
    do (Value x) <- pValue
       reservedOp ".."
       (Value y) <- pValue
       return $ Range (fromIntegral x) (fromIntegral y)

pRandom :: Parser Element
pRandom = braces pExpression >>= return . Random

parseMida :: String -> String -> Either String [Statement]
parseMida file str = case parse parser file str of
                         (Right x) -> if length x == 0
                                      then Left "invalid definition syntax"
                                      else Right x
                         (Left  x) -> Left $ show x
                     where parser = if elem '=' str
                                    then pSource
                                    else pExposition
