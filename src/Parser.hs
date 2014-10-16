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
    (Statement  (..),
     Expression (..),
     Element    (..))
where

-- Import Section --

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Data Structures --

data Statement = Definition String Expression String
               | Exposition Expression
                 deriving (Show)

type Expression = [Element]

data Element = Value Int
             | Reference String
             | Replicaton Int Int
             | Range Int Int
             | Random Int
               deriving (Show)

-- Parsing --

noteShortcuts :: [String]
noteShortcuts = [ch : show i | ch <- ['a'..'g'], i <- [0..7]]

language =
    emptyDef { Token.commentStart    = "/*",
               Token.commentEnd      = "*/",
               Token.commentLine     = "//",
               Token.nestedComments  = True,
               Token.identStart      = letter,
               Token.identLetter     = alphaNum,
               Token.reservedNames   = "R" : noteShortcuts,
               Token.reservedOpNames = ["*", "..","R","="],
               Token.caseSensitive   = True }

lexer = Token.makeTokenParser language

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
comma      = Token.comma      lexer

midaParser :: Parser [Statement]
midaParser = sepBy pStatement eol

eol :: Parser String
eol =  try (string "\n\r")
   <|> try (string "\r\n")
   <|> string "\n"
   <|> string "\r"
   <?> "end of line"

pStatement :: Parser Statement
pStatement = whiteSpace >> (try pDefinition <|> pExposition)

pDefinition :: Parser Statement
pDefinition =
    do x    <- getInput
       name <- identifier
       reservedOp "="
       expr <- pExpression
       y    <- getInput
       return $ Definition name expr $ consumed x y
    where consumed x y = take (length x - length y) x

pExposition :: Parser Statement
pExposition = pExpression >>= return . Exposition

pExpression :: Parser Expression
pExpression = sepBy pElement (optional comma)

pElement :: Parser Element
pElement =  try pValue
        <|> try pReference
        <|> try pReplication
        <|> try pRange
        <|> pRandom
        <?> "expression element"

pValue :: Parser Element
pValue = undefined

pReference :: Parser Element
pReference = undefined

pReplication :: Parser Element
pReplication = undefined

pRange :: Parser Element
pRange = undefined

pRandom :: Parser Element
pRandom = undefined

parseString :: String -> Either String [Statement]
parseString = undefined
