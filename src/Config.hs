-- -*- Mode: HASKELL; -*-

-- Config module helps parse unix-style config files.

-- Copyright (c) 2014 Mark Karpov

-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details.

module Config
    ( parseConfig )
    where

-- Import Section --

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

-- Parsing --

language = emptyDef { T.commentLine     = "#"
                    , T.identStart      = letter
                    , T.identLetter     = alphaNum
                    , T.reservedOpNames = ["="]
                    , T.caseSensitive   = True }

lexer = T.makeTokenParser language

identifier = T.identifier    lexer
reservedOp = T.reservedOp    lexer
pstring    = T.stringLiteral lexer
natural    = T.natural       lexer
whiteSpace = T.whiteSpace    lexer

pConfig :: Parser [(String, String)]
pConfig = whiteSpace >> many (try pVarInt <|> pVarString)

pVarInt :: Parser (String, String)
pVarInt =
    do var <- identifier
       reservedOp "="
       x   <- natural
       return (var, show x)

pVarString :: Parser (String, String)
pVarString =
    do var <- identifier
       reservedOp "="
       x   <- pstring
       return (var, x)

parseConfig :: String -> String -> Either String [(String, String)]
parseConfig file str =
    case parse pConfig file str of
      (Right x) -> if null x
                   then Left "invalid syntax of config file"
                   else Right x
      (Left  x) -> Left $ show x
