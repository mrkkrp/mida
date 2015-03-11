-- -*- Mode: Haskell; -*-
--
-- Parser module provides means of translation input in form of raw textual
-- data into predefined data structures that represent statements in MIDA.
--
-- Copyright (c) 2014, 2015 Mark Karpov
--
-- MIDA is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- MIDA is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program. If not, see <http://www.gnu.org/licenses/>.

module Parser
    ( Statement (..)
    , Elt'      (..)
    , SyntaxTree
    , parseMida )
where

import Data.List
import Data.Maybe (fromJust)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

----------------------------------------------------------------------------
--                               Data Types                               --
----------------------------------------------------------------------------

data Statement
    = Definition String SyntaxTree String
    | Exposition SyntaxTree
      deriving (Show)

type SyntaxTree = [Elt']

data Elt'
    = Value'    Int
    | Section'  [Elt']
    | Multi'    [Elt']
    | CMulti'   [(Elt', Elt')]
    | Reference String
    | Range     Int Int
    | Product   Elt' Elt'
    | Division  Elt' Elt'
    | Sum       Elt' Elt'
    | Diff      Elt' Elt'
    | Loop      Elt' Elt'
    | Rotation  Elt' Elt'
    | Reverse   Elt'
      deriving (Show)

----------------------------------------------------------------------------
--                               Constants                                --
----------------------------------------------------------------------------

noteAlias :: [String]
noteAlias = [n ++ show i | i <- [0..9] :: [Int],
             n <- ["c","c#","d","d#","e","f","f#","g","g#","a","a#","b"]]

langCommentStart :: String
langCommentStart = "/*"

langCommentEnd :: String
langCommentEnd = "*/"

langCommentLine :: String
langCommentLine = "//"

langProductOp :: String
langProductOp = "*"

langDivisionOp :: String
langDivisionOp = "/"

langSumOp :: String
langSumOp = "+"

langDiffOp :: String
langDiffOp = "-"

langLoopOp :: String
langLoopOp = "$"

langRotationOp :: String
langRotationOp = "^"

langReverseOp :: String
langReverseOp = "@"

langRangeOp :: String
langRangeOp = ".."

langDefinitionOp :: String
langDefinitionOp = "="

langOps :: [String]
langOps =
    [ langProductOp
    , langDivisionOp
    , langSumOp
    , langDiffOp
    , langLoopOp
    , langRotationOp
    , langReverseOp
    , langRangeOp
    , langDefinitionOp ]

langFigures :: [String]
langFigures = ["/\\","\\/","/","\\"]

----------------------------------------------------------------------------
--                          Language and Lexemes                          --
----------------------------------------------------------------------------

lang :: LanguageDef st
lang = emptyDef
       { Token.commentStart    = langCommentStart
       , Token.commentEnd      = langCommentEnd
       , Token.commentLine     = langCommentLine
       , Token.nestedComments  = True
       , Token.identStart      = letter
       , Token.identLetter     = alphaNum
       , Token.reservedNames   = noteAlias
       , Token.reservedOpNames = langOps
       , Token.caseSensitive   = True }

lexer :: Token.TokenParser st
lexer = Token.makeTokenParser lang

angles :: Parser a -> Parser a
angles = Token.angles lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

brackets :: Parser a -> Parser a
brackets = Token.brackets lexer

comma :: Parser String
comma = Token.comma lexer

identifier :: Parser String
identifier = Token.identifier lexer

natural :: Parser Integer
natural = Token.natural lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

----------------------------------------------------------------------------
--                                Parsing                                 --
----------------------------------------------------------------------------

parseMida :: String -> String -> Either String [Statement]
parseMida file str =
    case parse parser file str of
      Right x -> if null x
                 then Left $ "\"" ++ file ++ "\":\ninvalid definition syntax"
                 else Right x
      Left  x -> Left $ show x
    where parser = if langDefinitionOp `isInfixOf` str
                   then pSource
                   else pExposition

pSource :: Parser [Statement]
pSource = whiteSpace >> many pDefinition

pDefinition :: Parser Statement
pDefinition = do
  x <- getInput
  n <- identifier
  reservedOp langDefinitionOp
  p <- pPrinciple
  y <- getInput
  return $ Definition n p $ take (length x - length y) x

pExposition :: Parser [Statement]
pExposition = whiteSpace >> pPrinciple >>= return . return . Exposition

pPrinciple :: Parser SyntaxTree
pPrinciple = do
  result <- sepBy (pExpression <|> pElement) (optional comma)
  optional . choice $ map f langOps
  return result
    where f x = reservedOp x >> unexpected ("\"" ++ x ++ "\"")

pElement :: Parser Elt'
pElement
    =  try pRange
   <|> pValue
   <|> try pReference
   <|> pSection
   <|> try pMulti
   <|> pCMulti
   <?> "element"

pRange :: Parser Elt'
pRange = do
  Value' x <- pValue
  reservedOp langRangeOp
  Value' y <- pValue
  return $ Range x y

pValue :: Parser Elt'
pValue = pNatural <|> pNote <|> pFigure <?> "literal value"

pNatural :: Parser Elt'
pNatural = natural >>= return . Value' . fromIntegral

pNote :: Parser Elt'
pNote = do
  note <- choice $ map (try . string) noteAlias
  whiteSpace
  return . Value' . fromJust $ note `elemIndex` noteAlias

pFigure :: Parser Elt'
pFigure = do
  figure <- choice $ map (try . string) langFigures
  whiteSpace
  return . Value' . (* 128) . succ . fromJust $ figure `elemIndex` langFigures

pReference :: Parser Elt'
pReference = do
  n <- identifier
  notFollowedBy $ reservedOp langDefinitionOp
  return $ Reference n

pSection :: Parser Elt'
pSection = brackets pPrinciple >>= return . Section'

pMulti :: Parser Elt'
pMulti = braces pPrinciple >>= return . Multi'

pCMulti :: Parser Elt'
pCMulti = braces (many f) >>= return . CMulti'
    where f = do
            c <- angles pPrinciple
            r <- pPrinciple
            return (Multi' c, Multi' r)

pExpression :: Parser Elt'
pExpression = buildExpressionParser pOperators (parens pExpression <|> pElement)
              <?> "expression"

pOperators :: [[Operator Char () Elt']]
pOperators =
    [[ Prefix (reservedOp langReverseOp >> return Reverse ) ]
     , [ Infix (reservedOp langProductOp  >> return Product ) AssocLeft
       , Infix (reservedOp langDivisionOp >> return Division) AssocLeft
       , Infix (reservedOp langSumOp      >> return Sum     ) AssocLeft
       , Infix (reservedOp langDiffOp     >> return Diff    ) AssocLeft
       , Infix (reservedOp langLoopOp     >> return Loop    ) AssocLeft
       , Infix (reservedOp langRotationOp >> return Rotation) AssocLeft ]]
