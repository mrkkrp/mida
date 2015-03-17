-- -*- Mode: Haskell; -*-
--
-- This module describes how to build syntax tree from textual
-- representation of MIDA source.
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

module Mida.Representation.Parser
    ( Statement (..)
    , probeMida
    , parseMida )
where

import Control.Applicative ((<$>), (<*>))
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.List (elemIndex, isInfixOf, isSuffixOf)
import Data.Maybe (fromJust)

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Mida.Language.SyntaxTree
import qualified Mida.Representation.Base as B

data Statement
    = Definition String SyntaxTree String
    | Exposition SyntaxTree
      deriving (Show)

probeMida :: String -> Bool
probeMida arg = not $ or [isSuffixOf "," s, f "[]", f "{}", f "<>", f "()"]
    where s       = reverse . dropWhile isSpace . reverse $ arg
          g x     = length $ filter (== x) s
          f [x,y] = ((&&) <$> (> 0) <*> (/= g y)) (g x)
          f _     = False

parseMida :: String -> String -> Either String [Statement]
parseMida file str =
    case parse parser file str of
      Right x -> if null x
                 then Left $ "\"" ++ file ++ "\":\ninvalid definition syntax"
                 else Right x
      Left  x -> Left $ show x
    where parser = if B.definitionOp `isInfixOf` str
                   then pSource
                   else pExposition

pSource :: Parser [Statement]
pSource = whiteSpace >> many pDefinition

pDefinition :: Parser Statement
pDefinition = do
  x <- getInput
  n <- identifier
  reservedOp B.definitionOp
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

pElement :: Parser Sel
pElement
    =  try pRange
   <|> pValue
   <|> try pReference
   <|> pSection
   <|> try pMulti
   <|> pCMulti
   <?> "element"

pRange :: Parser Sel
pRange = do
  Value x <- pValue
  reservedOp B.rangeOp
  Value y <- pValue
  return $ Range x y

pValue :: Parser Sel
pValue = pNatural <|> pNote <|> pFigure <?> "literal value"

pNatural :: Parser Sel
pNatural = natural >>= return . Value . fromIntegral

pNote :: Parser Sel
pNote = do
  note <- choice $ map (try . string) B.noteAlias
  whiteSpace
  return . Value . fromJust $ note `elemIndex` B.noteAlias

pFigure :: Parser Sel
pFigure = do
  figure <- choice $ map (try . string) B.figures
  whiteSpace
  return . Value . (* 128) . succ . fromJust $ figure `elemIndex` B.figures

pReference :: Parser Sel
pReference = do
  n <- identifier
  notFollowedBy $ reservedOp B.definitionOp
  return $ Reference n

pSection :: Parser Sel
pSection = brackets pPrinciple >>= return . Section

pMulti :: Parser Sel
pMulti = braces pPrinciple >>= return . Multi

pCMulti :: Parser Sel
pCMulti = braces (many f) >>= return . CMulti
    where f = do
            c <- angles pPrinciple
            r <- pPrinciple
            return (Multi c, Multi r)

pExpression :: Parser Sel
pExpression = buildExpressionParser pOperators (parens pExpression <|> pElement)
              <?> "expression"

pOperators :: [[Operator String () Data.Functor.Identity.Identity Sel]]
pOperators =
    [[ Prefix (reservedOp B.reverseOp >> return Reverse ) ]
     , [ Infix (reservedOp B.productOp  >> return Product ) AssocLeft
       , Infix (reservedOp B.divisionOp >> return Division) AssocLeft
       , Infix (reservedOp B.sumOp      >> return Sum     ) AssocLeft
       , Infix (reservedOp B.diffOp     >> return Diff    ) AssocLeft
       , Infix (reservedOp B.loopOp     >> return Loop    ) AssocLeft
       , Infix (reservedOp B.rotationOp >> return Rotation) AssocLeft ]]

lang :: LanguageDef st
lang = emptyDef
       { Token.commentStart    = B.commentStart
       , Token.commentEnd      = B.commentEnd
       , Token.commentLine     = B.commentLine
       , Token.nestedComments  = True
       , Token.identStart      = letter
       , Token.identLetter     = alphaNum
       , Token.reservedNames   = B.noteAlias
       , Token.reservedOpNames = langOps
       , Token.caseSensitive   = True }

langOps :: [String]
langOps =
    [ B.productOp
    , B.divisionOp
    , B.sumOp
    , B.diffOp
    , B.loopOp
    , B.rotationOp
    , B.reverseOp
    , B.rangeOp
    , B.definitionOp ]

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
