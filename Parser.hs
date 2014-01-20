module Parser
    () where

import Prelude hiding (exp)
import Text.ParserCombinators.Parsec
import Ast

--TODO: use <$ and $>
--TODO: remove parenth: puts exp exp exp

numberLit :: Parser Exp
numberLit = many1 digit >>= return . NumLit . read

strLit :: Parser Exp
strLit = between quote quote (many $ noneOf "\"")
            >>= return . StrLit
                where quote = (char '"')

literal :: Parser Exp
literal = numberLit <|> strLit

symbol :: Parser Char
symbol = oneOf "!$%^&*-_+|?<>:"

identifier :: Parser Identifier
identifier = do c <- letter
                rest <- many $ alphaNum <|> symbol
                return $ [c] ++ rest

funcCall :: Parser Exp
funcCall = do name <- identifier
              args <- option [] parseArgs
              return $ FuncCall name args
                where open = char '('
                      close = char ')'
                      parseArgs = try (do spaces
                                          a <- between open close $ sepBy1 exp spaces
                                          spaces
                                          return a)

exp :: Parser Exp
exp = do optional spaces
         e <- literal <|> funcCall
         optional spaces
         return e
