module Parser (readProgram) where

import Prelude hiding (exp)
import Control.Applicative ((*>), (<*), (<**>), (<$>), (<$), (<*>), pure)
import Text.ParserCombinators.Parsec
import Ast

numberLit :: Parser Expr
numberLit = many1 digit <**> (pure $ NumLit . read)

strLit :: Parser Expr
strLit = StrLit <$> between quote quote (many $ noneOf "\"")
                where quote = (char '"')

literal :: Parser Expr
literal = numberLit <|> strLit

symbol :: Parser Char
symbol = oneOf "!$%^&*-_+|?<>:"

identifier :: Parser String
identifier = pure (++) <*> pure <$> letter <*> many (alphaNum <|> symbol)

funcCall :: Parser Expr
funcCall = pure FuncCall <*> identifier <*> option [] (parseArgs <|> parseArgs')
            where parseArgs  = try $ spaces *> sepBy1 exp space
                  parseArgs' = try $ manyTill (space *> exp) (try $ lookAhead $ spaces *> identifier <* spaces <* char '=')

exp :: Parser Expr
exp = literal <|> Identifier <$> identifier

definition :: Parser Lambda
definition = pure Lambda
                <*> identifier
                <*> (try parseParams <|> (spaces *> equal))
                <*> (funcCall <|> exp)
             where parseParams = manyTill (spaces *> identifier <* spaces) equal
                   equal = try $ [] <$ (char '=' <* spaces)

program :: Parser [Lambda]
program = sepBy definition $ skipMany1 newline

readProgram :: String -> Either ParseError [Lambda]
readProgram i = parse program "Saadeh" i
