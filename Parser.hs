module Parser (parser) where

import Prelude hiding (exp)
import Control.Applicative ((*>), (<*), (<**>), (<$>), (<*>), pure)
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

definition :: Parser Define
definition = pure Define <*> identifier <* spaces <* char '=' <* spaces <*> (funcCall <|> exp)

program :: Parser [Define]
program = sepBy definition $ skipMany1 newline

parser :: Parser [Define]
parser = program
