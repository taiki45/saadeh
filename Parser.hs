module Parser
    () where

import Prelude hiding (exp)
import Control.Applicative ((*>), (<*), (<**>), (<$>), (<*>), pure)
import Text.ParserCombinators.Parsec
import Ast

--TODO: remove parenth: puts exp exp exp

numberLit :: Parser Exp
numberLit = many1 digit <**> (pure $ NumLit . read)

strLit :: Parser Exp
strLit = StrLit <$> between quote quote (many $ noneOf "\"")
                where quote = (char '"')

literal :: Parser Exp
literal = numberLit <|> strLit

symbol :: Parser Char
symbol = oneOf "!$%^&*-_+|?<>:"

identifier :: Parser String
identifier = pure (++) <*> pure <$> letter <*> many (alphaNum <|> symbol)

definition :: Parser Exp
definition = pure Define <*> identifier <* spaces <* char '=' <* spaces <*> exp

funcCall :: Parser Exp
funcCall = pure FuncCall <*> identifier <*> option [] parseArgs
                where parseArgs = try $ spaces *> sepBy1 exp spaces <* spaces

exp :: Parser Exp
exp = literal <|> Identifier <$> identifier <|> funcCall
