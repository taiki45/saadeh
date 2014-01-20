module Parser
    () where

import Prelude hiding (exp)
import Control.Applicative ((*>), (<*), (<**>), (<$>), (<*>), pure)
import Text.ParserCombinators.Parsec
import Ast

--TODO: remove parenth: puts exp exp exp

numberLit :: Parser Exp
numberLit =  many1 digit <**> (pure $ NumLit . read)

strLit :: Parser Exp
strLit = StrLit <$> between quote quote (many $ noneOf "\"")
                where quote = (char '"')

literal :: Parser Exp
literal = numberLit <|> strLit

symbol :: Parser Char
symbol = oneOf "!$%^&*-_+|?<>:"

identifier :: Parser Identifier
identifier = pure (++) <*> pure <$> letter <*> many (alphaNum <|> symbol)

funcCall :: Parser Exp
funcCall = pure FuncCall <*> identifier <*> option [] parseArgs
                where open = char '('
                      close = char ')'
                      parseArgs = try $ spaces *> between open close (sepBy1 exp spaces) <* spaces

exp :: Parser Exp
exp = ignoreSpaces *> literal <|> funcCall <* ignoreSpaces
        where ignoreSpaces = optional spaces
