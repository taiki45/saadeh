import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Ast
import Evaluator
import Parser

main :: IO ()
main = do fpath <- head <$> getArgs
          code <- readFile fpath
          let defines = f code
              result = start defines
              in putStrLn . show $ result

f :: String -> [Lambda]
f i = case readProgram i of
          Left e -> error . show $ e
          Right r -> r
