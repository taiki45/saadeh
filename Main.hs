import Ast
import Evaluator
import Parser

main :: IO ()
main = do s <- getContents
          let r = f s
              m = toMap r in
              putStrLn . show $ m

f :: String -> [Lambda]
f i = case readProgram i of
          Left e -> error . show $ e
          Right r -> r
