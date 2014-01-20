module Ast
    ( Exp (NumLit, StrLit, FuncCall)
    , Identifier
    , Args
    ) where

import Data.List

data Exp = NumLit Int
         | StrLit String
         | FuncCall Identifier Args
instance Show Exp where
        show (NumLit x) = show x
        show (StrLit s) = "\"" ++ s ++ "\"@"
        show (FuncCall n args) = "(FuncCall " ++ n ++ " [" ++ showArgs ++ "]" ++ ")"
             where
                 showArgs = intercalate ", " . (fmap show) $ args

type Identifier = String

type Args = [Exp]
