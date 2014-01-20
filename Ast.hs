module Ast
    ( Exp
        ( NumLit
        , StrLit
        , Identifier
        , Define
        , FuncCall )
    , Args
    ) where

import Data.List

data Exp = NumLit Int
         | StrLit String
         | Identifier String
         | Define String Exp
         | FuncCall FuncName Args
instance Show Exp where
        show (NumLit x) = show x
        show (StrLit s) = "\"" ++ s ++ "\"@"
        show (Identifier s) = "(Identifier " ++ show s ++ ")"
        show (Define s e) = "(Define " ++ s ++ " = " ++ show e ++ ")"
        show (FuncCall n args) = "(FuncCall " ++ n ++ " [" ++ showArgs ++ "]" ++ ")"
             where
                 showArgs = intercalate ", " . (fmap show) $ args

type Args = [Exp]
type FuncName = String
