module Ast
    ( Exp
        ( NumLit
        , StrLit
        , Identifier
        , FuncCall )
    , Args
    , Define (Define)
    ) where

import Data.List

data Exp = NumLit Int
         | StrLit String
         | Identifier String
         | FuncCall FuncName Args
instance Show Exp where
        show (NumLit x) = show x
        show (StrLit s) = "\"" ++ s ++ "\"@"
        show (Identifier s) = "(Identifier " ++ show s ++ ")"
        show (FuncCall n args) = "(FuncCall " ++ n ++ " [" ++ showArgs ++ "]" ++ ")"
             where
                 showArgs = intercalate ", " . (fmap show) $ args

type Args = [Exp]
type FuncName = String

data Define = Define String Exp
            deriving Show
