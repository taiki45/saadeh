module Ast
    ( Expr
        ( NumLit -- TODO: Number
        , StrLit
        , Identifier
        , FuncCall )
    , Args
    , Params
    , Lambda (Lambda)
    ) where

import Data.List

data Expr = NumLit Int
          | StrLit String
          | Identifier String
          | FuncCall FuncName Args
          deriving Eq
instance Show Expr where
        show (NumLit x) = show x
        show (StrLit s) = "\"" ++ s ++ "\"@"
        show (Identifier s) = "(Identifier " ++ show s ++ ")"
        show (FuncCall n args) = "(FuncCall " ++ n ++ " [" ++ showArgs ++ "]" ++ ")"
             where showArgs = intercalate ", " . (fmap show) $ args

type Args = [Expr]
type FuncName = String

data Lambda = Lambda Name Params Expr
            deriving Show

type Name = String
type Params = [String]
