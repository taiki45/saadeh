module Ast
    ( Expr
        ( Number
        , String
        , Identifier
        , FuncCall )
    , Args
    , Params
    , Lambda (Lambda)
    , isLiteral
    ) where

import Data.List

data Expr = Number Int
          | String String
          | Identifier String
          | FuncCall FuncName Args
          deriving Eq
instance Show Expr where
        show (Number x) = show x
        show (String s) = "\"" ++ s ++ "\"@"
        show (Identifier s) = "(Identifier " ++ show s ++ ")"
        show (FuncCall n args) = "(FuncCall " ++ n ++ " [" ++ showArgs ++ "]" ++ ")"
             where showArgs = intercalate ", " . (fmap show) $ args

isLiteral :: Expr -> Bool
isLiteral (Number _) = True
isLiteral (String _) = True
isLiteral _ = False

type Args = [Expr]
type FuncName = String

data Lambda = Lambda Name Params Expr
            | Primitive Name Body
instance Show Lambda where
        show (Lambda s ps e) = intercalate " " ["(Lambda", s, show ps, show e, ")"]
        show (Primitive s _) = "(Primitive " ++ s ++ ")"

type Name = String
type Params = [String]
type Body = [Expr] -> Expr
