module Evaluator (start) where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Ast

-- @test
-- TODO: funccall with Identifier
--       FuncCall Expr Expr
--       if Expr is non Funcation in apply,
--          raise an error.
testLambdas :: [Lambda]
testLambdas = [(Lambda "main" [] (FuncCall "f" [(Identifier "g"), (NumLit 3)]))
              ,(Lambda "f" ["a", "x"] (FuncCall "a" [(Identifier "x")]))
              ,(Lambda "g" ["b"] (Identifier "b"))
              ]

-- @test
numLits :: [Expr]
numLits = (NumLit 3):numLits

primEnv :: M.Map String Lambda
primEnv = M.fromList []

toMap :: [Lambda] -> M.Map String Lambda
toMap = M.fromList . (fmap f)
        where f d@(Lambda s _ _) = (s, d)

start :: [Lambda] -> Maybe Expr
start ds = let m = toMap ds
               mDef = findDef "main" m in
                  do d <- mDef
                     eval d m

findDef :: String -> M.Map String Lambda -> Maybe Lambda
findDef s m = M.lookup s m

eval :: Lambda -> M.Map String Lambda -> Maybe Expr
eval (Lambda _ ps e) m = case e of
                             a@(NumLit _) -> return a
                             a@(StrLit _) -> return a
                             (Identifier i) -> let md = findDef i m
                                                   in md >>= (\d ->
                                                        eval d m)
                             (FuncCall i as) -> let pd = findDef i primEnv
                                                    md = findDef i m
                                                    in case pd of
                                                        (Just d) -> applyPrim d as
                                                        Nothing -> do d <- md
                                                                      r <- apply d as
                                                                      eval (Lambda "" [] r) m

apply :: Lambda -> [Expr] -> Maybe Expr
apply (Lambda _ [] e) [] = return e
apply (Lambda _ _ e@(NumLit _)) _ = return e
apply (Lambda _ _ e@(StrLit _)) _ = return e
apply (Lambda n param@(p:ps) e@(Identifier i)) arg@(a:as) = if p == i
                                                                then return a
                                                                else apply (Lambda n ps e) as
apply (Lambda n _ e@(FuncCall f [])) _ = return e
apply (Lambda n param (FuncCall f arg)) arg' =
        apply (Lambda n [] (FuncCall f (fmap replace arg))) []
            where replace e@(Identifier i) = if elem i param
                                                 then let l = getZipList $
                                                                pure (,)
                                                                <*> ZipList param
                                                                <*> ZipList arg'
                                                        in (snd . head) . filter ((== i) . fst) $ l
                                                 else e
                  replace e = e

applyPrim :: Lambda -> [Expr] -> Maybe Expr
applyPrim = undefined
