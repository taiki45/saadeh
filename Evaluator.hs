module Evaluator (start) where

import qualified Data.Map as M
import Ast

primEnv :: M.Map String Lambda
primEnv = M.fromList primitives

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
eval (Lambda _ _ e) m = case e of
                             a@(Number _) -> return a
                             a@(StrLit _) -> return a
                             (FuncCall i as) -> let pd = findDef i primEnv
                                                    md = findDef i m
                                                    in case pd of
                                                        (Just d) -> applyPrim d as
                                                        Nothing -> do d <- md
                                                                      r <- apply d as
                                                                      eval (Lambda "" [] r) m

apply :: Lambda -> [Expr] -> Maybe Expr
apply (Lambda _ [] e) [] = return e
apply (Lambda _ _ e@(Number _)) _ = return e
apply (Lambda _ _ e@(StrLit _)) _ = return e
apply (Lambda n (p:ps) e@(FuncCall i [])) (a:as) = if p == i
                                                    then return a
                                                    else apply (Lambda n ps e) as
apply (Lambda n param (FuncCall f arg)) arg' =
        apply (Lambda n [] (FuncCall (replaceFunc f) (fmap replace arg))) []
            where replace e@(Identifier i) = if elem i param
                                                 then (snd . head) . filter ((== i) . fst) $ zip param arg'
                                                 else e
                  replace e = e
                  replaceFunc name = if elem name param
                                         then let l = zip param arg'
                                                  in (takeName . snd . head) . filter ((== name) . fst) $ l
                                         else name
                  takeName (Identifier i) = i

applyPrim :: Lambda -> [Expr] -> Maybe Expr
applyPrim = undefined

primitives :: [(String, Lambda)]
primitives = []
