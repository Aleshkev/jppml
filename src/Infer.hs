{-# LANGUAGE LambdaCase #-}

module Infer where

import AbsSyntax (Id (Id), IdVar (IdVar), TTupElem' (TTupJust), Typ, Typ' (TFn, TId, TIdVar, TTup), TypLst' (TLEmpty, TLMany, TLOne))
import Control.Monad.Except (throwError)
import Data.Foldable (foldlM)
import Data.Function ((&))
import Data.List (group, intercalate, nub, sort)
import PrintSyntax (printTree)

data Type
  = RVar String
  | RTerm [Type] String

toAbsTyp :: Type -> Typ
toAbsTyp = \case
  RVar id -> TIdVar Nothing (IdVar id)
  RTerm (x : xs) "__tuple" -> TTup Nothing (toAbsTyp x) (map (TTupJust Nothing . toAbsTyp) xs)
  RTerm [a, b] "__fn" -> TFn Nothing (toAbsTyp a) (toAbsTyp b)
  RTerm [] id -> TId Nothing (TLEmpty Nothing) (Id id)
  RTerm [x] id -> TId Nothing (TLOne Nothing (toAbsTyp x)) (Id id)
  RTerm (x : xs) id -> TId Nothing (TLMany Nothing (toAbsTyp x) (map toAbsTyp xs)) (Id id)

instance Show Type where
  show = printTree . toAbsTyp

niceShowType :: Type -> String
niceShowType = show . simplifyVars

-- Collect all vars that occur in a type, from left to right.
collectVars :: Type -> [String]
collectVars = \case
  RVar x -> [x]
  RTerm ts _ -> ts >>= collectVars

niceVarNames :: [String]
niceVarNames = map (\x -> "'" ++ [x]) ['a' .. 'z']

simplifyVars :: Type -> Type
simplifyVars t = do
  let subs = zip (collectVars t & nub) (map RVar niceVarNames)
  substituteTypes subs t

substituteTypes :: [(String, Type)] -> Type -> Type
substituteTypes subs t =
  foldl
    ( \t (a, rept) -> case t of
        RVar x -> if x == a then rept else RVar x
        RTerm ts x -> RTerm (map (substituteTypes subs) ts) x
    )
    t
    (reverse subs)

-- Type unification algorithm from
-- https://www.cs.cornell.edu/courses/cs3110/2011sp/Lectures/lec26-type-inference/type-inference.htm
unifyTypePair :: Type -> Type -> Either String [(String, Type)]
unifyTypePair a b = do
  case (a, b) of
    (RVar idA, RVar idB) -> return [(idB, a) | idA /= idB]
    (RTerm depsA idA, RTerm depsB idB) -> do
      if length depsA == length depsB && idA == idB
        then unifyAll (zip depsA depsB)
        else throwError $ "expression has type " ++ show b ++ " incompatible with type " ++ show a
    (RVar id, term) -> idTerm id term
    (term, RVar id) -> idTerm id term
 where
  idTerm :: String -> Type -> Either String [(String, Type)]
  idTerm id term =
    if occurs id term then throwError $ "type " ++ id ++ " is cyclic" else return [(id, term)]

  occurs varId term = case term of
    RVar x -> x == varId
    RTerm xs _ -> any (occurs varId) xs

unifyAll :: [(Type, Type)] -> Either String [(String, Type)]
unifyAll =
  foldlM
    ( \acc (a, b) -> do
        ps <- unifyTypePair (substituteTypes acc a) (substituteTypes acc b)
        return $ ps ++ acc
    )
    []
