module Preprocess where

import ParSyntax
import LexSyntax


import qualified Data.Map as Map
import Data.Map((!))
import GHC.Show (Show)
import Prelude (($), Either(..), String, (++), Show, show, Int, Ord, Eq, Integer, fail, map, error, id, reverse)
import AbsSyntax
import Data.Ord (Ord)



transTree :: [Dec] -> [Dec]
transTree decs =
  map transDec decs


transExp :: Show a => Exp' a -> Exp' a 
transExp x = case x of
  ECon p con -> ECon p con
  EObjCon p idcap -> EObjCon p idcap
  EId p id -> EId p id
  ETup p exp exps -> ETup p (transExp exp) (map transExp exps)
  ELst p exps -> case exps of 
    [] -> EObjCon p (IdCap "__Empty")
    exp : exps_ -> transExp $ ECons p (transExp exp) (transExp $ ELst p exps_)
  EApp p exp1 exp2 -> EApp p (transExp exp1) (transExp exp2)
  ENeg p exp -> delegate p "__neg" [exp]
  -- EMul p exp1 exp2 -> delegate p "__mul" [exp1, exp2]
  -- EDiv p exp1 exp2 -> delegate p "__div" [exp1, exp2]
  -- EAdd p exp1 exp2 -> delegate p "__add" [exp1, exp2]
  -- ESub p exp1 exp2 -> delegate p "__sub" [exp1, exp2]
  ECons p exp1 exp2 -> delegate p "__cons" [exp1, exp2]
  EAppend p exp1 exp2 -> delegate p "__append" [exp1, exp2]
  -- ECat p exp1 exp2 -> delegate p "__cat" [exp1, exp2]
  ERel p exp1 erelop exp2 -> ERel p (transExp exp1) erelop (transExp exp2)
  -- ERel p exp1 erelop exp2 -> case erelop of
  --   EREq _ -> delegate p "__eq" [exp1, exp2]
  --   ERNe _ -> delegate p "__ne" [exp1, exp2]
  --   ERLt _ -> delegate p "__lt" [exp1, exp2]
  --   ERLe _ -> delegate p "__le" [exp1, exp2]
  --   ERGt _ -> delegate p "__gt" [exp1, exp2]
  --   ERGe _ -> delegate p "__ge" [exp1, exp2]
  EAnd p exp1 exp2 -> delegate p "__and" [exp1, EFn p [Id "__x"] exp2]
  EOr p exp1 exp2 -> delegate p "__or" [exp1, EFn p [Id "__x"] exp2]
  EIf p exp1 exp2 exp3 -> EIf p (transExp exp1) (transExp exp2) (transExp exp3)
  ELet p letbinds exp -> ELet p (map transLetBind letbinds) (transExp exp)
  ECase p exp ecasebinds -> ECase p (transExp exp) (map transECaseBind ecasebinds)
  EFn p ids exp -> case ids of
    [] -> error "a function must have at least one argument"
    [id] -> EFn p [id] (transExp exp)
    id : ids_ -> EFn p [id] (transExp $ EFn p ids_ exp)
  x -> x
  where
    delegate :: Show a => a -> String -> [Exp' a] -> Exp' a
    delegate p fname exps = 
      case reverse exps of 
      [] -> EId p (Id fname)
      exp : exps_ -> EApp p (delegate p fname (reverse exps_)) (transExp exp) 

transECaseBind :: Show a => ECaseBind' a -> ECaseBind' a
transECaseBind x = case x of
  ECBJust p pat exp -> ECBJust p (transPat pat) (transExp exp)

transPat :: Show a => Pat' a -> Pat' a
transPat x = case x of
  -- PCon p con -> 
  -- PId _ id -> failure x
  -- PWild _ -> failure x
  -- PTup p pat pats -> failure x
  PLst p pats -> case pats of
    [] -> PObjCon p (IdCap "__Empty")
    pat : pats_ -> transPat $ PCons p pat (PLst p pats_)
  -- PObjCon _ idcap -> failure x
  -- PObj _ idcap pat -> failure x
  PCons p pat1 pat2 -> PObj p (IdCap "__Cons") (PTup p (transPat pat1) [transPat pat2])
  x -> x

transTyp :: Show a => Typ' a -> Typ' a
transTyp x = case x of
  -- TIdVar _ idvar -> failure x
  -- TId p typlst id -> TId p (transTypLst typlst) id
  -- TTup p typ ttupelems -> TTup p 
  -- TFn p typ1 typ2 -> failure x
  x -> x

transTypLst :: Show a => TypLst' a -> TypLst' a
transTypLst = id
-- transTypLst x = case x of
--   TLEmpty p -> transTypLst $ TLMany p (Typ' a) ([Typ' a])
--   TLOne _ typ -> failure x
--   TLMany p typ typs -> TLMany p (transTyp typ) (map transTyp typs)

transTTupElem :: Show a => TTupElem' a -> TTupElem' a
transTTupElem x = case x of
  TTupJust p typ -> TTupJust p (transTyp typ)

transDec :: Show a => Dec' a -> Dec' a
transDec x = case x of
  DLet p letbinds -> DLet p (map transLetBind letbinds)
  DType p typbinds -> DType p (map transTypBind typbinds)
  -- DExn p exnbinds -> DExn p (map transExnBind exnbinds)
  -- DExn p exnbinds -> error "Not implemented"
  DOpen p idcaps -> DOpen p idcaps
  x -> x

transLetBind :: Show a => LetBind' a -> LetBind' a
transLetBind x = case x of
  LBJust p id exp -> LBJust p id (transExp exp)
  LBAnon p exp -> transLetBind $ LBJust p (Id "__x") exp

transTypBind :: Show a => TypBind' a -> TypBind' a
transTypBind x = case x of
  TBJust p typlst id dtags -> TBJust p (transTypLst typlst) id (map transDTag dtags)

transDTag :: Show a => DTag' a -> DTag' a
transDTag x = case x of
  DTCon p idcap -> DTCon p idcap
  DTArg p idcap typ -> DTArg p idcap (transTyp typ)

-- transExnBind :: Show a => ExnBind' a -> ExnBind' a
-- transExnBind x = case x of
--   EBCon _ idcap -> failure x
--   EBArg _ idcap typ -> failure x