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
    exp : exps' -> transExp $ ECons p (transExp exp) (transExp $ ELst p exps')
  EApp p exp1 exp2 -> EApp p (transExp exp1) (transExp exp2)
  ENeg p exp -> repWithFn p "__neg" [exp]
  EMul p exp1 exp2 -> repWithFn p "__mul" [exp1, exp2]
  EDiv p exp1 exp2 -> repWithFn p "__div" [exp1, exp2]
  EAdd p exp1 exp2 -> repWithFn p "__add" [exp1, exp2]
  ESub p exp1 exp2 -> repWithFn p "__sub" [exp1, exp2]
  ECons p exp1 exp2 -> repWithFn p "__cons" [exp1, exp2]
  EAppend p exp1 exp2 -> repWithFn p "__append" [exp1, exp2]
  ECat p exp1 exp2 -> repWithFn p "__cat" [exp1, exp2]
  ERel p exp1 erelop exp2 -> do 
    let op = case erelop of
          EREq _ -> "__eq"
          ERNe _ -> "__ne"
          ERLt _ -> "__lt"
          ERLe _ -> "__le"
          ERGt _ -> "__gt"
          ERGe _ -> "__ge"
    repWithFn p op [exp1, exp2]
  EAnd p exp1 exp2 -> repWithFn p "__and" [exp1, EFn p [Id "__x"] exp2]
  EOr p exp1 exp2 -> repWithFn p "__or" [exp1, EFn p [Id "__x"] exp2]
  EIf p exp1 exp2 exp3 -> repWithFn p "__if" [exp1, EFn p [Id "__x"] exp2, EFn p [Id "__x"] exp3]
  ELet p letbinds exp -> ELet p (map transLetBind letbinds) (transExp exp)
  ECase p exp ecasebinds -> ECase p (transExp exp) (map transECaseBind ecasebinds)
  EFn p ids exp -> case ids of
    [] -> error "a function must have at least one argument"
    [id] -> EFn p [id] (transExp exp)
    id : ids_ -> EFn p [id] (transExp $ EFn p ids_ exp)
  where
    repWithFn :: Show a => a -> String -> [Exp' a] -> Exp' a
    repWithFn p fname exps = 
      case reverse exps of 
      [] -> EId p (Id fname)
      exp : exps_ -> EApp p (repWithFn p fname (reverse exps_)) (transExp exp) 

transECaseBind :: Show a => ECaseBind' a -> ECaseBind' a
transECaseBind x = case x of
  ECBJust p pat exp -> ECBJust p (transPat pat) (transExp exp)

transPat :: Show a => Pat' a -> Pat' a
transPat x = case x of
  PCon p con -> PCon p con
  PId p id -> PId p id
  PWild p -> PWild p
  PTup p pat pats -> PTup p (transPat pat) (map transPat pats)
  PLst p pats -> case pats of
    [] -> PObjCon p (IdCap "__Empty")
    pat : pats_ -> transPat $ PCons p pat (PLst p pats_)
  PObjCon p idcap -> PObjCon p idcap
  PObj p idcap pat -> PObj p idcap (transPat pat)
  PCons p pat1 pat2 -> PObj p (IdCap "__Cons") (PTup p (transPat pat1) [transPat pat2])

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