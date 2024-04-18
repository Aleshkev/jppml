-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Jppml.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Jppml.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transId :: Jppml.Abs.Id -> Result
transId x = case x of
  Jppml.Abs.Id string -> failure x

transIdCap :: Jppml.Abs.IdCap -> Result
transIdCap x = case x of
  Jppml.Abs.IdCap string -> failure x

transIdVar :: Jppml.Abs.IdVar -> Result
transIdVar x = case x of
  Jppml.Abs.IdVar string -> failure x

transCon :: Show a => Jppml.Abs.Con' a -> Result
transCon x = case x of
  Jppml.Abs.CInt _ integer -> failure x
  Jppml.Abs.CString _ string -> failure x
  Jppml.Abs.CUnit _ -> failure x

transExp :: Show a => Jppml.Abs.Exp' a -> Result
transExp x = case x of
  Jppml.Abs.ECon _ con -> failure x
  Jppml.Abs.EObjCon _ idcap -> failure x
  Jppml.Abs.EId _ id -> failure x
  Jppml.Abs.ETup _ exp exps -> failure x
  Jppml.Abs.ELst _ exps -> failure x
  Jppml.Abs.EApp _ exp1 exp2 -> failure x
  Jppml.Abs.ENeg _ exp -> failure x
  Jppml.Abs.EMul _ exp1 exp2 -> failure x
  Jppml.Abs.EDiv _ exp1 exp2 -> failure x
  Jppml.Abs.EAdd _ exp1 exp2 -> failure x
  Jppml.Abs.ESub _ exp1 exp2 -> failure x
  Jppml.Abs.ECons _ exp1 exp2 -> failure x
  Jppml.Abs.EAppend _ exp1 exp2 -> failure x
  Jppml.Abs.ECat _ exp1 exp2 -> failure x
  Jppml.Abs.ERel _ exp1 erelop exp2 -> failure x
  Jppml.Abs.EAnd _ exp1 exp2 -> failure x
  Jppml.Abs.EOr _ exp1 exp2 -> failure x
  Jppml.Abs.EIf _ exp1 exp2 exp3 -> failure x
  Jppml.Abs.ELet _ letbinds exp -> failure x
  Jppml.Abs.ECase _ exp ecasebinds -> failure x
  Jppml.Abs.EFn _ ids exp -> failure x

transERelOp :: Show a => Jppml.Abs.ERelOp' a -> Result
transERelOp x = case x of
  Jppml.Abs.EREq _ -> failure x
  Jppml.Abs.ERNe _ -> failure x
  Jppml.Abs.ERLt _ -> failure x
  Jppml.Abs.ERLe _ -> failure x
  Jppml.Abs.ERGt _ -> failure x
  Jppml.Abs.ERGe _ -> failure x

transECaseBind :: Show a => Jppml.Abs.ECaseBind' a -> Result
transECaseBind x = case x of
  Jppml.Abs.ECBJust _ pat exp -> failure x

transPat :: Show a => Jppml.Abs.Pat' a -> Result
transPat x = case x of
  Jppml.Abs.PCon _ con -> failure x
  Jppml.Abs.PId _ id -> failure x
  Jppml.Abs.PWild _ -> failure x
  Jppml.Abs.PTup _ pat pats -> failure x
  Jppml.Abs.PLst _ pats -> failure x
  Jppml.Abs.PObjCon _ idcap -> failure x
  Jppml.Abs.PObj _ idcap pat -> failure x
  Jppml.Abs.PCons _ pat1 pat2 -> failure x

transTyp :: Show a => Jppml.Abs.Typ' a -> Result
transTyp x = case x of
  Jppml.Abs.TIdVar _ idvar -> failure x
  Jppml.Abs.TId _ typlst id -> failure x
  Jppml.Abs.TTup _ typ ttupelems -> failure x
  Jppml.Abs.TFn _ typ1 typ2 -> failure x

transTypLst :: Show a => Jppml.Abs.TypLst' a -> Result
transTypLst x = case x of
  Jppml.Abs.TLEmpty _ -> failure x
  Jppml.Abs.TLOne _ typ -> failure x
  Jppml.Abs.TLMany _ typ typs -> failure x

transTTupElem :: Show a => Jppml.Abs.TTupElem' a -> Result
transTTupElem x = case x of
  Jppml.Abs.TTupJust _ typ -> failure x

transDec :: Show a => Jppml.Abs.Dec' a -> Result
transDec x = case x of
  Jppml.Abs.DLet _ letbinds -> failure x
  Jppml.Abs.DType _ typbinds -> failure x
  Jppml.Abs.DExn _ exnbinds -> failure x
  Jppml.Abs.DOpen _ idcaps -> failure x

transLetBind :: Show a => Jppml.Abs.LetBind' a -> Result
transLetBind x = case x of
  Jppml.Abs.LBJust _ id exp -> failure x
  Jppml.Abs.LBAnon _ exp -> failure x

transTypBind :: Show a => Jppml.Abs.TypBind' a -> Result
transTypBind x = case x of
  Jppml.Abs.TBJust _ typlst id dtags -> failure x

transDTag :: Show a => Jppml.Abs.DTag' a -> Result
transDTag x = case x of
  Jppml.Abs.DTCon _ idcap -> failure x
  Jppml.Abs.DTArg _ idcap typ -> failure x

transExnBind :: Show a => Jppml.Abs.ExnBind' a -> Result
transExnBind x = case x of
  Jppml.Abs.EBCon _ idcap -> failure x
  Jppml.Abs.EBArg _ idcap typ -> failure x
