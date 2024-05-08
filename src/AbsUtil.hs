{-# LANGUAGE LambdaCase #-}

module AbsUtil where

import AbsSyntax
import Data.Function ((&))
import Data.Maybe (isJust)
import ParSyntax (myLexer, pListDec)
import Preprocess (transTree)
import AbsSyntax (BNFC'Position, HasPosition)

type Src = BNFC'Position

srcOf :: HasPosition a => a -> Src
srcOf = hasPosition

printPosition :: BNFC'Position -> String
printPosition = \case
  Just (line, col) -> "line " ++ show line ++ ", column " ++ show col
  Nothing -> "unknown position"

fromIdCap :: IdCap -> String
fromIdCap (IdCap id) = id

fromId :: Id -> String
fromId (Id id) = id

stringToDecs :: String -> Either String [Dec]
stringToDecs = fmap transTree . pListDec . myLexer

tTupElems :: [TTupElem] -> [Typ]
tTupElems = map (\(TTupJust _ t) -> t)

typLstToTyps :: TypLst -> [Typ]
typLstToTyps = \case
  TLEmpty _ -> []
  TLOne _ typ -> [typ]
  TLMany _ typ typs -> typ : typs

exnBindId :: ExnBind -> String
exnBindId = \case
  EBCon _ (IdCap id) -> id
  EBArg _ (IdCap id) _ -> id

exnBindTyp :: ExnBind -> Maybe Typ
exnBindTyp = \case
  EBCon _ _ -> Nothing
  EBArg _ _ typ -> Just typ

exnBindHasArg :: ExnBind -> Bool
exnBindHasArg x = exnBindTyp x & isJust

typBindTypLst :: TypBind -> TypLst 
typBindTypLst = \case
  TBJust _ typlst _ _ -> typlst

typBindId :: TypBind -> String
typBindId = \case
  TBJust _ _ (Id id) _ -> id

typBindDTags :: TypBind -> [DTag]
typBindDTags = \case
  TBJust _ _ _ dtags -> dtags

dtagId :: DTag -> String
dtagId = \case
  DTCon _ (IdCap id) -> id
  DTArg _ (IdCap id) _ -> id

dtagTyp :: DTag -> Maybe Typ
dtagTyp = \case
  DTCon _ _ -> Nothing
  DTArg _ _ typ -> Just typ

dtagHasArg :: DTag -> Bool
dtagHasArg x = dtagTyp x & isJust

typsOfTypLst :: TypLst -> [Typ]
typsOfTypLst = \case
  TLEmpty _ -> []
  TLOne _ typ -> [typ]
  TLMany _ typ typs -> typ : typs

letBindId :: LetBind -> Maybe String
letBindId = \case
  LBJust _ (Id id) _ -> Just id
  LBAnon _ _ -> Nothing

letBindExp :: LetBind -> Exp
letBindExp = \case
  LBJust _ _ exp -> exp
  LBAnon _ exp -> exp