-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Jppml.

module Jppml.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Jppml.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Jppml.Abs.Id where
  prt _ (Jppml.Abs.Id i) = doc $ showString i
instance Print Jppml.Abs.IdCap where
  prt _ (Jppml.Abs.IdCap i) = doc $ showString i
instance Print Jppml.Abs.IdVar where
  prt _ (Jppml.Abs.IdVar i) = doc $ showString i
instance Print (Jppml.Abs.Con' a) where
  prt i = \case
    Jppml.Abs.CInt _ n -> prPrec i 0 (concatD [prt 0 n])
    Jppml.Abs.CString _ str -> prPrec i 0 (concatD [printString str])
    Jppml.Abs.CUnit _ -> prPrec i 0 (concatD [doc (showString "("), doc (showString ")")])

instance Print (Jppml.Abs.Exp' a) where
  prt i = \case
    Jppml.Abs.ECon _ con -> prPrec i 11 (concatD [prt 0 con])
    Jppml.Abs.EObjCon _ idcap -> prPrec i 11 (concatD [prt 0 idcap])
    Jppml.Abs.EId _ id_ -> prPrec i 11 (concatD [prt 0 id_])
    Jppml.Abs.ETup _ exp exps -> prPrec i 11 (concatD [doc (showString "("), prt 0 exp, doc (showString ","), prt 0 exps, doc (showString ")")])
    Jppml.Abs.ELst _ exps -> prPrec i 11 (concatD [doc (showString "["), prt 0 exps, doc (showString "]")])
    Jppml.Abs.EApp _ exp1 exp2 -> prPrec i 10 (concatD [prt 10 exp1, prt 11 exp2])
    Jppml.Abs.ENeg _ exp -> prPrec i 9 (concatD [doc (showString "-"), prt 10 exp])
    Jppml.Abs.EMul _ exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "*"), prt 9 exp2])
    Jppml.Abs.EDiv _ exp1 exp2 -> prPrec i 8 (concatD [prt 8 exp1, doc (showString "/"), prt 9 exp2])
    Jppml.Abs.EAdd _ exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "+"), prt 8 exp2])
    Jppml.Abs.ESub _ exp1 exp2 -> prPrec i 7 (concatD [prt 7 exp1, doc (showString "-"), prt 8 exp2])
    Jppml.Abs.ECons _ exp1 exp2 -> prPrec i 6 (concatD [prt 7 exp1, doc (showString "::"), prt 6 exp2])
    Jppml.Abs.EAppend _ exp1 exp2 -> prPrec i 5 (concatD [prt 6 exp1, doc (showString "@"), prt 5 exp2])
    Jppml.Abs.ECat _ exp1 exp2 -> prPrec i 5 (concatD [prt 6 exp1, doc (showString "^"), prt 5 exp2])
    Jppml.Abs.ERel _ exp1 erelop exp2 -> prPrec i 4 (concatD [prt 5 exp1, prt 0 erelop, prt 5 exp2])
    Jppml.Abs.EAnd _ exp1 exp2 -> prPrec i 3 (concatD [prt 3 exp1, doc (showString "&&"), prt 4 exp2])
    Jppml.Abs.EOr _ exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "||"), prt 3 exp2])
    Jppml.Abs.EIf _ exp1 exp2 exp3 -> prPrec i 1 (concatD [doc (showString "if"), prt 0 exp1, doc (showString "then"), prt 0 exp2, doc (showString "else"), prt 1 exp3])
    Jppml.Abs.ELet _ letbinds exp -> prPrec i 0 (concatD [doc (showString "let"), prt 0 letbinds, doc (showString "in"), prt 0 exp])
    Jppml.Abs.ECase _ exp ecasebinds -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp, doc (showString "of"), prt 0 ecasebinds])
    Jppml.Abs.EFn _ ids exp -> prPrec i 0 (concatD [doc (showString "fn"), prt 0 ids, doc (showString "->"), prt 0 exp])

instance Print [Jppml.Abs.Exp' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Jppml.Abs.ERelOp' a) where
  prt i = \case
    Jppml.Abs.EREq _ -> prPrec i 0 (concatD [doc (showString "==")])
    Jppml.Abs.ERNe _ -> prPrec i 0 (concatD [doc (showString "!=")])
    Jppml.Abs.ERLt _ -> prPrec i 0 (concatD [doc (showString "<")])
    Jppml.Abs.ERLe _ -> prPrec i 0 (concatD [doc (showString "<=")])
    Jppml.Abs.ERGt _ -> prPrec i 0 (concatD [doc (showString ">")])
    Jppml.Abs.ERGe _ -> prPrec i 0 (concatD [doc (showString ">=")])

instance Print (Jppml.Abs.ECaseBind' a) where
  prt i = \case
    Jppml.Abs.ECBJust _ pat exp -> prPrec i 0 (concatD [prt 0 pat, doc (showString "->"), prt 0 exp])

instance Print [Jppml.Abs.ECaseBind' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print [Jppml.Abs.Id] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Jppml.Abs.Pat' a) where
  prt i = \case
    Jppml.Abs.PCon _ con -> prPrec i 3 (concatD [prt 0 con])
    Jppml.Abs.PId _ id_ -> prPrec i 3 (concatD [prt 0 id_])
    Jppml.Abs.PWild _ -> prPrec i 3 (concatD [doc (showString "_")])
    Jppml.Abs.PTup _ pat pats -> prPrec i 3 (concatD [doc (showString "("), prt 0 pat, doc (showString ","), prt 0 pats, doc (showString ")")])
    Jppml.Abs.PLst _ pats -> prPrec i 3 (concatD [doc (showString "["), prt 0 pats, doc (showString "]")])
    Jppml.Abs.PObjCon _ idcap -> prPrec i 3 (concatD [prt 0 idcap])
    Jppml.Abs.PObj _ idcap pat -> prPrec i 1 (concatD [prt 0 idcap, prt 2 pat])
    Jppml.Abs.PCons _ pat1 pat2 -> prPrec i 0 (concatD [prt 1 pat1, doc (showString "::"), prt 0 pat2])

instance Print [Jppml.Abs.Pat' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Jppml.Abs.Typ' a) where
  prt i = \case
    Jppml.Abs.TIdVar _ idvar -> prPrec i 3 (concatD [prt 0 idvar])
    Jppml.Abs.TId _ typlst id_ -> prPrec i 3 (concatD [prt 0 typlst, prt 0 id_])
    Jppml.Abs.TTup _ typ ttupelems -> prPrec i 1 (concatD [prt 2 typ, doc (showString "*"), prt 0 ttupelems])
    Jppml.Abs.TFn _ typ1 typ2 -> prPrec i 0 (concatD [prt 1 typ1, doc (showString "->"), prt 0 typ2])

instance Print (Jppml.Abs.TypLst' a) where
  prt i = \case
    Jppml.Abs.TLEmpty _ -> prPrec i 0 (concatD [])
    Jppml.Abs.TLOne _ typ -> prPrec i 0 (concatD [prt 3 typ])
    Jppml.Abs.TLMany _ typ typs -> prPrec i 0 (concatD [doc (showString "("), prt 0 typ, doc (showString ","), prt 0 typs, doc (showString ")")])

instance Print [Jppml.Abs.Typ' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Jppml.Abs.TTupElem' a) where
  prt i = \case
    Jppml.Abs.TTupJust _ typ -> prPrec i 0 (concatD [prt 2 typ])

instance Print [Jppml.Abs.TTupElem' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "*"), prt 0 xs]

instance Print (Jppml.Abs.Dec' a) where
  prt i = \case
    Jppml.Abs.DLet _ letbinds -> prPrec i 0 (concatD [doc (showString "let"), prt 0 letbinds])
    Jppml.Abs.DType _ typbinds -> prPrec i 0 (concatD [doc (showString "type"), prt 0 typbinds])
    Jppml.Abs.DExn _ exnbinds -> prPrec i 0 (concatD [doc (showString "exception"), prt 0 exnbinds])
    Jppml.Abs.DOpen _ idcaps -> prPrec i 0 (concatD [doc (showString "open"), prt 0 idcaps])

instance Print [Jppml.Abs.IdCap] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]

instance Print [Jppml.Abs.Dec' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Jppml.Abs.LetBind' a) where
  prt i = \case
    Jppml.Abs.LBJust _ id_ exp -> prPrec i 0 (concatD [prt 0 id_, doc (showString "="), prt 0 exp])
    Jppml.Abs.LBAnon _ exp -> prPrec i 0 (concatD [doc (showString "_"), doc (showString "="), prt 0 exp])

instance Print [Jppml.Abs.LetBind' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]

instance Print (Jppml.Abs.TypBind' a) where
  prt i = \case
    Jppml.Abs.TBJust _ typlst id_ dtags -> prPrec i 0 (concatD [prt 0 typlst, prt 0 id_, doc (showString "="), prt 0 dtags])

instance Print [Jppml.Abs.TypBind' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]

instance Print (Jppml.Abs.DTag' a) where
  prt i = \case
    Jppml.Abs.DTCon _ idcap -> prPrec i 0 (concatD [prt 0 idcap])
    Jppml.Abs.DTArg _ idcap typ -> prPrec i 0 (concatD [prt 0 idcap, doc (showString "of"), prt 0 typ])

instance Print [Jppml.Abs.DTag' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print (Jppml.Abs.ExnBind' a) where
  prt i = \case
    Jppml.Abs.EBCon _ idcap -> prPrec i 0 (concatD [prt 0 idcap])
    Jppml.Abs.EBArg _ idcap typ -> prPrec i 0 (concatD [prt 0 idcap, doc (showString "of"), prt 0 typ])

instance Print [Jppml.Abs.ExnBind' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]
