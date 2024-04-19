module Interpreter where

import ParSyntax
import LexSyntax

transTree s =
  pListDec (myLexer s)


