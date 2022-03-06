/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import SQLDSL
import Lean

open Lean Elab Meta

declare_syntax_cat        colStx
syntax ident            : colStx
syntax ident "AS" ident : colStx

declare_syntax_cat selectStx
syntax "*"       : selectStx
syntax colStx,+  : selectStx

def mkStrOfIdent (id : Syntax) : Expr :=
  mkStrLit id.getId.toString

def mkCol (colStx : Syntax) : MetaM Expr :=
  match colStx with
  | `(colStx|$c:ident)             =>
    pure $ mkApp (mkConst `SQLColumn.col) (mkStrOfIdent c)
  | `(colStx|$c:ident AS $a:ident) => do
    let col := mkApp (mkConst `SQLColumn.col) (mkStrOfIdent c)
    pure $ mkApp2 (mkConst `SQLColumn.as) col (mkStrOfIdent a)
  | _                              => throwUnsupportedSyntax

def mkBoolLit : Bool → Expr
  | true  => mkConst `Bool.true
  | false => mkConst `Bool.false

def mkSelect (distinctStx selectStx : Syntax) : MetaM Expr :=
  match selectStx with
  | `(selectStx|*)            =>
    pure $ mkApp (mkConst `SQLSelect.all) (mkBoolLit !distinctStx.isNone)
  | `(selectStx|$cs:colStx,*) => do
    let cols ← mkListLit (mkConst `SQLColumn) (← cs.getElems.toList.mapM mkCol)
    pure $ mkApp2 (mkConst `SQLSelect.list) (mkBoolLit !distinctStx.isNone) cols
  | _                         => throwUnsupportedSyntax

elab "SELECT" distinctStx:"DISTINCT"? selectStx:selectStx : term => do
  let select ← mkSelect distinctStx selectStx
  pure select

#check SELECT a, b AS c
def s : SQLSelect := SELECT DISTINCT a, b AS c
#eval s.toString
