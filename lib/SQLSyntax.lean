/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Lean
import SQLDSL

open Lean Elab Meta

declare_syntax_cat        selectField
syntax ident            : selectField
syntax ident "AS" ident : selectField

declare_syntax_cat     sqlSelect
syntax "*"           : sqlSelect
syntax selectField,+ : sqlSelect

declare_syntax_cat        sqlProp
syntax ident "="  ident : sqlProp
syntax ident "<>" ident : sqlProp
syntax ident "<"  ident : sqlProp
syntax ident "<=" ident : sqlProp
syntax ident ">"  ident : sqlProp
syntax ident ">=" ident : sqlProp

def mkStrOfIdent (id : Syntax) : Expr :=
  mkStrLit id.getId.toString

def mkCol (colStx : Syntax) : MetaM Expr :=
  match colStx with
  | `(selectField|$c:ident)             =>
    pure $ mkApp (mkConst `SQLSelectField.col) (mkStrOfIdent c)
  | `(selectField|$c:ident AS $a:ident) =>
    pure $ mkApp2 (mkConst `SQLSelectField.alias) (mkStrOfIdent c) (mkStrOfIdent a)
  | _                                   => throwUnsupportedSyntax

def mkBoolLit : Bool → Expr
  | true  => mkConst `Bool.true
  | false => mkConst `Bool.false

def mkSelect (distinctStx selectStx : Syntax) : MetaM Expr :=
  match selectStx with
  | `(sqlSelect|*)                 =>
    pure $ mkApp (mkConst `SQLSelect.all) (mkBoolLit !distinctStx.isNone)
  | `(sqlSelect|$cs:selectField,*) => do
    let cols ← mkListLit (mkConst `SQLSelectField) (← cs.getElems.toList.mapM mkCol)
    pure $ mkApp2 (mkConst `SQLSelect.list) (mkBoolLit !distinctStx.isNone) cols
  | _                              => throwUnsupportedSyntax

elab "SELECT" distinctStx:"DISTINCT"? selectStx:sqlSelect : term => do
  let select ← mkSelect distinctStx selectStx
  pure select

#check SELECT a, b AS c
def s : SQLSelect := SELECT DISTINCT a, b AS c
#eval s.toString
