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

declare_syntax_cat          sqlFrom
syntax ident              : sqlFrom
syntax sqlFrom "AS" ident : sqlFrom

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

def mkSelect (dist sel : Syntax) : MetaM Expr :=
  match sel with
  | `(sqlSelect|*)                 =>
    pure $ mkApp (mkConst `SQLSelect.all) (mkBoolLit !dist.isNone)
  | `(sqlSelect|$cs:selectField,*) => do
    let cols ← mkListLit (mkConst `SQLSelectField) (← cs.getElems.toList.mapM mkCol)
    pure $ mkApp2 (mkConst `SQLSelect.list) (mkBoolLit !dist.isNone) cols
  | _                              => throwUnsupportedSyntax

def mkFrom (frm : Syntax) : MetaM Expr :=
  pure $ mkApp (mkConst `SQLFrom.table) (mkStrOfIdent frm)

def mkWhere (whr : Syntax) : MetaM Expr :=
  pure $ mkConst `SQLProp.all

elab "SELECT" dist:"DISTINCT"? sel:sqlSelect "FROM" frm:sqlFrom : term => do
  pure $ mkApp3 (mkConst `SQLQuery.mk)
    (← mkSelect dist sel) (← mkFrom frm) (mkConst `SQLProp.all)

elab "SELECT" dist:"DISTINCT"? sel:sqlSelect "FROM" frm:sqlFrom "WHERE" whr:sqlProp : term => do
  pure $ mkApp3 (mkConst `SQLQuery.mk)
    (← mkSelect dist sel) (← mkFrom frm) (← mkWhere whr)

#check SELECT a, b AS c FROM f
def s : SQLQuery := SELECT DISTINCT a, b AS c FROM f WHERE w = t
#eval s.toString
