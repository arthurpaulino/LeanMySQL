/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Lean
import SQLDSL

open Lean Elab Meta

declare_syntax_cat                  selectField
syntax ident                      : selectField
syntax ident " AS " ident         : selectField
syntax "(" ident ")"              : selectField
syntax "(" ident ")" " AS " ident : selectField

declare_syntax_cat                 sqlSelect
syntax "*"                       : sqlSelect
syntax "DISTINCT " "*"           : sqlSelect
syntax selectField,+             : sqlSelect
syntax "DISTINCT " selectField,+ : sqlSelect

declare_syntax_cat        sqlProp
syntax "TRUE"           : sqlProp
syntax "FALSE"          : sqlProp
syntax ident "="  ident : sqlProp
syntax ident "<>" ident : sqlProp
syntax ident "<"  ident : sqlProp
syntax ident "<=" ident : sqlProp
syntax ident ">"  ident : sqlProp
syntax ident ">=" ident : sqlProp

declare_syntax_cat            sqlFrom
syntax ident                : sqlFrom
syntax sqlFrom " AS " ident : sqlFrom

syntax (name := query) "SELECT" sqlSelect "FROM" sqlFrom ("WHERE" sqlProp)? : term

def mkStrOfIdent (id : Syntax) : Expr :=
  mkStrLit id.getId.toString

def mkCol : Syntax → TermElabM Expr
  | `(selectField|$c:ident)               => mkAppM `SQLSelectField.col #[mkStrOfIdent c]
  | `(selectField|($c:ident))             => mkAppM `SQLSelectField.col #[mkStrOfIdent c]
  | `(selectField|$c:ident AS $a:ident)   =>
    mkAppM `SQLSelectField.alias #[mkStrOfIdent c, mkStrOfIdent a]
  | `(selectField|($c:ident) AS $a:ident) =>
    mkAppM `SQLSelectField.alias #[mkStrOfIdent c, mkStrOfIdent a]
  | _                                     => throwUnsupportedSyntax

def mkSelect : Syntax → TermElabM Expr
  | `(sqlSelect|*)                          => mkAppM `SQLSelect.all #[mkConst ``false]
  | `(sqlSelect|DISTINCT *)                 => mkAppM `SQLSelect.all #[mkConst ``true]
  | `(sqlSelect|$cs:selectField,*)          => do
    let cols ← mkListLit (mkConst `SQLSelectField) (← cs.getElems.toList.mapM mkCol)
    mkAppM `SQLSelect.list #[mkConst ``false, cols]
  | `(sqlSelect|DISTINCT $cs:selectField,*) => do
    let cols ← mkListLit (mkConst `SQLSelectField) (← cs.getElems.toList.mapM mkCol)
    mkAppM `SQLSelect.list #[mkConst ``true, cols]
  | _                                       => throwUnsupportedSyntax

partial def mkFrom : Syntax → TermElabM Expr
  | `(sqlFrom|$id:ident) => mkAppM `SQLFrom.table #[mkStrOfIdent id]
  | _                    => throwUnsupportedSyntax

def mkWhere (whr : Syntax) : TermElabM Expr :=
  pure $ mkConst `SQLProp.all

@[termElab query] def elabQuery : Term.TermElab := fun stx _ =>
  match stx with
  | `(query| SELECT $sel FROM $frm $[WHERE $whr]?) => do
    let whrExp ← match whr with
      | none     => pure $ mkConst `SQLProp.all
      | some whr => mkWhere whr
    mkAppM `SQLQuery.mk #[← mkSelect sel, ← mkFrom frm, whrExp]
  | _ => throwUnsupportedSyntax

#check SELECT a, b AS c FROM f
def s : SQLQuery := SELECT DISTINCT a, b AS c FROM f WHERE w = t
#eval s.toString
