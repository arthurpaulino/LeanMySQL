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

declare_syntax_cat               sqlProp
syntax "TRUE"                  : sqlProp
syntax "FALSE"                 : sqlProp
syntax ident "="  ident        : sqlProp
syntax ident "<>" ident        : sqlProp
syntax ident "<"  ident        : sqlProp
syntax ident "<=" ident        : sqlProp
syntax ident ">"  ident        : sqlProp
syntax ident ">=" ident        : sqlProp
syntax sqlProp " AND " sqlProp : sqlProp
syntax sqlProp " OR "  sqlProp : sqlProp
syntax " NOT " sqlProp         : sqlProp

declare_syntax_cat join
syntax "INNER "  : join
syntax "LEFT "   : join
syntax "RIGHT "  : join
syntax "OUTER "  : join

declare_syntax_cat                                  sqlFrom
syntax ident                                      : sqlFrom
syntax "(" ident ")"                              : sqlFrom
syntax sqlFrom " AS " ident                       : sqlFrom
syntax sqlFrom join "JOIN " sqlFrom "ON " sqlProp : sqlFrom

syntax (name := query) "SELECT " sqlSelect "FROM" sqlFrom ("WHERE" sqlProp)? : term

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

def mkConstM (name : Name) : MetaM Expr :=
  pure $ mkConst name

def mkProp : Syntax → TermElabM Expr
  | stx => mkConstM `SQLProp.tt

def mkJoin : Syntax → TermElabM Expr
  | `(join|INNER) => mkConstM `SQLJoin.inner
  | `(join|LEFT)  => mkConstM `SQLJoin.left
  | `(join|RIGHT) => mkConstM `SQLJoin.right
  | `(join|OUTER) => mkConstM `SQLJoin.outer
  | _ => throwUnsupportedSyntax

partial def mkFrom : Syntax → TermElabM Expr
  | `(sqlFrom|$t:ident)               => mkAppM `SQLFrom.table #[mkStrOfIdent t]
  | `(sqlFrom|($t:ident))             => mkAppM `SQLFrom.table #[mkStrOfIdent t]
  | `(sqlFrom|$f:sqlFrom AS $t:ident) => do
    mkAppM `SQLFrom.alias #[← mkFrom f, mkStrOfIdent t]
  | `(sqlFrom|$l:sqlFrom $j:join JOIN $r:sqlFrom ON $p:sqlProp) => do
    mkAppM `SQLFrom.join #[← mkJoin j, ← mkFrom l, ← mkFrom r, ← mkProp p]
  | _                                 => throwUnsupportedSyntax

@[termElab query] def elabQuery : Term.TermElab := fun stx _ =>
  match stx with
  | `(query| SELECT $sel FROM $frm $[WHERE $prp]?) => do
    let whr ← match prp with
    | none     => mkConstM `SQLProp.tt
    | some prp => mkProp prp
    mkAppM `SQLQuery.mk #[← mkSelect sel, ← mkFrom frm, whr]
  | _ => throwUnsupportedSyntax

#check SELECT (a), b AS c FROM f
def s : SQLQuery := SELECT DISTINCT a, b AS c FROM l LEFT JOIN r ON z = x WHERE w = t
#eval s.toString
