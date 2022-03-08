/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Lean
import SQLDSL

open Lean Elab Meta Parser

declare_syntax_cat      parsId
syntax ident          : parsId
syntax "(" parsId ")" : parsId

declare_syntax_cat           selectField
syntax parsId              : selectField
syntax parsId " AS " ident : selectField

declare_syntax_cat                 sqlSelect
syntax "*"                       : sqlSelect
syntax "DISTINCT " "*"           : sqlSelect
syntax selectField,+             : sqlSelect
syntax "DISTINCT " selectField,+ : sqlSelect

declare_syntax_cat               entry
syntax num                     : entry
syntax "-" noWs num            : entry
syntax str                     : entry
syntax scientificLit           : entry
syntax "-" noWs scientificLit  : entry
syntax "NULL"                  : entry
syntax "(" entry ")"           : entry

declare_syntax_cat               sqlProp
syntax "TRUE"                  : sqlProp
syntax "FALSE"                 : sqlProp
syntax parsId " = "  parsId    : sqlProp
syntax parsId " <> " parsId    : sqlProp
syntax parsId " < "  parsId    : sqlProp
syntax parsId " <= " parsId    : sqlProp
syntax parsId " > "  parsId    : sqlProp
syntax parsId " >= " parsId    : sqlProp
syntax parsId " = "  entry     : sqlProp
syntax sqlProp " AND " sqlProp : sqlProp
syntax sqlProp " OR "  sqlProp : sqlProp
syntax " NOT " sqlProp         : sqlProp
syntax "(" sqlProp ")"         : sqlProp

declare_syntax_cat join
syntax " INNER " : join
syntax " LEFT "  : join
syntax " RIGHT " : join
syntax " OUTER " : join

declare_syntax_cat                                    sqlFrom
syntax ident                                        : sqlFrom
syntax sqlFrom " AS " ident                         : sqlFrom
syntax sqlFrom join " JOIN " sqlFrom " ON " sqlProp : sqlFrom
syntax "(" sqlFrom ")"                              : sqlFrom

syntax (name := query) "SELECT " sqlSelect " FROM " sqlFrom (" WHERE " sqlProp)? : term

def mkStrOfIdent (id : Syntax) : Expr :=
  mkStrLit id.getId.toString

partial def mkStrOfParsId : Syntax → TermElabM Expr
  | `(parsId|$id:ident)      => pure $ mkStrLit id.getId.toString
  | `(parsId|($pars:parsId)) => mkStrOfParsId pars
  | _                        => throwUnsupportedSyntax

def mkCol : Syntax → TermElabM Expr
  | `(selectField|$c:parsId)             => do
    mkAppM `SQLSelectField.col #[← mkStrOfParsId c]
  | `(selectField|$c:parsId AS $a:ident) => do
    mkAppM `SQLSelectField.alias #[← mkStrOfParsId c, mkStrOfIdent a]
  | _                                    => throwUnsupportedSyntax

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

def mkApp' (name : Name) (e : Expr) : Expr :=
  mkApp (mkConst name) e

def mkConstM (name : Name) : TermElabM Expr :=
  pure $ mkConst name

partial def mkEntry : Syntax → TermElabM Expr
  | `(entry|$v:numLit)         =>
    mkAppM `DataEntry.EInt #[mkApp' `Int.ofNat (mkNatLit v.toNat)]
  | `(entry|-$v:numLit)        =>
    mkAppM `DataEntry.EInt $ match v.toNat with
      | Nat.zero   => #[mkApp' `Int.ofNat (mkConst `Nat.zero)]
      | Nat.succ n => #[mkApp' `Int.negSucc (mkNatLit n)]
  | `(entry|$v:strLit)         =>
    mkAppM `DataEntry.EString #[mkStrLit $ v.isStrLit?.getD ""]
  | `(entry|$v:scientificLit)  => do
    mkAppM `DataEntry.EFloat #[← Term.elabScientificLit v (mkConst `Float)]
  -- | `(entry|-$v:scientificLit)  => 
  | `(entry|NULL)              => mkConstM `DataEntry.ENull
  | `(entry|($e:entry))        => mkEntry e
  | _                          => throwUnsupportedSyntax

def mkProp : Syntax → TermElabM Expr
  | `(sqlProp|$id:parsId = $e:entry) => do
    mkAppM `SQLProp.eqE #[← mkStrOfParsId id, ← mkEntry e]
  | stx => mkConstM `SQLProp.tt

def mkJoin : Syntax → TermElabM Expr
  | `(join|INNER) => mkConstM `SQLJoin.inner
  | `(join|LEFT)  => mkConstM `SQLJoin.left
  | `(join|RIGHT) => mkConstM `SQLJoin.right
  | `(join|OUTER) => mkConstM `SQLJoin.outer
  | _             => throwUnsupportedSyntax

partial def mkFrom : Syntax → TermElabM Expr
  | `(sqlFrom|$t:ident)               => mkAppM `SQLFrom.table #[mkStrOfIdent t]
  | `(sqlFrom|$f:sqlFrom AS $t:ident) => do
    mkAppM `SQLFrom.alias #[← mkFrom f, mkStrOfIdent t]
  | `(sqlFrom|$l:sqlFrom $j:join JOIN $r:sqlFrom ON $p:sqlProp) => do
    mkAppM `SQLFrom.join #[← mkJoin j, ← mkFrom l, ← mkFrom r, ← mkProp p]
  | `(sqlFrom|($f:sqlFrom))           => mkFrom f
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
def s : SQLQuery := SELECT DISTINCT (a), b AS c
  FROM (l) AS ll LEFT JOIN r ON z = x
  WHERE w = 1.4
#eval s.toString
