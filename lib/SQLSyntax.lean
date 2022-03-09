/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Lean
import SQLDSL

open Lean Elab Meta

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

declare_syntax_cat           entry
syntax num                 : entry
syntax "-" noWs num        : entry
syntax str                 : entry
syntax scientific          : entry
syntax "-" noWs scientific : entry
syntax "NULL"              : entry
syntax "(" entry ")"       : entry

declare_syntax_cat propSymbol
syntax " = "     : propSymbol
syntax " <> "    : propSymbol
syntax " < "     : propSymbol
syntax " <= "    : propSymbol
syntax " > "     : propSymbol
syntax " >= "    : propSymbol

declare_syntax_cat                prop
syntax "TRUE"                   : prop
syntax "FALSE"                  : prop
syntax parsId propSymbol parsId : prop
syntax parsId propSymbol entry  : prop
syntax prop " AND " prop        : prop
syntax prop " OR "  prop        : prop
syntax " NOT " prop             : prop
syntax "(" prop ")"             : prop

declare_syntax_cat join
syntax " INNER " : join
syntax " LEFT "  : join
syntax " RIGHT " : join
syntax " OUTER " : join

declare_syntax_cat                                 sqlFrom
syntax ident                                     : sqlFrom
syntax sqlFrom " AS " ident                      : sqlFrom
syntax sqlFrom join " JOIN " sqlFrom " ON " prop : sqlFrom
syntax "(" sqlFrom ")"                           : sqlFrom

syntax (name := query) "SELECT " sqlSelect " FROM " sqlFrom (" WHERE " prop)? : term

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

def negFloat (f : Float) : Float :=
  -1.0 * f

partial def mkEntry : Syntax → TermElabM Expr
  | `(entry|$v:numLit)         =>
    mkAppM `DataEntry.EInt #[mkApp' `Int.ofNat (mkNatLit v.toNat)]
  | `(entry|-$v:numLit)        =>
    mkAppM `DataEntry.EInt $ match v.toNat with
      | Nat.zero   => #[mkApp' `Int.ofNat (mkConst `Nat.zero)]
      | Nat.succ n => #[mkApp' `Int.negSucc (mkNatLit n)]
  | `(entry|$v:scientificLit)  => do
    mkAppM `DataEntry.EFloat #[← Term.elabScientificLit v (mkConst `Float)]
  | `(entry|-$v:scientificLit) => do
    let f ← Term.elabScientificLit v (mkConst `Float)
    mkAppM `DataEntry.EFloat #[mkApp' `negFloat f]
  | `(entry|$v:strLit)         =>
    mkAppM `DataEntry.EString #[mkStrLit $ v.isStrLit?.getD ""]
  | `(entry|NULL)              => mkConstM `DataEntry.ENull
  | `(entry|($e:entry))        => mkEntry e
  | _                          => throwUnsupportedSyntax

def mkPropSymbol (stx : Syntax) (isEntry : Bool) : TermElabM Name :=
  match stx with
  | `(propSymbol|=)  => pure $ if isEntry then `SQLProp.eqE else `SQLProp.eqC
  | `(propSymbol|<>) => pure $ if isEntry then `SQLProp.neE else `SQLProp.neC
  | `(propSymbol|<)  => pure $ if isEntry then `SQLProp.ltE else `SQLProp.ltC
  | `(propSymbol|<=) => pure $ if isEntry then `SQLProp.leE else `SQLProp.leC
  | `(propSymbol|>)  => pure $ if isEntry then `SQLProp.gtE else `SQLProp.gtC
  | `(propSymbol|>=) => pure $ if isEntry then `SQLProp.geE else `SQLProp.geC
  | _                => throwUnsupportedSyntax

partial def mkProp : Syntax → TermElabM Expr
  | `(prop|TRUE)                              => mkConstM `SQLProp.tt
  | `(prop|FALSE)                             => mkConstM `SQLProp.ff
  | `(prop|$l:parsId $s:propSymbol $r:parsId) => do
    mkAppM (← mkPropSymbol s false) #[← mkStrOfParsId l, ← mkStrOfParsId r]
  | `(prop|$c:parsId $s:propSymbol $e:entry)  => do
    mkAppM (← mkPropSymbol s true) #[← mkStrOfParsId c, ← mkEntry e]
  | `(prop|$l:prop AND $r:prop)               => do
    mkAppM `SQLProp.and #[← mkProp l, ← mkProp r]
  | `(prop|$l:prop OR $r:prop)                => do
    mkAppM `SQLProp.or #[← mkProp l, ← mkProp r]
  | `(prop|NOT $p:prop)                       => do
    mkAppM `SQLProp.not #[← mkProp p]
  | `(prop|($p:prop))                         => mkProp p
  | _                                         => throwUnsupportedSyntax

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
  | `(sqlFrom|$l:sqlFrom $j:join JOIN $r:sqlFrom ON $p:prop) => do
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
