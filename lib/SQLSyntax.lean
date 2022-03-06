/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import SQLDSL
import Lean

open Lean Elab Meta

declare_syntax_cat col
syntax ident : col
syntax ident " AS " ident : col

declare_syntax_cat select
syntax "*" : select
syntax col,* : select

def mkCol (stx : Syntax) : Expr :=
  mkApp (mkConst `SQLColumn.col) (mkStrLit stx.getId.toString) 

def mkBoolLit : Bool → Expr
  | true  => mkConst `Bool.true
  | false => mkConst `Bool.false

def mkSelect (distinct select : Syntax) : MetaM Expr :=
  match select with
  | `(select| *)         => pure $ mkApp (mkConst `SQLSelect.all) (mkBoolLit !distinct.isNone)
  | `(select|)           => throwUnsupportedSyntax
  | `(select| $cs:col,*) => do
    let cols ← mkListLit (mkConst `SQLColumn) (cs.getElems.toList.map mkCol)
    pure $ mkApp2 (mkConst `SQLSelect.list) (mkBoolLit !distinct.isNone) cols
  | _                    => throwUnsupportedSyntax

elab "SELECT " distinct:"DISTINCT "? select:select : term => do
  let s ← mkSelect distinct select
  pure s

#check SELECT a, b

def s : SQLSelect := SELECT DISTINCT a, b

-- def mkCol (stx : Syntax) : Expr :=
--   mkApp (mkConst `SQLColumn.col) (mkStrLit stx.getId.toString)

-- elab "SELECT " cs:ident,+ : term => do
--   let cols ← mkListLit (mkConst `SQLColumn) (cs.getElems.toList.map mkCol)
--   pure $ mkApp2 (mkConst `SQLSelect.list) (mkConst `Bool.true) cols

-- def s : SQLSelect := SELECT a, b