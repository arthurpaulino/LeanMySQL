/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import DataEntries
import Lean

inductive SQLColumn
  | col : String    → SQLColumn
  | as  : SQLColumn → String → SQLColumn

inductive SQLTableJoin
  | inner : String → SQLTableJoin
  | left  : String → SQLTableJoin
  | right : String → SQLTableJoin
  | outer : String → SQLTableJoin

inductive SQLFrom
  | table : String  → SQLFrom
  | join  : SQLFrom → SQLFrom → SQLTableJoin → SQLFrom
  | as    : SQLFrom → String  → SQLFrom

inductive SQLWhere
  | nil : SQLWhere
  | eqC : SQLColumn → SQLColumn → SQLWhere
  | neC : SQLColumn → SQLColumn → SQLWhere
  | ltC : SQLColumn → SQLColumn → SQLWhere
  | leC : SQLColumn → SQLColumn → SQLWhere
  | gtC : SQLColumn → SQLColumn → SQLWhere
  | geC : SQLColumn → SQLColumn → SQLWhere
  | eqE : SQLColumn → DataEntry → SQLWhere
  | neE : SQLColumn → DataEntry → SQLWhere
  | ltE : SQLColumn → DataEntry → SQLWhere
  | leE : SQLColumn → DataEntry → SQLWhere
  | gtE : SQLColumn → DataEntry → SQLWhere
  | geE : SQLColumn → DataEntry → SQLWhere
  | and : SQLWhere  → SQLWhere  → SQLWhere
  | or  : SQLWhere  → SQLWhere  → SQLWhere
  | not : SQLWhere  → SQLWhere

structure SQLQuery where
  Select : List SQLColumn
  From   : SQLFrom
  Where  : SQLWhere

declare_syntax_cat query
declare_syntax_cat column_stx
declare_syntax_cat from_stx
declare_syntax_cat where_stx

-- syntax "select " column_stx,+ "from " from_stx "where " where_stx : query
-- macro_rules
--   | `(query|select )

elab "select " cs:column_stx,+ "from " f:stx "where " w:where_stx : term =>
  match f with

-- open SQLColumn SQLTableJoin SQLFrom SQLWhere

-- def qq : SQLQuery := ⟨
--   [col "c1", as (col "c2") "c3"],
--   as (join (table "t1") (table "t2") (left "c1")) "t3",
--   and (neE (col "t1.c1") 3) (ltC (col "t2.c2") (col "t2.c3"))
-- ⟩
