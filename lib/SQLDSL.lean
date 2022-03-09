/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import DataEntries

inductive SQLSelectField
  | col   : String → SQLSelectField
  | alias : String → String         → SQLSelectField

inductive SQLSelect
  | list : Bool → List SQLSelectField → SQLSelect
  | all  : Bool → SQLSelect

inductive SQLProp
  | tt : SQLProp
  | ff : SQLProp
  | eqC : String  → String    → SQLProp
  | neC : String  → String    → SQLProp
  | ltC : String  → String    → SQLProp
  | leC : String  → String    → SQLProp
  | gtC : String  → String    → SQLProp
  | geC : String  → String    → SQLProp
  | eqE : String  → DataEntry → SQLProp
  | neE : String  → DataEntry → SQLProp
  | ltE : String  → DataEntry → SQLProp
  | leE : String  → DataEntry → SQLProp
  | gtE : String  → DataEntry → SQLProp
  | geE : String  → DataEntry → SQLProp
  | and : SQLProp → SQLProp   → SQLProp
  | or  : SQLProp → SQLProp   → SQLProp
  | not : SQLProp → SQLProp

inductive SQLJoin
  | inner | left | right | outer

inductive SQLFrom
  | table : String  → SQLFrom
  | alias : SQLFrom → String  → SQLFrom
  | join  : SQLJoin → SQLFrom → SQLFrom → SQLProp → SQLFrom

structure SQLQuery where
  SELECT : SQLSelect
  FROM   : SQLFrom
  WHERE  : SQLProp

def SQLSelectField.toString : SQLSelectField → String
  | col   c   => c
  | alias c a => s!"{c} AS {a}"

def SQLSelect.distinct? (d : Bool) : String :=
  if d then "DISTINCT " else default

def SQLSelect.toString : SQLSelect → String
  | list d l => (distinct? d).append $ ", ".intercalate $ l.map (SQLSelectField.toString)
  | all  d   => (distinct? d).append $ "*"

def SQLProp.toString : SQLProp → String
  | tt     => "TRUE"
  | ff     => "FALSE"
  | eqC l r => s!"{l} = {r}"
  | neC l r => s!"{l} <> {r}"
  | ltC l r => s!"{l} < {r}"
  | leC l r => s!"{l} <= {r}"
  | gtC l r => s!"{l} > {r}"
  | geC l r => s!"{l} >= {r}"
  | eqE l r => s!"{l} = {r.toString}"
  | neE l r => s!"{l} <> {r.toString}"
  | ltE l r => s!"{l} < {r.toString}"
  | leE l r => s!"{l} <= {r.toString}"
  | gtE l r => s!"{l} > {r.toString}"
  | geE l r => s!"{l} >= {r.toString}"
  | and l r => s!"({l.toString}) AND ({r.toString})"
  | or  l r => s!"({l.toString}) OR ({r.toString})"
  | not w   => s!"NOT ({w.toString})"

def SQLJoin.toString : SQLJoin → String
  | inner => "INNER"
  | left  => "LEFT"
  | right => "RIGHT"
  | outer => "OUTER"

def SQLFrom.toString : SQLFrom → String
  | table s       => s
  | alias f s     => s!"({f.toString}) AS {s}"
  | join  j l r p => s!"{l.toString} {j.toString} JOIN {r.toString} ON {p.toString}"

def SQLQuery.toString (q : SQLQuery) : String :=
  s!"SELECT {q.SELECT.toString} FROM {q.FROM.toString} WHERE {q.WHERE.toString}"
