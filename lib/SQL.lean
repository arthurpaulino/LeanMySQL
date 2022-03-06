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

inductive SQLSelect
  | list : Bool → List SQLColumn → SQLSelect
  | all  : Bool → SQLSelect

inductive SQLWhere
  | all : SQLWhere
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

inductive SQLTableJoin
  | inner | left | right | outer

inductive SQLFrom
  | table : String       → SQLFrom
  | as    : SQLFrom      → String   → SQLFrom
  | join  : SQLTableJoin → SQLWhere → SQLFrom → SQLFrom → SQLFrom

structure SQLQuery where
  SELECT : SQLSelect
  FROM   : SQLFrom
  WHERE  : SQLWhere

def SQLColumn.toString : SQLColumn → String
  | col s   => s
  | as  c s => s!"{c.toString} AS {s}"

def SQLSelect.distinct? (d : Bool) : String :=
  if d then "DISTINCT " else default

def SQLSelect.toString : SQLSelect → String
  | list d l => (distinct? d).append $ ", ".intercalate $ l.map (SQLColumn.toString)
  | all  d   => (distinct? d).append $ "*"

def SQLWhere.toString : SQLWhere → String
  | all     => "TRUE"
  | eqC l r => s!"{l.toString} = {r.toString}"
  | neC l r => s!"{l.toString} <> {r.toString}"
  | ltC l r => s!"{l.toString} < {r.toString}"
  | leC l r => s!"{l.toString} <= {r.toString}"
  | gtC l r => s!"{l.toString} > {r.toString}"
  | geC l r => s!"{l.toString} >= {r.toString}"
  | eqE l r => s!"{l.toString} = {r.toString}"
  | neE l r => s!"{l.toString} <> {r.toString}"
  | ltE l r => s!"{l.toString} < {r.toString}"
  | leE l r => s!"{l.toString} <= {r.toString}"
  | gtE l r => s!"{l.toString} > {r.toString}"
  | geE l r => s!"{l.toString} >= {r.toString}"
  | and l r => s!"({l.toString}) AND ({r.toString})"
  | or  l r => s!"({l.toString}) OR ({r.toString})"
  | not w   => s!"NOT ({w.toString})"

def SQLTableJoin.toString : SQLTableJoin → String
  | inner => "INNER"
  | left  => "LEFT"
  | right => "RIGHT"
  | outer => "OUTER"

def SQLFrom.toString : SQLFrom → String
  | table s       => s
  | as    f s     => s!"({f.toString}) AS {s}"
  | join  j w l r => s!"{l.toString} {j.toString} JOIN {r.toString} ON {w.toString}"

def SQLQuery.toString (q : SQLQuery) : String :=
  s!"SELECT {q.SELECT.toString} FROM {q.FROM.toString} WHERE {q.WHERE.toString}"

open SQLSelect SQLColumn SQLTableJoin SQLFrom SQLWhere

def q : SQLQuery := ⟨
  list true [col "name", as (col "age") "age_years"],
  join left (eqC (col "person.id") (col "job.person_id")) (table "person") (table "job"),
  and (eqE (col "name") "Arthur") (ltE (col "age_years") 50)
⟩

#eval q.toString
-- SELECT DISTINCT name, age AS age_years
-- FROM person LEFT JOIN job ON person.id = job.person_id
-- WHERE (name = 'Arthur') AND (age_years < 50)"
