@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

inductive Entry
  | str (s : String)
  | nat (n : Nat)
  | float (f : Float)

instance : Coe Nat Entry where
  coe := Entry.nat

instance : Coe String Entry where
  coe := Entry.str

instance : Coe Float Entry where
  coe := Entry.float

abbrev Row := List Entry

namespace Row

private def toStrings (r : Row) : List String :=
r.map λ e => match e with
  | Entry.str e => s!"'{e}'"
  | Entry.nat e => toString e
  | Entry.float e => toString e

private def build (r : Row) : String :=
s!"({",".intercalate (r.toStrings)})"

end Row

structure Column where
  name : String
  type : String
  deriving Inhabited

namespace Column

private def build (c : Column) : String :=
s!"{c.name} {c.type}"

end Column

abbrev TableScheme := List Column

namespace TableScheme

private def build (ts : TableScheme) : String :=
s!"({",".intercalate (ts.map λ v => v.build)})"

end TableScheme

--------------------------

abbrev Col := Lean.Name

private inductive ColProp
  | EqE (c : Col) (e : Entry)
  | NeqE (c : Col) (e : Entry)
  | LeE (c : Col) (e : Entry)
  | LE (c : Col) (e : Entry)
  | GeE (c : Col) (e : Entry)
  | GE (c : Col) (e : Entry)
  | EqC (c : Col) (c' : Col)
  | NeqC (c : Col) (c' : Col)
  | LeC (c : Col) (c' : Col)
  | LC (c : Col) (c' : Col)
  | GeC (c : Col) (c' : Col)
  | GC (c : Col) (c' : Col)
  | And (cp : ColProp) (cp' : ColProp)
  | Or (cp : ColProp) (cp' : ColProp)

infix:50 " = " => ColProp.EqE
infix:50 " ≠ " => ColProp.NeqE
infix:50 " ≤ " => ColProp.LeE
infix:50 " < " => ColProp.LE
infix:50 " ≥ " => ColProp.GeE
infix:50 " > " => ColProp.GE
infix:50 " = " => ColProp.EqC
infix:50 " ≠ " => ColProp.NeqC
infix:50 " ≤ " => ColProp.LeC
infix:50 " < " => ColProp.LC
infix:50 " ≥ " => ColProp.GeC
infix:50 " > " => ColProp.GC
infix:25 " ∧ " => ColProp.And
infix:25 " ∨ " => ColProp.Or

mutual
  inductive Query
    | mk (name : String) (steps : List QueryStep)
  private inductive QueryStep
    | select (l : List Col)
    | filter (cp : ColProp)
    | join (q : Query) (on : ColProp) (how : String)
end

def table (n : String) : Query := ⟨n, []⟩

namespace Query

private def name : Query → String
| mk name _ => name

private def steps : Query → List QueryStep
| mk _ steps => steps

end Query

def select (l : List Col) (q : Query) : Query :=
⟨q.name, q.steps.concat (QueryStep.select l)⟩

def filter (cp : ColProp) (q : Query)  : Query :=
⟨q.name, q.steps.concat (QueryStep.filter cp)⟩

def join (q' : Query) (on : ColProp) (how : String) (q : Query) : Query :=
⟨q.name, q.steps.concat (QueryStep.join q' on how)⟩

private def transform (q : Query) (f : Query → Query) : Query := f q

infixl:50 "↠" => transform

namespace Query

/-
#todo
Find a way to transform a Query like

```
table "cars" ↠
select [`id, `name] ↠
join (table "prices") (`l.id = `r.id) "left" ↠
filter (`price = 2)
```

into:

```
select *
from (select id,name from car) as l left join (select * from price) as r on l.id=r.id
where price = 2;
```
-/
private def build (q : Query) : String := sorry

end Query

structure Table where
  types : List Type
  rows : List Row
  deriving Inhabited

namespace Table

private def parse (s : String) : Table := do
  if s.length = 0 then
    ⟨[],[]⟩
  else
    ⟨[],[]⟩

end Table

---------------------

constant MySQL : Type

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk (bufferSizeKB : Nat := 8) : IO MySQL

@[extern "lean_mysql_set_buffer_size"]
constant setBufferSizeMB (bufferSizeKB : Nat) : IO Unit

@[extern "lean_mysql_version"]
constant version (m : MySQL) : BaseIO String

@[extern "lean_mysql_login"]
constant login (m : MySQL) (h u p : String) : IO Unit

@[extern "lean_mysql_run"]
private constant run (m : MySQL) (q : String) : IO Unit

def createDB (m : MySQL) (d : String) : IO Unit :=
m.run ("create database " ++ d)

def dropDB (m : MySQL) (d : String) : IO Unit :=
m.run ("drop database " ++ d)

def useDB (m : MySQL) (d : String) : IO Unit :=
m.run ("use " ++ d)

def createTable (m : MySQL) (n : String) (ts : TableScheme) : IO Unit :=
m.run ("create table " ++ (n ++ ts.build))

def dropTable (m : MySQL) (n : String) : IO Unit :=
m.run ("drop table " ++ n)

def insertIntoTable (m : MySQL) (n : String) (r : Row) : IO Unit :=
m.run s!"insert into {n} values{r.build}"

------ querying

@[extern "lean_mysql_query"]
private constant querySQLPriv (m : MySQL) (q : String) : IO Unit

def querySQL (m : MySQL) (q : String) : IO Unit := m.querySQLPriv q

/-# todo
  constant query (m : MySQL) (q : Query) : IO Unit := m.querySQL q.build
-/

------ extract query result

@[extern "lean_mysql_get_query_result"]
private constant getQueryResultPriv (m : MySQL) : String

def getQueryResult (m : MySQL) : Table := Table.parse (getQueryResultPriv m)

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
