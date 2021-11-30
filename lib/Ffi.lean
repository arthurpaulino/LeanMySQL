@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

inductive Entry
  | str (s : String)
  | int (n : Int)
  | float (f : Float)
  | null

constant NULL : Entry := Entry.null

instance : Coe Int Entry where
  coe := Entry.int

instance : Coe String Entry where
  coe := Entry.str

instance : Coe Float Entry where
  coe := Entry.float

instance : OfScientific Entry where
  ofScientific m s e := Entry.float (OfScientific.ofScientific m s e)

namespace Entry

protected def toString (e : Entry) : String := 
match e with
  | Entry.str e => s!"'{e}'"
  | Entry.int e => toString e
  | Entry.float e => toString e
  | Entry.null => "NULL"

instance : ToString Entry where
  toString e := e.toString

end Entry

abbrev Row := List Entry

namespace Row

private def toStrings (r : Row) : List String :=
r.map Entry.toString

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

abbrev Col := Lean.Name

namespace Col

private def toString (c : Col) := Lean.Name.toString c

end Col

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

namespace ColProp

private def toString (cp : ColProp) : String :=
match cp with
| ColProp.EqE c e => s!"{c}={e}"
| ColProp.NeqE c e => s!"{c}≠{e}"
| ColProp.LeE c e => s!"{c}≤{e}"
| ColProp.LE c e => s!"{c}<{e}"
| ColProp.GeE c e => s!"{c}≥{e}"
| ColProp.GE c e => s!"{c}>{e}"
| ColProp.EqC c c' => s!"{c}={c'}"
| ColProp.NeqC c c' => s!"{c}≠{c'}"
| ColProp.LeC c c' => s!"{c}≤{c'}"
| ColProp.LC c c' => s!"{c}<{c'}"
| ColProp.GeC c c' => s!"{c}≥{c'}"
| ColProp.GC c c' => s!"{c}>{c'}"
| ColProp.And cp cp' => s!"({toString cp}) and ({toString cp'})"
| ColProp.Or cp cp' => s!"({toString cp}) or ({toString cp'})"

private instance : ToString ColProp where
  toString cp := cp.toString

end ColProp

mutual
  inductive Query
    | mk (name : String) (steps : List QueryStep)
  private inductive QueryStep
    | select (l : List Col)
    | filter (cp : ColProp)
    | join (q : Query) (on : ColProp) (how : String)
    deriving Inhabited
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

private structure SQL where
  Select : List String
  From : String
  Where : String
  As : String

namespace SQL

private def init (t : String) (as : String) : SQL :=
⟨[], t, "", as⟩

private def toString (sql : SQL) : String :=
let select := if sql.Select = [] then "*" else ",".intercalate sql.Select
  if sql.Where ≠ "" then
    if sql.As ≠ "" then
      s!"(select {select} from {sql.From} where {sql.Where}) as {sql.As}"
    else
      s!"select {select} from {sql.From} where {sql.Where}"
  else
    if sql.As ≠ "" then
      s!"(select {select} from {sql.From}) as {sql.As}"
    else
      s!"select {select} from {sql.From}"

-- private def toString' (sql : SQL) : String :=

end SQL

private partial def applyStep (sql : SQL) (step : QueryStep) : SQL :=
match step with
| QueryStep.select l =>
  let newList := l.map Col.toString
  if sql.Select = [] then
    ⟨newList, sql.From, sql.Where, sql.As⟩
  else
    ⟨sql.Select.filter (λ s => newList.contains s), sql.From, sql.Where, sql.As⟩
| QueryStep.filter cp =>
  let newWhere := if sql.Where ≠ "" then s!"({sql.Where}) and ({cp})" else cp.toString
  ⟨sql.Select, sql.From, newWhere, sql.As⟩
| QueryStep.join q on how =>
  let newSQL := q.steps.foldl applyStep (SQL.init q.name q.name)
  let newFrom := s!"{sql.toString} {how} join {newSQL.toString} on {on}"
  ⟨[], newFrom, "", "joined"⟩

namespace Query

private def build (q : Query) : String :=
"select * from " ++ (SQL.toString (q.steps.foldl applyStep (SQL.init q.name q.name)))

end Query

#eval (table "person"
  ↠ select [`age, `job_id]
  -- ↠ join (table "job") (`person.job_id = `job.id) "left"
  ↠ filter (`age > 20)
  -- ↠ join (table "person") (`person.id = `person.id) "inner"
  ).build
#exit
private inductive DType | DInt | DFloat | DString

structure Table where
  names : List String
  types : List DType
  rows : List Row
  deriving Inhabited

namespace Table

private def dTypesMap (t : String) : DType :=
if t = "i" then
  DType.DInt
else
  if t = "f" then
    DType.DFloat
  else
    DType.DString

private constant typeSep : String := "^^"
private constant colSep : String := "~~"
private constant lineSep : String := "¨¨"

private def toFloat (s : String) : Float :=
let split := s.splitOn "."
let l := split.head!.splitOn "-"
let r := split.getLast!
let rFloat := r.toNat!.toFloat / (10.0 ^ r.length.toFloat)
if l.length = 1 then
  return l.head!.toNat!.toFloat + rFloat
else
  return -1.0 * (l.getLast!.toNat!.toFloat + rFloat)

private def parseStringToEntry (dType : DType) (s : String) : Entry :=
if s ≠ "NULL" then
  match dType with
  | DType.DInt => s.toInt!
  | DType.DFloat => toFloat s
  | DType.DString => s
else
  Entry.null

private def parse (s : String) : Table := do
  if s.length = 0 then
    ⟨[], [], []⟩
  else
    let mut names : List String := []
    let mut dTypes : List DType := []
    let mut data : List Row := []
    let lines : List String := s.splitOn lineSep
    let header : String := lines.head!
    let headerParts : List String := header.splitOn colSep
    for headerPart in headerParts do
      let split : List String := headerPart.splitOn typeSep
      names := names.concat (split.head!)
      dTypes := dTypes.concat (dTypesMap (split.getLast!))
    let mut i : Nat := 0
    let maxI : Nat := lines.tail!.length
    for row in lines.tail! do
      let mut j : Nat := 0
      let mut rowData : List Entry := []
      let rowSplit := row.splitOn colSep
      for dType in dTypes do
        let valString : String := rowSplit.get! j
        rowData := rowData.concat (parseStringToEntry dType valString)
        j := j + 1
      data := data.concat rowData
      i := i + 1
      if i = maxI - 1 then
        break
    ⟨names, dTypes, data⟩

def toString (t : Table) : String := do
  let mut res : String := ""
  for n in t.names do
    res := res ++ n ++ "|"
  res := res ++ "\n"
  let mut i : Nat := 0
  let maxI : Nat := t.rows.length
  for row in t.rows do
    let rowStrings := row.toStrings
    for s in rowStrings do
      res := res ++ s ++ "|"
    if i = maxI - 1 then
      break
    res := res ++ "\n"
    i := i + 1
  res

instance : ToString Table where
  toString t := t.toString

end Table

constant MySQL : Type

def kb (b : UInt64) : UInt64 := 1024 * b

def mb (b : UInt64) : UInt64 := 1048576 * b

def gb (b : UInt64) : UInt64 := 1073741824 * b

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk (bufferSize : UInt64 := kb 8) : IO MySQL

@[extern "lean_mysql_set_buffer_size"]
constant setBufferSizeMB (bufferSize : UInt64) : IO Unit

@[extern "lean_mysql_version"]
constant version (m : MySQL) : String

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

@[extern "lean_mysql_query"]
private constant querySQLPriv (m : MySQL) (q : String) : IO Unit

def querySQL (m : MySQL) (q : String) : IO Unit := m.querySQLPriv q

constant query (m : MySQL) (q : Query) : IO Unit := m.querySQL q.build

@[extern "lean_mysql_get_query_result"]
private constant getQueryResultPriv (m : MySQL) : String

def getQueryResult (m : MySQL) : Table := Table.parse (getQueryResultPriv m)

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
