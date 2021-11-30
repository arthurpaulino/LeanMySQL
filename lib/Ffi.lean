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

namespace List

def reversed : List α → List α
  | nil      => nil
  | cons h t => (reversed t).concat h

end List

namespace String

def withoutRightmostZeros (s : String) : String := do
  if s = "" then
    s
  else
    let data := s.data
    let mut rangeList : List Nat := []
    for i in [0 : data.length] do
      rangeList := rangeList.concat i
    for i in rangeList.reversed do
      if i = 0 then
        return ""
      if (data.get! i) ≠ '0' then
        let sub : Substring := ⟨s, 0, i + 1⟩
        return sub.toString
    s

def optimizeFloatString (s : String) : String :=
  let split := s.splitOn "."
  let cleanL := split.getLast!.withoutRightmostZeros
  split.head! ++ "." ++ (if cleanL = "" then "0" else cleanL)

end String

namespace Entry

protected def toString (e : Entry) : String := 
  match e with
  | Entry.str e => s!"'{e}'"
  | Entry.int e => toString e
  | Entry.float e => (toString e).optimizeFloatString
  | Entry.null => "NULL"

instance : ToString Entry where
  toString e := e.toString

end Entry

abbrev Row := List Entry

namespace Row

def toStrings (r : Row) : List String :=
  r.map Entry.toString

def build (r : Row) : String :=
  s!"({",".intercalate (r.toStrings)})"

end Row

structure Column where
  name : String
  type : String
  deriving Inhabited

namespace Column

def build (c : Column) : String :=
  s!"{c.name} {c.type}"

end Column

abbrev TableScheme := List Column

namespace TableScheme

def build (ts : TableScheme) : String :=
  s!"({",".intercalate (ts.map λ v => v.build)})"

end TableScheme

inductive DType | DInt | DFloat | DString

namespace String

def toFloat! (s : String) : Float :=
  let split := s.splitOn "."
  let l := split.head!.splitOn "-"
  if split.length = 2 then
    let r := split.getLast!
    let rFloat := r.toNat!.toFloat / (10.0 ^ r.length.toFloat)
    if l.length = 1 then
      l.head!.toNat!.toFloat + rFloat
    else
      -1.0 * (l.getLast!.toNat!.toFloat + rFloat)
  else
    if l.length = 1 then
      l.head!.toNat!.toFloat
    else
      -1.0 * l.getLast!.toNat!.toFloat

def toDType! (t : String) : DType :=
  if t = "i" then
    DType.DInt
  else
    if t = "f" then
      DType.DFloat
    else
      DType.DString


def toEntry! (s : String) (dType : DType) : Entry :=
  if s ≠ "NULL" then
    match dType with
    | DType.DInt => s.toInt!
    | DType.DFloat => s.toFloat!
    | DType.DString => s
  else
    Entry.null

end String

structure Table where
  names : List String
  types : List DType
  rows : List Row
  deriving Inhabited

namespace Table

def parse (s : String) : Table := do
  if s.length = 0 then
    ⟨[], [], []⟩
  else
    let typeSep : String := "^^"
    let colSep : String := "~~"
    let lineSep : String := "¨¨"
    let mut names : List String := []
    let mut dTypes : List DType := []
    let mut data : List Row := []
    let lines : List String := s.splitOn lineSep
    let header : String := lines.head!
    let headerParts : List String := header.splitOn colSep
    for headerPart in headerParts do
      let split : List String := headerPart.splitOn typeSep
      names := names.concat (split.head!)
      dTypes := dTypes.concat (split.getLast!.toDType!)
    let mut i : Nat := 0
    let maxI : Nat := lines.tail!.length
    for row in lines.tail! do
      let mut j : Nat := 0
      let mut rowData : List Entry := []
      let rowSplit := row.splitOn colSep
      for dType in dTypes do
        let valString : String := rowSplit.get! j
        rowData := rowData.concat (valString.toEntry! dType)
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
constant run (m : MySQL) (q : String) : IO Unit

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
constant query (m : MySQL) (q : String) : IO Unit

@[extern "lean_mysql_get_query_result"]
constant getQueryResultRaw (m : MySQL) : String

def getQueryResult (m : MySQL) : Table :=
  Table.parse (getQueryResultRaw m)

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
