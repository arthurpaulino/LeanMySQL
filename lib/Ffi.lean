import DataFrame

@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

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

namespace DataFrame

def fromString (s : String) : DataFrame := do
  if s.length = 0 then
    DataFrame.empty
  else
    let typeSep : String := "^^"
    let colSep : String := "~~"
    let lineSep : String := "¨¨"
    let lines : List String := s.splitOn lineSep
    let mut header : Header := []
    for headerPart in lines.head!.splitOn colSep do
      let split : List String := headerPart.splitOn typeSep
      header := header.concat (ColName.fromString split.head!, split.getLast!.toDType!)
    let maxI : Nat := lines.tail!.length
    let mut data : List Row := []
    for row in lines.tail! do
      let mut j : Nat := 0
      let mut rowData : List Entry := []
      let rowSplit := row.splitOn colSep
      for dType in header.map λ h => h.2 do
        let valString : String := rowSplit.get! j
        rowData := rowData.concat (valString.toEntry! dType)
        j := j + 1
      data := data.concat rowData
    ⟨header, data⟩

end DataFrame

abbrev MySQLScheme := List (String × String)

namespace MySQLScheme

def build (ts : MySQLScheme) : String :=
  s!"({",".intercalate (ts.map λ v => " ".intercalate [v.1, v.2])})"

end MySQLScheme

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

def createTable (m : MySQL) (n : String) (ts : MySQLScheme) : IO Unit :=
  m.run ("create table " ++ (n ++ ts.build))

def dropTable (m : MySQL) (n : String) : IO Unit :=
  m.run ("drop table " ++ n)

def insertIntoTable (m : MySQL) (n : String) (r : Row) : IO Unit :=
  m.run s!"insert into {n} values{r.build}"

@[extern "lean_mysql_query"]
constant query (m : MySQL) (q : String) : IO Unit

@[extern "lean_mysql_get_query_result"]
constant getQueryResultBuffer (m : MySQL) : String

def getQueryResult (m : MySQL) : DataFrame :=
  DataFrame.fromString (getQueryResultBuffer m)

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
