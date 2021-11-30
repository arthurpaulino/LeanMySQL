namespace List

def reversed : List α → List α
  | nil      => nil
  | cons h t => (reversed t).concat h

end List

namespace String

def withoutRightmostZeros (s : String) : String := do
  if s ≠ "" then
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
  else
    s

def optimizeFloatString (s : String) : String :=
  let split := s.splitOn "."
  let length := split.length
  if length = 1 then
    s
  else
    if length = 2 then
      let cleanR := split.getLast!.withoutRightmostZeros
      split.head! ++ "." ++ (if cleanR = "" then "0" else cleanR)
    else
      panic! "ill-formed float string"

def leftFillWithUntil (s : String) (f : Char) (n : Nat) : String := do
  let mut data : List Char := s.data
  for _ in [0 : n - s.length] do
    data := [f].append data
  ⟨data⟩

end String

inductive Entry
| int (n : Int)
| float (f : Float)
| str (s : String)
| null

constant NULL : Entry := Entry.null

instance : Coe Int Entry where
  coe := Entry.int

instance : Coe Float Entry where
  coe := Entry.float

instance : OfScientific Entry where
  ofScientific m s e := Entry.float (OfScientific.ofScientific m s e)

instance : Coe String Entry where
  coe := Entry.str

namespace Entry

protected def toString (e : Entry) : String := 
  match e with
  | Entry.int e => toString e
  | Entry.float e => (toString e).optimizeFloatString
  | Entry.str e => s!"'{e}'"
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

inductive DType
| DInt
| DFloat
| DString

abbrev Header := List (String × DType)

structure DataFrame where
  header : Header
  rows : List Row

namespace DataFrame

def shape (df : DataFrame) : Nat × Nat :=
  (df.rows.length, df.header.length)

def colNames (df : DataFrame) : List String :=
  df.header.map λ h => h.1

def colTypes (df : DataFrame) : List DType :=
  df.header.map λ h => h.2

def toString (df : DataFrame) : String := do
  let mut cells : List (List String) := []
  let mut colLengths : List Nat := []
  let mut header : List String := []
  for colName in df.colNames do
    colLengths := colLengths.concat colName.length
    header := header.concat colName
  cells := cells.concat header
  for row in df.rows do
    let mut line : List String := []
    let rowStrings : List String := row.toStrings
    for j in [0 : rowStrings.length] do
      let s := rowStrings.get! j
      let s_length : Nat := s.length
      if s_length > (colLengths.get! j) then
        colLengths := colLengths.set j s_length
      line := line.concat s
    cells := cells.concat line
  let mut res : String := ""
  for i in [0 : cells.length] do
    let row := cells.get! i
    for j in [0 : row.length] do
      let val : String := row.get! j
      res := res ++ "|" ++ (val.leftFillWithUntil ' ' (colLengths.get! j))
    res := res ++ "|"
    if i < cells.length - 1 then
      res := res ++ "\n"
    if i = 0 then
      for j in [0 : row.length] do
        res := res ++ "|" ++ "".leftFillWithUntil '-' (colLengths.get! j)
      res := res ++ "|\n"
  res

instance : ToString DataFrame where
  toString df := df.toString

end DataFrame
