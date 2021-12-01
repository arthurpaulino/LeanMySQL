namespace List

def reversed : List α → List α
  | nil      => nil
  | cons h t => (reversed t).concat h

end List

namespace Int

def toFloat : Int → Float
  | ofNat n   => n.toFloat
  | negSucc n => -n.toFloat - 1

end Int

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
      split.head! ++ "." ++ (if cleanR.isEmpty then "0" else cleanR)
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
  deriving Inhabited

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

open Lean

abbrev ColName := Name

namespace ColName

def toString (c : ColName) : String := Name.toString c

def length (c : ColName) : Nat := c.toString.length

def fromString (s : String) : ColName := Lean.Name.mkSimple s

instance : Coe String ColName where
  coe := ColName.fromString

end ColName

inductive DType
  | DInt
  | DFloat
  | DString
  | DAny
  deriving Inhabited, DecidableEq

namespace Entry

def toDType (e : Entry) : DType :=
  match e with
  | Entry.int e => DType.DInt
  | Entry.float e => DType.DFloat
  | Entry.str e => DType.DString
  | Entry.null => DType.DAny

def toFloatEntryIfIntEntry (e : Entry) : Entry :=
  match e with
  | Entry.int e => e.toFloat
  | _ => e

end Entry

abbrev Header := List (ColName × DType)

namespace Header

def colNames (header : Header) : List ColName :=
  header.map λ h => h.1

def colTypes (header : Header) : List DType :=
  header.map λ h => h.2

end Header

abbrev Col := List Entry

structure DataFrame where
  header : Header
  rows : List Row
  deriving Inhabited

namespace DataFrame

constant empty : DataFrame := ⟨[], []⟩

def setHeader (df : DataFrame) (header : Header) : DataFrame := do
  if df.rows.isEmpty then
    ⟨header, df.rows⟩
  else
    if df.header.length ≠ header.length then
      panic! "inconsistent header length"
    let mut invalidItems : List (ColName × ColName) := []
    for ((dfColName, dfDType), (colName, dType)) in df.header.zip header do
      if dfDType ≠ DType.DAny ∧ dfDType ≠ dType then
        invalidItems := invalidItems.concat (dfColName, colName)
    if ¬invalidItems.isEmpty then
      panic! s!"inconsistent types for old columns {invalidItems.map λ i => i.1} " ++
        s!"and new columns {invalidItems.map λ i => i.2}"
    else
      ⟨header, df.rows⟩

def addRow (df : DataFrame) (row : Row) : DataFrame := do
  if df.header.length ≠ row.length then
    panic! "inconsistent row size"
  let mut invalidItems : List (ColName × Entry) := []
  let mut newRow : Row := []
  for ((dfColName, dfDType), e) in df.header.zip row do
    let eDType := e.toDType
    if dfDType = eDType ∨ [dfDType, eDType].contains DType.DAny then
      newRow := newRow.concat e
    else
      if eDType = DType.DInt ∧ dfDType = DType.DFloat then
        newRow := newRow.concat e.toFloatEntryIfIntEntry
      else
        invalidItems := invalidItems.concat (dfColName, e)
  if ¬invalidItems.isEmpty then
    panic! s!"inconsistent types for old columns {invalidItems.map λ i => i.1} " ++
      s!"and entries {invalidItems.map λ i => i.2}"
  else
    ⟨df.header, df.rows.concat newRow⟩


def addRows (df : DataFrame) (rows : List Row) : DataFrame := do
  let mut df : DataFrame := df
  for row in rows do
    df := df.addRow row
  df

def new (colNames : List ColName) (colTypes : List DType) (rows : List Row := []) : DataFrame :=
  if colNames.length ≠ colTypes.length then
    panic! "columns names and types of different lengths"
  else
    (DataFrame.empty.setHeader (colNames.zip colTypes)).addRows rows

def nRows (df : DataFrame) : Nat :=
  df.rows.length

def nCols (df : DataFrame) : Nat :=
  df.header.length

def shape (df : DataFrame) : Nat × Nat :=
  (df.nRows, df.nCols)

def colNames (df : DataFrame) : List ColName :=
  df.header.colNames

def colTypes (df : DataFrame) : List DType :=
  df.header.colTypes

def row! (df : DataFrame) (i : Nat) : Row :=
  if i >= df.rows.length then
    panic! s!"invalid index {i}"
  else
    (df.rows.get! i)

def rows! (df : DataFrame) (li : List Nat) : List Row := do
  let mut invalidIndexes : List Nat := []
  for i in li do
    if i >= df.rows.length then
      invalidIndexes := invalidIndexes.concat i
  if ¬invalidIndexes.isEmpty then
    panic! s!"invalid indexes {invalidIndexes}"
  else
    li.map λ i => df.row! i

def col! (df : DataFrame) (j : Nat) : Col :=
  if j >= df.header.length then
    panic! s!"invalid index {j}"
  else
    df.rows.map λ r => r.get! j

def cols! (df : DataFrame) (lj : List Nat) : List Col := do
  let mut invalidIndexes : List Nat := []
  for j in lj do
    if j >= df.header.length then
      invalidIndexes := invalidIndexes.concat j
  if ¬invalidIndexes.isEmpty then
    panic! s!"invalid indexes {invalidIndexes}"
  else
    lj.map λ j => df.col! j

def at! (df : DataFrame) (i j : Nat) : Entry :=
  if i >= df.rows.length then
    panic! s!"invalid row index {i}"
  else
    if j >= df.header.length then
      panic! s!"invalid column index {j}"
    else
      (df.row! i).get! j

def toString (df : DataFrame) : String := do
  if df.nCols = 0 then
    ""
  else
    let mut cells : List (List String) := []
    let mut colLengths : List Nat := []
    let mut header : List ColName := []
    for colName in df.colNames do
      colLengths := colLengths.concat colName.length
      header := header.concat colName
    cells := cells.concat (header.map ColName.toString)
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
      if cells.length = 1 ∨ i < cells.length - 1 then
        res := res ++ "\n"
      if i = 0 then
        for j in [0 : row.length] do
          res := res ++ "|" ++ "".leftFillWithUntil '-' (colLengths.get! j)
        res := res ++ "|\n"
    res

instance : ToString DataFrame where
  toString df := df.toString

end DataFrame
