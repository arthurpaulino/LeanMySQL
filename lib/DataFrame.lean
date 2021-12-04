/-
  Copyright (c) 2021 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

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
    for i in rangeList.reverse do
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

inductive DataType
  | DInt
  | DFloat
  | DString
  | DAny
  deriving Inhabited, DecidableEq

namespace Entry

def toDataType (e : Entry) : DataType :=
  match e with
  | Entry.int e => DataType.DInt
  | Entry.float e => DataType.DFloat
  | Entry.str e => DataType.DString
  | Entry.null => DataType.DAny

def toFloatEntryIfIntEntry (e : Entry) : Entry :=
  match e with
  | Entry.int e => e.toFloat
  | _ => e

end Entry

abbrev Header := List (String × DataType)

namespace Header

def colNames (header : Header) : List String :=
  header.map λ h => h.1

def colTypes (header : Header) : List DataType :=
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
    let mut invalidItems : List (String × String) := []
    for ((dfColName, dfDataType), (colName, dataType)) in df.header.zip header do
      if dfDataType ≠ DataType.DAny ∧ dfDataType ≠ dataType then
        invalidItems := invalidItems.concat (dfColName, colName)
    if ¬invalidItems.isEmpty then
      panic! s!"inconsistent types for old columns {invalidItems.map λ i => i.1} " ++
        s!"and new columns {invalidItems.map λ i => i.2}"
    else
      ⟨header, df.rows⟩

def addRow (df : DataFrame) (row : Row) : DataFrame := do
  if df.header.length ≠ row.length then
    panic! "inconsistent row size"
  let mut invalidItems : List (String × Entry) := []
  let mut newRow : Row := []
  for ((dfColName, dfDataType), e) in df.header.zip row do
    let eDataType := e.toDataType
    if dfDataType = eDataType ∨ [dfDataType, eDataType].contains DataType.DAny then
      newRow := newRow.concat e
    else
      if eDataType = DataType.DInt ∧ dfDataType = DataType.DFloat then
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

def new (colNames : List String) (colTypes : List DataType) (rows : List Row := []) : DataFrame :=
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

def colNames (df : DataFrame) : List String :=
  df.header.colNames

def colTypes (df : DataFrame) : List DataType :=
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
