/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Utils

inductive DataType
  | TInt
  | TFloat
  | TString
  deriving Inhabited

open DataType

inductive DataEntry
  | EInt (i : Int)
  | EFloat (f : Float)
  | EString (s : String)
  | ENull
  deriving Inhabited

def NULL := DataEntry.ENull

instance : OfNat DataEntry n where
  ofNat := DataEntry.EInt (Int.ofNat n)

instance : Coe Int DataEntry where
  coe := DataEntry.EInt

instance : Coe Float DataEntry where
  coe := DataEntry.EFloat

instance : Neg DataEntry where
  neg e := match e with
  | DataEntry.EInt   i => ((-1 : Int) * i : Int)
  | DataEntry.EFloat f => ((-1 : Float) * f : Float)
  | _                  => panic! "invalid DataEntry"

instance : OfScientific DataEntry where
  ofScientific m s e := DataEntry.EFloat (OfScientific.ofScientific m s e)

instance : Coe String DataEntry where
  coe := DataEntry.EString

/- Prouces a `DataEntry` given its `DataType` and a `String` -/
def DataType.entryOfString! (dataType : DataType) (s : String) : DataEntry :=
  if s = "NULL" then NULL
  else match dataType with
  | DataType.TInt    => s.toInt!
  | DataType.TFloat  => toFloat! s
  | DataType.TString => s

/- Whether a `DataEntry` is of a `DataType` or not -/
@[simp] def DataEntry.ofType : DataEntry → DataType → Bool
  | EInt _,    TInt    => true
  | EFloat _,  TFloat  => true
  | EString _, TString => true
  | ENull,     _       => true
  | _,         _       => false

/- The `String` representation of a `DataEntry` -/
protected def DataEntry.toString (e : DataEntry) : String := 
  match e with
  | EInt e    => toString e
  | EFloat e  => optimizeFloatString $ toString e
  | EString e => s!"'{e}'"
  | ENull     => "NULL"

instance : ToString DataEntry where
  toString e := e.toString

abbrev Header := List (DataType × String)

/- Returns the column types of a `Header` -/
def Header.colTypes (h : Header) : List DataType :=
  h.map fun x => x.1

/- Returns the column names of a `Header` -/
def Header.colNames (h : Header) : List String :=
  h.map fun x => x.2

abbrev DataEntries := List DataEntry

/- Given a list of `DataEntry` and a list of `DataType`, tells whether
  every `DataEntry` is of `DataType` in a "zip" logic -/
@[simp] def DataEntries.ofTypes : DataEntries → List DataType → Bool
  | e :: es, t :: ts => e.ofType t && ofTypes es ts
  | [],      []      => true
  | _,       _       => false

/-- Given a list of `DataType`, turns a list of `String` into a list of
  `DataEntry` according to the respective type from the list -/
def entriesOfStrings! : List DataType → List String → DataEntries
  | t :: ts, s :: ss => t.entryOfString! s :: (entriesOfStrings! ts ss)
  | _,       _       => []

/- Whether every list of `DataEntry` obeys to `DataEntries.ofTypes` -/
@[simp] def rowsOfTypes : List DataEntries → List DataType → Prop
  | row :: rows, types => row.ofTypes types ∧ rowsOfTypes rows types
  | [],          _     => True

/- Turns a list of `DataEntry` into a list of their respective `String`
  representation-/
def DataEntries.toStrings (r : DataEntries) : List String :=
  r.map DataEntry.toString

/- A DataFrame consists of:
  * A header, containing the column names and their types
  * The rows, containing the actual data
  * A consistenty rule, guaranteeing that every row obeys to the scheme -/
structure DataFrame where
  header     : Header 
  rows       : List DataEntries
  consistent : rowsOfTypes rows header.colTypes := by simp

namespace DataFrame

/- The column types of a `DataFrame` -/
def colTypes (df : DataFrame) : List DataType :=
  df.header.colTypes

/- The column names of a `DataFrame` -/
def colNames (df : DataFrame) : List String :=
  df.header.colNames

/- Returns an empty `DataFrame` -/
def empty (header : Header := []) : DataFrame :=
  ⟨header, [], by simp⟩

/- Given a `DataFrame` `df` and a new `Row` `r` that's consistent with its
  scheme, the concatenation of the `df.rows` and `r` is also consistent
  with the scheme of `df` -/
theorem consistentConcatOfConsistentRow
    {df : DataFrame} (row : DataEntries)
    (hc : row.ofTypes df.colTypes) :
      rowsOfTypes (df.rows.concat row) (Header.colTypes df.header) :=
  match df with
    | ⟨_, rows, hr⟩ => by
      induction rows with
        | nil         => simp only [colTypes] at hc; simp [hc]
        | cons _ _ hi => exact ⟨hr.1, hi hr.2 hc⟩

/- Adds a new row on a `DataFrame` -/
def addRow (df : DataFrame) (row : DataEntries)
    (h : row.ofTypes df.colTypes := by simp) : DataFrame :=
  ⟨df.header, df.rows.concat row, consistentConcatOfConsistentRow row h⟩

/- The number of rows in a `DataFrame` -/
def nRows (df : DataFrame) : Nat :=
  df.rows.length

/- The number of columns in a `DataFrame` -/
def nCols (df : DataFrame) : Nat :=
  df.header.length

/- The shape of a `DataFrame` (# of rows × # of columns) -/
def shape (df : DataFrame) : Nat × Nat :=
  (df.nRows, df.nCols)

/- The i-th row of a `DataFrame` -/
def row! (df : DataFrame) (i : Nat) : DataEntries :=
  if i >= df.rows.length then
    panic! s!"invalid index {i}"
  else
    (df.rows.get! i)

/- The i-th's rows of a `DataFrame` -/
def rows! (df : DataFrame) (li : List Nat) : List DataEntries := Id.run do
  let mut invalidIndexes : List Nat := []
  for i in li do
    if i >= df.rows.length then
      invalidIndexes := invalidIndexes.concat i
  if ¬invalidIndexes.isEmpty then
    panic! s!"invalid indexes {invalidIndexes}"
  else
    li.map fun i => df.row! i

/- The j-th column of a `DataFrame` -/
def col! (df : DataFrame) (j : Nat) : DataEntries :=
  if j >= df.header.length then
    panic! s!"invalid index {j}"
  else
    df.rows.map fun r => r.get! j

/- The j-th's columns of a `DataFrame` -/
def cols! (df : DataFrame) (lj : List Nat) : List DataEntries := Id.run do
  let mut invalidIndexes : List Nat := []
  for j in lj do
    if j >= df.header.length then
      invalidIndexes := invalidIndexes.concat j
  if ¬invalidIndexes.isEmpty then
    panic! s!"invalid indexes {invalidIndexes}"
  else
    lj.map fun j => df.col! j

/- The element at the i-th row and j-th column -/
def at! (df : DataFrame) (i j : Nat) : DataEntry :=
  if i >= df.rows.length then
    panic! s!"invalid row index {i}"
  else
    if j >= df.header.length then
      panic! s!"invalid column index {j}"
    else
      (df.row! i).get! j

/- The `String` representation of a `DataFrame` -/
def toString (df : DataFrame) : String := Id.run do
  if df.nCols = 0 then ""
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
        res := res ++ "|" ++
          (leftFillWithUntil val ' ' (colLengths.get! j))
      res := res ++ "|"
      if cells.length = 1 ∨ i < cells.length - 1 then
        res := res ++ "\n"
      if i = 0 then
        for j in [0 : row.length] do
          res := res ++ "|" ++
            leftFillWithUntil "" '-' (colLengths.get! j)
        res := res ++ "|\n"
    res

instance : ToString DataFrame where
  toString df := df.toString

end DataFrame
