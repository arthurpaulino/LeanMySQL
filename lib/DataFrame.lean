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

open DataType

inductive DataEntry
  | EInt (i : Int)
  | EFloat (f : Float)
  | EString (s : String)
  | NULL

def NULL := DataEntry.NULL

instance : Coe Int DataEntry where
  coe := DataEntry.EInt

instance : Coe Float DataEntry where
  coe := DataEntry.EFloat

instance : OfScientific DataEntry where
  ofScientific m s e := DataEntry.EFloat (OfScientific.ofScientific m s e)

instance : Coe String DataEntry where
  coe := DataEntry.EString

namespace DataEntry

@[simp] def isOf (e : DataEntry) (t : DataType) : Prop :=
  match e, t with
  | EInt _,    TInt    => True
  | EFloat _,  TFloat  => True
  | EString _, TString => True
  | NULL,      _       => True
  | _,         _       => False

protected def toString (e : DataEntry) : String := 
  match e with
  | EInt e    => toString e
  | EFloat e  => optimizeFloatString $ toString e
  | EString e => s!"'{e}'"
  | NULL      => "NULL"

instance : ToString DataEntry where
  toString e := e.toString

end DataEntry

abbrev Header := List (DataType × String)

def Header.colTypes (h : Header) : List DataType :=
  h.map fun x => x.1

def Header.colNames (h : Header) : List String :=
  h.map fun x => x.2

abbrev Row := List DataEntry

@[simp] def rowOfTypes : Row → List DataType → Prop
  | [],       []       => True
  | eh :: et, th :: tt => eh.isOf th ∧ rowOfTypes et tt
  | _,        _        => False

def Row.toStrings (r : Row) : List String :=
  r.map DataEntry.toString

@[simp] def rowsOfTypes : List Row → List DataType → Prop
  | row :: rows, types => rowOfTypes row types ∧ rowsOfTypes rows types
  | [],          _     => True

structure DataFrame where
  header     : Header 
  rows       : List Row
  consistent : rowsOfTypes rows header.colTypes := by simp

namespace DataFrame

def empty (header : Header := []) : DataFrame :=
  ⟨header, [], by simp⟩

theorem consistentConcatOfConsistentRow
    {df : DataFrame} (row : List DataEntry)
    (hc : rowOfTypes row df.header.colTypes) :
      rowsOfTypes (df.rows.concat row) (Header.colTypes df.header) :=
  match df with
    | ⟨_, rows, hr⟩ => by
      induction rows with
        | nil         => simp [hc]
        | cons _ _ hi => exact ⟨hr.1, hi hr.2 hc⟩

def addRow (df : DataFrame) (row : List DataEntry)
    (h : rowOfTypes row df.header.colTypes := by simp) : DataFrame :=
  ⟨df.header, df.rows.concat row, consistentConcatOfConsistentRow row h⟩

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

def toString (df : DataFrame) : String := Id.run do
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

def allPos : List Nat → Prop
  | [] => True
  | h :: t => 0 < h ∧ allPos t

structure Foo where
  data  : List Nat
  allPos : allPos data

theorem Bar {f : Foo} (n : Nat) (hl : 0 < n) :
    allPos (f.data.concat n) :=
  match f with
    | ⟨data, hh⟩ => by
      induction data with
        | nil =>
          simp [allPos]
          exact hl
        | cons h t hi =>
          simp [allPos]
          exact ⟨hh.1, hi hh.2⟩
