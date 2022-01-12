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
  ofScientific m s e := DataEntry.EFloat
    (OfScientific.ofScientific m s e)

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

@[simp] def dataOf : List DataEntry → DataType → Prop
  | [],      _ => True
  | e :: es, t => e.isOf t ∧ dataOf es t

structure Column where
  name       : String
  type       : DataType
  data       : List DataEntry
  consistent : dataOf data type

namespace Column

theorem consistentOfConcat
    {c : Column} (e : DataEntry) (hc : e.isOf c.type) :
      dataOf (c.data.concat e) c.type := by
  have hh := c.consistent
  induction c.data with
    | nil => exact And.intro hc ⟨⟩
    | cons h t hi =>
      simp only [dataOf] at *
      sorry

def push (c : Column) (e : DataEntry) (h : e.isOf c.type) :
    Column :=
  ⟨c.name, c.type, c.data.concat e, consistentOfConcat e h⟩

end Column

@[simp] def ofSameSize : List Column → Prop
  | c₁ :: c₂ :: cs =>
    c₁.data.length = c₂.data.length ∧ ofSameSize (c₂ :: cs)
  | _              => True

def lengthFst : List Column → Nat
  | c :: _ => c.data.length
  | _      => 0

@[simp] def entriesOfTypes : List DataEntry → List DataType → Prop
  | [],       []       => True
  | eh :: et, th :: tt => eh.isOf th ∧ entriesOfTypes et tt
  | _,        _        => False

structure DataFrame where
  columns    : List Column
  consistent : ofSameSize columns := by simp

namespace DataFrame

def colNames (df : DataFrame) : List String :=
  df.columns.map fun c => c.name

def colTypes (df : DataFrame) : List DataType :=
  df.columns.map fun c => c.type

def addRow (df : DataFrame) (es : List DataEntry)
    (hc : entriesOfTypes es df.colTypes) : DataFrame :=
  sorry

end DataFrame
