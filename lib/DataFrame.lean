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

open Lean

abbrev ColName := Name

namespace ColName

def toString (c : ColName) : String := Name.toString c

def length (c : ColName) : Nat := c.toString.length

def fromString (s : String) : ColName := Lean.Name.mkSimple s

end ColName

private inductive ColProp
  | EqE (c : ColName) (e : Entry)
  | NeqE (c : ColName) (e : Entry)
  | LeE (c : ColName) (e : Entry)
  | LE (c : ColName) (e : Entry)
  | GeE (c : ColName) (e : Entry)
  | GE (c : ColName) (e : Entry)
  | EqC (c : ColName) (c' : ColName)
  | NeqC (c : ColName) (c' : ColName)
  | LeC (c : ColName) (c' : ColName)
  | LC (c : ColName) (c' : ColName)
  | GeC (c : ColName) (c' : ColName)
  | GC (c : ColName) (c' : ColName)
  | And (cp : ColProp) (cp' : ColProp)
  | Or (cp : ColProp) (cp' : ColProp)

namespace ColProp

declare_syntax_cat entry

syntax "#" noWs "{" term "}" : entry

syntax num : entry
syntax "-" noWs num : entry
syntax str : entry
syntax "NULL" : entry

macro "entry% " stx:entry : term =>
  match stx with
  | `(entry| #{$v}) => `(coe $v)
  | `(entry| $v:numLit) => `(Entry.int $v)
  | `(entry| -$v:numLit) => `(Entry.int (-$v))
  | `(entry| $v:strLit) => `(Entry.str $v)
  | `(entry| NULL) => `(Entry.null)
  | _ => Macro.throwErrorAt stx "ill-formed entry"

declare_syntax_cat colProp

syntax:50 ident " = " entry : colProp
syntax:50 ident " ≠ " entry : colProp
syntax:50 ident " ≤ " entry : colProp
syntax:50 ident " < " entry : colProp
syntax:50 ident " ≥ " entry : colProp
syntax:50 ident " > " entry : colProp
syntax:50 ident " = " ident : colProp
syntax:50 ident " ≠ " ident : colProp
syntax:50 ident " ≤ " ident : colProp
syntax:50 ident " < " ident : colProp
syntax:50 ident " ≥ " ident : colProp
syntax:50 ident " > " ident : colProp
syntax:25 colProp:26 " ∧ " colProp:26 : colProp
syntax:25 colProp:26 " ∨ " colProp:26 : colProp
syntax:max "(" colProp ")" : colProp

syntax "colProp% " colProp : term

macro_rules
| `(colProp% $stx) =>
  match stx with
  | `(colProp| $x:ident = $y:entry) => `(ColProp.EqE $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident ≠ $y:entry) => `(ColProp.NeE $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident ≤ $y:entry) => `(ColProp.LeE $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident < $y:entry) => `(ColProp.LE  $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident ≥ $y:entry) => `(ColProp.GeE $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident > $y:entry) => `(ColProp.GE  $(quote x.getId) (entry% $y))
  | `(colProp| $x:ident = $y:ident) => `(ColProp.EqC $(quote x.getId) $(quote y.getId))
  | `(colProp| $x:ident ≠ $y:ident) => `(ColProp.NeC $(quote x.getId) $(quote y.getId))
  | `(colProp| $x:ident ≤ $y:ident) => `(ColProp.LeC $(quote x.getId) $(quote y.getId))
  | `(colProp| $x:ident < $y:ident) => `(ColProp.LC  $(quote x.getId) $(quote y.getId))
  | `(colProp| $x:ident ≥ $y:ident) => `(ColProp.GeC $(quote x.getId) $(quote y.getId))
  | `(colProp| $x:ident > $y:ident) => `(ColProp.GC  $(quote x.getId) $(quote y.getId))
  | `(colProp| $x ∧ $y) => `(ColProp.And (colProp% $x) (colProp% $y))
  | `(colProp| $x ∨ $y) => `(ColProp.Or  (colProp% $x) (colProp% $y))
  | `(colProp| ($x)) => `(colProp% $x)
  | _ => Macro.throwErrorAt stx "ill-formed column proposition"

end ColProp

inductive DType
| DInt
| DFloat
| DString

abbrev Header := List (ColName × DType)

structure DataFrame where
  header : Header
  rows : List Row

namespace DataFrame

def empty : DataFrame := ⟨[], []⟩

def shape (df : DataFrame) : Nat × Nat :=
  (df.rows.length, df.header.length)

def colNames (df : DataFrame) : List ColName :=
  df.header.map λ h => h.1

def colTypes (df : DataFrame) : List DType :=
  df.header.map λ h => h.2

def toString (df : DataFrame) : String := do
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
    if i < cells.length - 1 then
      res := res ++ "\n"
    if i = 0 then
      for j in [0 : row.length] do
        res := res ++ "|" ++ "".leftFillWithUntil '-' (colLengths.get! j)
      res := res ++ "|\n"
  res

instance : ToString DataFrame where
  toString df := df.toString

def transform (df : DataFrame) (f : DataFrame → DataFrame) : DataFrame := f df

def filter (df : DataFrame) (cp : ColProp) : DataFrame := sorry

#check DataFrame.empty.filter (colProp% a = 2)
-- filter empty (ColProp.EqE `a (Entry.int 2)) : DataFrame

-- def select (df : DataFrame) : DataFrame :=


end DataFrame
