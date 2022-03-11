/-
  Copyright (c) 2022 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import Utils

inductive DataEntry
  | EInt (i : Int)
  | EFloat (f : Float)
  | EString (s : String)
  | ENull
  deriving Inhabited

def NIL : DataEntry := DataEntry.ENull

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

/- The `String` representation of a `DataEntry` -/
protected def DataEntry.toString (e : DataEntry) : String := 
  match e with
  | EInt e    => toString e
  | EFloat e  => optimizeFloatString $ toString e
  | EString e => s!"'{e}'"
  | ENull     => "NULL"

instance : ToString DataEntry := ⟨DataEntry.toString⟩
