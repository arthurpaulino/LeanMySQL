@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

inductive Entry
  | str (s : String)
  | nat (n : Nat)
  | float (f : Float)

instance : Coe Nat Entry where
  coe := Entry.nat

instance : Coe String Entry where
  coe := Entry.str

instance : Coe Float Entry where
  coe := Entry.float

private constant entryListToStringList (l : List Entry) : List String :=
l.map λ e => match e with
  | Entry.str e => s!"'{e}'"
  | Entry.nat e => toString e
  | Entry.float e => toString e

structure Column where
  name : String
  type : String
  deriving Inhabited

namespace Column

private constant build (c : Column) : String :=
s!"{c.name} {c.type}"

end Column

abbrev TableScheme := List Column

namespace TableScheme

private constant build (ts : TableScheme) : String :=
s!"({",".intercalate (ts.map λ v => v.build)})"

end TableScheme

structure Query where
  Select : String
  From : String
  Where : String
  deriving Inhabited

namespace Query

private constant build (mq : Query) : String :=
s!"(SELECT {mq.Select} FROM {mq.From} WHERE {mq.Where})"

end Query

constant MySQL : Type

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk : IO MySQL

@[extern "lean_mysql_version"]
constant version (m : MySQL) : BaseIO String

@[extern "lean_mysql_login"]
constant login (m : MySQL) (h u p : String) : IO Unit

@[extern "lean_mysql_run"]
private constant run (m : MySQL) (q : String) : IO Unit

constant createDB (m : MySQL) (d : String) : IO Unit :=
m.run ("CREATE DATABASE " ++ d)

constant dropDB (m : MySQL) (d : String) : IO Unit :=
m.run ("DROP DATABASE " ++ d)

constant useDB (m : MySQL) (d : String) : IO Unit :=
m.run ("USE " ++ d)

constant createTable (m : MySQL) (n : String) (ts : TableScheme) : IO Unit :=
m.run ("CREATE TABLE " ++ (n ++ ts.build))

constant dropTable (m : MySQL) (n : String) : IO Unit :=
m.run ("DROP TABLE " ++ n)

constant insertIntoTable (m : MySQL) (n : String) (l : List Entry) : IO Unit :=
m.run s!"INSERT INTO {n} VALUES({",".intercalate (entryListToStringList l)})"

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
