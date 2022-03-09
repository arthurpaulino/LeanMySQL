/-
  Copyright (c) 2021 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import DataFrame
import Std
import SQLSyntax

@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

/- Conventioned `DataType` of certain strings -/
open Std (HashMap) in
def dataTypeMap : HashMap String DataType :=
  HashMap.ofList
    [("i", DataType.TInt), ("f", DataType.TFloat), ("s", DataType.TString)]

/- Builds a `DataFrame` from a properly formated `String` -/
def DataFrame.fromString (s : String) : DataFrame := Id.run do
  if s.isEmpty then DataFrame.empty
  else
    let typeSep : String := "^^"
    let colSep  : String := "~~"
    let lineSep : String := "¨¨"
    let lines : List String := s.splitOn lineSep
    if h : lines = [] then DataFrame.empty
    else
      let mut header : Header := []
      for headerPart in (lines.head h).splitOn colSep do
        let split : List String := headerPart.splitOn typeSep
        header := header.concat (dataTypeMap.find! split.getLast!, split.head!)
      let mut df : DataFrame := DataFrame.empty header
      for row in lines.tailD [] do
        let entries := entriesOfStrings! df.colTypes $ row.splitOn colSep
        if ht : entries.ofTypes df.colTypes then
          df := df.addRow entries ht
        else
          panic! s!"inconsistent entries: {entries}"
      df

abbrev MySQLScheme := List (String × String)

def MySQLScheme.build (ts : MySQLScheme) : String :=
  s!"({",".intercalate (ts.map fun v => " ".intercalate [v.1, v.2])})"

def DataEntries.build (r : DataEntries) : String :=
  s!"({",".intercalate (r.toStrings)})"

constant MySQL : Type

def kb (b : UInt64) : UInt64 := 1024 * b

def mb (b : UInt64) : UInt64 := 1048576 * b

def gb (b : UInt64) : UInt64 := 1073741824 * b

namespace MySQL

/- Instantiates the object that provides access to MySQL API -/
@[extern "lean_mysql_mk"]
constant mk (bufferSize : UInt64 := kb 8) : IO MySQL

/- Sets the buffer size for queries -/
@[extern "lean_mysql_set_buffer_size"]
constant setBufferSizeMB (bufferSize : UInt64) : IO Unit

/- MySQL server version -/
@[extern "lean_mysql_version"]
constant version (m : MySQL) : String

/- Makes the login in the MySQL server -/
@[extern "lean_mysql_login"]
constant login (m : MySQL) (h u p : String) : IO Unit

@[extern "lean_mysql_run"]
private constant run (m : MySQL) (q : String) : IO Unit

/- Creates a new database -/
def createDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("create database " ++ d)

/- Drops a database -/
def dropDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("drop database " ++ d)

/- Sets a database to be used -/
def useDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("use " ++ d)

/- Creates a table in the currently used database -/
def createTable (m : MySQL) (n : String) (ts : MySQLScheme) : IO Unit :=
  m.run ("create table " ++ (n ++ ts.build))

/- Drops a table in the currently used database -/
def dropTable (m : MySQL) (n : String) : IO Unit :=
  m.run ("drop table " ++ n)

/- Inserts a row into a table -/
def insertIntoTable (m : MySQL) (n : String) (r : DataEntries) : IO Unit :=
  m.run s!"insert into {n} values{r.build}"

@[extern "lean_mysql_process_query_result"]
private constant processQueryResult (m : MySQL) : String

/- Runs an SQL query and returns a `DataFrame` with the results -/
def query (m : MySQL) (q : SQLQuery) : IO DataFrame := do
  m.run q.toString
  pure $ DataFrame.fromString (processQueryResult m)

/- Closes the connection with the MySQL server -/
@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
