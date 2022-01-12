/-
  Copyright (c) 2021 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import DataFrame

@[extern "lean_mysql_initialize"]
constant initMySQL : BaseIO Unit

builtin_initialize initMySQL

def dataTypeMap := mapFromList
  [("i", DataType.TInt), ("f", DataType.TFloat), ("s", DataType.TString)]

def DataFrame.fromString (s : String) : DataFrame := Id.run do
  if s.isEmpty then DataFrame.empty
  else
    let typeSep : String := "^^"
    let colSep : String := "~~"
    let lineSep : String := "¨¨"
    let lines : List String := s.splitOn lineSep
    let mut header : Header := []
    for headerPart in lines.head!.splitOn colSep do
      let split : List String := headerPart.splitOn typeSep
      header := header.concat
        (dataTypeMap.find! split.getLast!, split.head!)
    let mut df : DataFrame := DataFrame.empty header
    for row in lines.tail! do
      let mut j : Nat := 0
      let mut rowData : List DataEntry := []
      let rowSplit := row.splitOn colSep
      for dataType in header.colTypes do
        let valString : String := rowSplit.get! j
        rowData := rowData.concat $ dataType.entryOfString! valString
        j := j + 1
      df := df.addRow rowData sorry -- no consistency guaranteed
    df

abbrev MySQLScheme := List (String × String)

def MySQLScheme.build (ts : MySQLScheme) : String :=
  s!"({",".intercalate (ts.map λ v => " ".intercalate [v.1, v.2])})"

def Row.build (r : Row) : String :=
  s!"({",".intercalate (r.toStrings)})"

constant MySQL : Type

def kb (b : UInt64) : UInt64 := 1024 * b

def mb (b : UInt64) : UInt64 := 1048576 * b

def gb (b : UInt64) : UInt64 := 1073741824 * b

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk (bufferSize : UInt64 := kb 8) : IO MySQL

@[extern "lean_mysql_set_buffer_size"]
constant setBufferSizeMB (bufferSize : UInt64) : IO Unit

@[extern "lean_mysql_version"]
constant version (m : MySQL) : String

@[extern "lean_mysql_login"]
constant login (m : MySQL) (h u p : String) : IO Unit

@[extern "lean_mysql_run"]
constant run (m : MySQL) (q : String) : IO Unit

def createDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("create database " ++ d)

def dropDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("drop database " ++ d)

def useDB (m : MySQL) (d : String) : IO Unit :=
  m.run ("use " ++ d)

def createTable (m : MySQL) (n : String) (ts : MySQLScheme) : IO Unit :=
  m.run ("create table " ++ (n ++ ts.build))

def dropTable (m : MySQL) (n : String) : IO Unit :=
  m.run ("drop table " ++ n)

def insertIntoTable (m : MySQL) (n : String) (r : Row) : IO Unit :=
  m.run s!"insert into {n} values{r.build}"

@[extern "lean_mysql_query"]
constant query (m : MySQL) (q : String) : IO Unit

@[extern "lean_mysql_get_query_result"]
constant getQueryResultBuffer (m : MySQL) : String

def getQueryResult (m : MySQL) : DataFrame :=
  DataFrame.fromString (getQueryResultBuffer m)

@[extern "lean_mysql_close"]
constant close (m : MySQL) : BaseIO Unit

end MySQL
