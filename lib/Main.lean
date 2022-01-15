/-
  Copyright (c) 2021 Arthur Paulino. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Arthur Paulino
-/

import LeanMySQL

def main : IO Unit := do
  let mysql ← MySQL.mk
  IO.println $ mysql.version
  mysql.login "localhost" "root" "root"
  mysql.createDB "test_db"
  mysql.useDB "test_db"

  mysql.createTable "job" [
    ("id", "INT PRIMARY KEY"),
    ("job_name", "VARCHAR(255)")
  ]
  mysql.insertIntoTable "job" [1, "Computer Scientist"]
  mysql.insertIntoTable "job" [2, "Mathematician"]

  mysql.createTable "person" [
      ("id", "INT PRIMARY KEY"),
      ("name", "VARCHAR(255)"),
      ("age", "INT"),
      ("height", "FLOAT"),
      ("job_id", "INT"),
      ("country_id", "INT")
    ]
  mysql.insertIntoTable "person" [1, "Alice", 20, 1.72, 1, 1]
  mysql.insertIntoTable "person" [2, "Bob", 21, 1.64, 2, 3]
  mysql.insertIntoTable "person" [3, "Craig", 22, 1.76, NULL, 2]

  let df ← mysql.query ("select name, age, height, job_name " ++
    "from person left join job on person.job_id = job.id")

  IO.println $ df

  mysql.close
