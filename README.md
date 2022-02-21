# LeanMySQL

This Lean 4 package provides an API for the MySQL database.

Usage example:

```lean
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
```

The example above prints out:

```
8.0.27
|   name|age|height|            job_name|
|-------|---|------|--------------------|
|'Alice'| 20|  1.72|'Computer Scientist'|
|  'Bob'| 21|  1.64|     'Mathematician'|
|'Craig'| 22|  1.76|                NULL|
```

## What's next?

* Replace lists by arrays in the implementation of dataframes
* Provide a proper SQL DSL instead of relying on unsafe strings

Feel free to contribute! :D
