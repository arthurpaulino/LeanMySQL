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

  let df ← mysql.query $
    SELECT name, age, height, job_name
    FROM person LEFT JOIN job ON person.job_id = job.id
    WHERE person.age > 20

  IO.println $ df

  mysql.close
```

The example above prints out:

```
8.0.28
|   name|age|height|       job_name|
|-------|---|------|---------------|
|  'Bob'| 21|  1.64|'Mathematician'|
|'Craig'| 22|  1.76|           NULL|
```

## What's next?

* Allow queries on the `FROM` clause (nested queries)
* Expand the implicit join so it can accept multiple sources, not just two

Feel free to contribute! :D
