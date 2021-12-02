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

  mysql.query ("select name, age, height, job_name " ++
    "from person left join job on person.job_id = job.id")
  mysql.close
  IO.println $ mysql.getQueryResult
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

## DataFrame

This package (for now) also comes with a `DataFrame` and some functions for its usage.

It was implemented to serve as a rudimentary return of `getQueryResult`.

A better solution would be more similar to Python's `pandas`, using low level storage and manipulation
while providing a good higher level interface.

## What's next?

* Provide a better way to create tables and make queries without relying so much on freestyle
strings, which are prone to error;
* Plug in a more efficient data structure for query results;

Feel free to contribute! :D
