import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  IO.println $ mysql.version
  mysql.login "localhost" "root" "root"
  -- mysql.close
  -- mysql.login "localhost" "root" "root"

  -- mysql.createDB "test_db"
  -- mysql.dropDB "test_db"
  -- mysql.createDB "test_db"

  mysql.useDB "test_db"

  -- mysql.createTable "job" [
  --   ⟨"id", "INT PRIMARY KEY"⟩,
  --   ⟨"name", "VARCHAR(255)"⟩
  -- ]
  -- mysql.insertIntoTable "job" [1, "Computer Scientist"]
  -- mysql.insertIntoTable "job" [2, "Mathematician"]

  -- mysql.createTable "person" [
  --     ⟨"id", "INT PRIMARY KEY"⟩,
  --     ⟨"name", "VARCHAR(255)"⟩,
  --     ⟨"age", "INT"⟩,
  --     ⟨"height", "FLOAT"⟩,
  --     ⟨"job_id", "INT"⟩
  --   ]

  -- mysql.insertIntoTable "person" [1, "Alice", 20, 1.72, 1]
  -- mysql.insertIntoTable "person" [2, "Bob", 21, 1.64, 2]
  -- mysql.insertIntoTable "person" [3, "Craig", 22, 1.76, NULL]

  mysql.querySQL ("select person.name, person.age, person.height, job.name " ++
    "from person left join job on person.job_id = job.id")
  mysql.close
  IO.println $ mysql.getQueryResult
