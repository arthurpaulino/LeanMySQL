import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  mysql.login "localhost" "root" "root"
  -- mysql.close
  -- mysql.login "localhost" "root" "root"

  -- mysql.createDB "test_db"
  -- mysql.dropDB "test_db"
  -- mysql.createDB "test_db"

  mysql.useDB "test_db"
  -- let scheme : TableScheme := [
  --     ⟨"id", "INT PRIMARY KEY"⟩,
  --     ⟨"name", "VARCHAR(255)"⟩,
  --     ⟨"price", "INT"⟩
  --   ]
  -- mysql.createTable "cars" scheme
  -- mysql.dropTable "cars"
  -- mysql.createTable "cars" scheme

  mysql.insertIntoTable "cars" [3, "corolla", 100000]
  -- mysql.dropDB "test_db"
  -- let q := MySQLQuery.mk "a" "b" "c"
  -- mysql.close
