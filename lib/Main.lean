import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  mysql.login "localhost" "root" "root"
  -- mysql.close
  -- mysql.login "localhost" "root" "root"

  -- mysql.dropDB "test_db"
  -- mysql.createDB "test_db"

  mysql.useDB "test_db"

  -- mysql.createTable "car" [
  --     ⟨"id", "INT PRIMARY KEY"⟩,
  --     ⟨"name", "VARCHAR(255)"⟩
  --   ]
  -- mysql.insertIntoTable "car" [1, "A"]
  -- mysql.insertIntoTable "car" [2, "B"]
  -- mysql.insertIntoTable "car" [3, "C"]

  -- mysql.createTable "price" [
  --   ⟨"id", "INT PRIMARY KEY"⟩,
  --   ⟨"price", "INT"⟩
  -- ]
  -- mysql.insertIntoTable "price" [1, 3]
  -- mysql.insertIntoTable "price" [2, 2]
  -- mysql.insertIntoTable "price" [3, 1]
  let q := table "cars" ↠
    select [`id, `name] ↠
    join (table "prices") (`l.id = `r.id) "left" ↠
    filter (`price = 2)
  IO.println <| ← mysql.querySQL "select * from car"
