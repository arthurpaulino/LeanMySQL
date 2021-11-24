import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  let ver ← mysql.version
  mysql.login "localhost" "root" "root"
  mysql.createDB "test_db"
  -- mysql.useDB "a"
  -- mysql.close
