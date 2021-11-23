import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  let ver ← mysql.version
  mysql.login "localhost" "root" "root"
  mysql.createDB "a"
  mysql.useDB "a"
  mysql.close
