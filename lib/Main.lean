import Ffi

def main : IO Unit := do
  let mysql ← MySQL.mk
  mysql.connect "a" "b" "c" "d"
  mysql.close
