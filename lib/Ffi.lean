@[extern "lean_mysql_initialize"] constant initMySQL : IO Unit

builtin_initialize initMySQL

constant MySQL : Type

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk : IO MySQL

@[extern "lean_mysql_connect"]
constant connect (m : MySQL) (h u p d : String) : IO Unit

@[extern "lean_mysql_close"]
constant close (m : MySQL) : IO Unit

end MySQL
