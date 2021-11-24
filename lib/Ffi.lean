@[extern "lean_mysql_initialize"]
-- constant initMySQL : BaseIO Unit
constant initMySQL : IO Unit

builtin_initialize initMySQL

constant MySQLQuery : Type

constant MySQLQueryResult : Type

constant MySQL : Type

namespace MySQL

@[extern "lean_mysql_mk"]
constant mk : IO MySQL

@[extern "lean_mysql_version"]
-- constant version (m : MySQL) : BaseIO String
constant version (m : MySQL) : IO String

@[extern "lean_mysql_login"]
constant login (m : MySQL) (h u p : String) : IO Unit

@[extern "lean_mysql_create_db"]
constant createDB (m : MySQL) (d : String) : IO Unit

@[extern "lean_mysql_use_db"]
constant useDB (m : MySQL) (d : String) : IO Unit

@[extern "lean_mysql_close"]
-- constant close (m : MySQL) : BaseIO Unit
constant close (m : MySQL) : IO Unit

end MySQL
