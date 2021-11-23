#include <lean/lean.h>
#include <mysql.h>
#include <stdio.h>

typedef struct mysql {
    int status = 0;
    MYSQL* connection = NULL;
    MYSQL_RES* result = NULL;
} mysql;

static lean_external_class* g_mysql_external_class = NULL;

lean_object* mysql_box(mysql* m) {
    return lean_alloc_external(g_mysql_external_class, m);
}

mysql* mysql_unbox(lean_object* p) {
    return (mysql*) (lean_get_external_data(p));
}

inline static void close_connection(mysql* m) {
    mysql_close(m->connection);
    if (m->result) {
        free(m->result);
    }
}

inline static void mysql_finalizer(void* mysql_ptr) {
    mysql* m = (mysql*) mysql_ptr;
    close_connection(m);
    if (m->connection) {
        free(m->connection);
    }
    free(m);
}

inline static void noop_foreach(void* mod, b_lean_obj_arg fn) {}

extern "C" lean_obj_res lean_mysql_initialize() {
    g_mysql_external_class = lean_register_external_class(mysql_finalizer, noop_foreach);
    return lean_io_result_mk_ok(lean_box(0));
}

extern "C" lean_obj_res lean_mysql_mk() {
    mysql* m = (mysql*) malloc(sizeof(mysql));
    return lean_io_result_mk_ok(mysql_box(m));
}

extern "C" lean_obj_res lean_mysql_connect(
        b_lean_obj_arg m_,
        b_lean_obj_arg h_, b_lean_obj_arg u_, b_lean_obj_arg p_, b_lean_obj_arg d_
    ) {
    mysql* m = mysql_unbox(m_);
    m->connection = mysql_init(NULL);
    const char* h = lean_string_cstr(h_);
    const char* u = lean_string_cstr(u_);
    const char* p = lean_string_cstr(p_);
    const char* d = lean_string_cstr(d_);
    MYSQL *connection_ret = mysql_real_connect(
        m->connection,
        h, u, p, d,
        0, NULL, 0
    );
    if (connection_ret == NULL) {
        printf("%s\n", (char*) mysql_error(m->connection));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

extern "C" lean_obj_res lean_mysql_close(b_lean_obj_arg m_) {
    mysql* m = mysql_unbox(m_);
    close_connection(m);
    return lean_io_result_mk_ok(lean_box(0));
}
