#include <lean/lean.h>
#include <mysql/mysql.h>
#include <string.h>
#include <stdio.h>

#define internal inline static
#define external extern "C"
#define l_arg b_lean_obj_arg
#define l_res lean_obj_res

typedef struct mysql {
    MYSQL* connection = NULL;
    char       logged = 0;
    int        status = 0;
    MYSQL_RES* result = NULL;
} mysql;

MYSQL_ROW row;

static lean_external_class* g_mysql_external_class = NULL;

internal lean_object* mysql_box(mysql* m) {
    return lean_alloc_external(g_mysql_external_class, m);
}

internal mysql* mysql_unbox(lean_object* p) {
    return (mysql*) (lean_get_external_data(p));
}

internal l_res make_error(const char* err_msg) {
    return lean_mk_io_user_error(lean_mk_io_user_error(lean_mk_string(err_msg)));
}

internal void close_connection(mysql* m) {
    mysql_close(m->connection);
    m->logged = 0;
    if (m->result) {
        free(m->result);
    }
}

internal void mysql_finalizer(void* mysql_ptr) {
    mysql* m = (mysql*) mysql_ptr;
    close_connection(m);
    if (m->connection) {
        free(m->connection);
    }
    free(m);
}

internal void noop_foreach(void* mod, l_arg fn) {}

internal void query_all(mysql* m, const char* q) {
    m->status = mysql_query(m->connection, q);
    m->result = mysql_store_result(m->connection);
}

internal void query_some(mysql* m, const char* q) {
    m->status = mysql_query(m->connection, q);
    m->result = mysql_use_result(m->connection);
}

internal l_res lean_mysql_manage_db(l_arg m_, l_arg q_) {
    mysql* m = mysql_unbox(m_);
    if (!m->logged) {
        return make_error("Not logged in.");
    }

    query_all(m, lean_string_cstr(q_));
    if (m->status) {
        return make_error(mysql_error(m->connection));
    }
    return lean_io_result_mk_ok(lean_box(0));
}

// API

external l_res lean_mysql_initialize() {
    g_mysql_external_class = lean_register_external_class(mysql_finalizer, noop_foreach);
    return lean_io_result_mk_ok(lean_box(0));
}

external l_res lean_mysql_mk() {
    mysql* m = (mysql*) malloc(sizeof(mysql));
    return lean_io_result_mk_ok(mysql_box(m));
}

external l_res lean_mysql_version() {
    return lean_io_result_mk_ok(lean_mk_string(mysql_get_client_info()));
}

external l_res lean_mysql_login(l_arg m_, l_arg h_, l_arg u_, l_arg p_) {
    mysql* m = mysql_unbox(m_);
    if (m->logged) {
        return make_error("Already logged in. Try using 'close' first.");
    }

    m->connection = mysql_init(NULL);
    if (m->connection == NULL) {
        return make_error("Failed to instantiate a connection with MySQL.");
    }

    const char* h = lean_string_cstr(h_);
    const char* u = lean_string_cstr(u_);
    const char* p = lean_string_cstr(p_);
    MYSQL *connection_ret = mysql_real_connect(
        m->connection,
        h, u, p,
        NULL, 0, NULL, 0
    );

    if (connection_ret == NULL) {
        return make_error(mysql_error(m->connection));
    }
    else {
        m->logged = 1;
        return lean_io_result_mk_ok(lean_box(0));
    }
}

external l_res lean_mysql_run(l_arg m_, l_arg q_) {
    return lean_mysql_manage_db(m_, q_);
}

external l_res lean_mysql_query(l_arg m_, l_arg q_) {
    mysql* m = mysql_unbox(m_);
    if (!m->logged) {
        return make_error("Not logged in.");
    }

    query_all(m, lean_string_cstr(q_));
    if (m->status) {
        return make_error(mysql_error(m->connection));
    }

    int num_fields = mysql_num_fields(m->result);
    MYSQL_ROW row;
    MYSQL_FIELD *field;

    while ((row = mysql_fetch_row(m->result))) {
        for(int i = 0; i < num_fields; i++) {
            if (i == 0) {
                while(field = mysql_fetch_field(m->result)) {
                    printf("%s\t", field->name);
                }
                printf("\n");
            }
            printf("%s\t", row[i] ? row[i] : "NULL");
        }
    }

    return lean_io_result_mk_ok(lean_mk_string(""));
}

external l_res lean_mysql_close(l_arg m_) {
    mysql* m = mysql_unbox(m_);
    close_connection(m);
    return lean_io_result_mk_ok(lean_box(0));
}
