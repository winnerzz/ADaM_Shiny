# auth_db.R
# 依赖包由 app.R 统一加载（DBI、RSQLite、sodium）

AUTH_DB_PATH <- Sys.getenv(
  "AUTH_DB_PATH",
  file.path(getwd(), "auth", "users.db")
)

auth_db_init <- function() {
  db_dir <- dirname(AUTH_DB_PATH)
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)

  con <- DBI::dbConnect(RSQLite::SQLite(), AUTH_DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS users (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      username      TEXT    UNIQUE NOT NULL,
      password_hash TEXT    NOT NULL,
      display_name  TEXT    NOT NULL DEFAULT '',
      email         TEXT    NOT NULL DEFAULT '',
      role          TEXT    NOT NULL DEFAULT 'user',
      created_at    TEXT    NOT NULL DEFAULT (datetime('now', 'localtime')),
      last_login    TEXT,
      is_active     INTEGER NOT NULL DEFAULT 1
    )
  ")

  count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
  if (count == 0) {
    admin_pw <- Sys.getenv("ADMIN_PASSWORD", "Admin@ADaM2024!")
    .auth_insert_user(
      con,
      username = "admin", password = admin_pw,
      display_name = "系统管理员", email = "", role = "admin"
    )
    message("[Auth] 默认管理员账号已创建 — 用户名: admin  密码: ", admin_pw)
  }
  invisible(TRUE)
}

.auth_con <- function() DBI::dbConnect(RSQLite::SQLite(), AUTH_DB_PATH)

.auth_insert_user <- function(con, username, password, display_name, email, role) {
  pw_hash <- sodium::password_store(password)
  DBI::dbExecute(
    con,
    "INSERT INTO users (username, password_hash, display_name, email, role) VALUES (?, ?, ?, ?, ?)",
    params = list(username, pw_hash, display_name, email, role)
  )
}

auth_db_create_user <- function(username, password, display_name = "", email = "", role = "user") {
  tryCatch({
    con <- .auth_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    existing <- DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM users WHERE username = ?",
      params = list(username)
    )$n
    if (existing > 0) return(list(ok = FALSE, msg = "该用户名已被注册，请换一个"))

    .auth_insert_user(con, username, password, display_name, email, role)
    list(ok = TRUE, msg = "注册成功")
    }, error = function(e) list(ok = FALSE, msg = paste0("注册失败：", conditionMessage(e))))
}

auth_db_verify_user <- function(username, password) {
  tryCatch({
    con <- .auth_con()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    row <- DBI::dbGetQuery(
      con,
      "SELECT id, password_hash, display_name, role, is_active FROM users WHERE username = ?",
      params = list(username)
    )
    if (nrow(row) == 0) return(list(ok = FALSE, msg = "用户名或密码错误"))
    if (row$is_active[1] == 0) return(list(ok = FALSE, msg = "账号已被禁用，请联系管理员"))

    ok <- tryCatch(sodium::password_verify(row$password_hash[1], password), error = function(e) FALSE)
    if (!ok) return(list(ok = FALSE, msg = "用户名或密码错误"))

    DBI::dbExecute(
      con,
      "UPDATE users SET last_login = datetime('now','localtime') WHERE username = ?",
      params = list(username)
    )

    list(
      ok = TRUE,
      msg = "登录成功",
      user = list(
        id = row$id[1], username = username, display_name = row$display_name[1], role = row$role[1]
      )
    )
  }, error = function(e) {
    list(ok = FALSE, msg = paste0("登录失败：", conditionMessage(e)))
  })
}

auth_db_list_users <- function() {
  con <- .auth_con()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbGetQuery(
    con,
    "SELECT id, username, display_name, email, role, created_at, last_login, is_active FROM users ORDER BY id"
  )
}

auth_db_set_active <- function(username, is_active) {
  con <- .auth_con()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(
    con,
    "UPDATE users SET is_active = ? WHERE username = ?",
    params = list(as.integer(is_active), username)
  )
}
