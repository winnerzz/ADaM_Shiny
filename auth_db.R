# =============================================================================
# auth_db.R
# 用户认证数据库模块
#
# 使用 DBI + RSQLite + sodium 实现安全的本地用户管理
# 密码使用 libsodium argon2id 算法哈希，不可逆
#
# 公共函数：
#   auth_db_init()           — 初始化数据库（首次启动自动建表 + 创建 admin）
#   auth_db_create_user(...) — 注册新用户，返回 list(ok, msg)
#   auth_db_verify_user(...) — 验证登录，返回 list(ok, msg, user)
#   auth_db_list_users()     — 列出所有用户（管理员用）
# =============================================================================

library(DBI)
library(RSQLite)
library(sodium)

# ── 数据库路径（通过环境变量覆盖，便于 Docker 挂载） ─────────────────────────
AUTH_DB_PATH <- Sys.getenv(
  "AUTH_DB_PATH",
  file.path(getwd(), "auth", "users.db")
)

# ── 初始化数据库 ──────────────────────────────────────────────────────────────
auth_db_init <- function() {
  db_dir <- dirname(AUTH_DB_PATH)
  if (!dir.exists(db_dir)) dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)

  con <- dbConnect(RSQLite::SQLite(), AUTH_DB_PATH)
  on.exit(dbDisconnect(con))

  dbExecute(con, "
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

  # 首次启动：若表为空则创建默认管理员账号
  count <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM users")$n
  if (count == 0) {
    admin_pw <- Sys.getenv("ADMIN_PASSWORD", "Admin@ADaM2024!")
    .auth_insert_user(con,
      username     = "admin",
      password     = admin_pw,
      display_name = "系统管理员",
      email        = "",
      role         = "admin"
    )
    message("[Auth] 默认管理员账号已创建 — 用户名: admin  密码: ", admin_pw,
            "\n[Auth] 请登录后及时修改密码！")
  }

  invisible(TRUE)
}

# ── 内部：打开连接 ────────────────────────────────────────────────────────────
.auth_con <- function() dbConnect(RSQLite::SQLite(), AUTH_DB_PATH)

# ── 内部：向已有连接插入用户（避免重复开连接） ───────────────────────────────
.auth_insert_user <- function(con, username, password, display_name, email, role) {
  pw_hash <- sodium::password_store(chartr("", "", password))
  dbExecute(con,
    "INSERT INTO users (username, password_hash, display_name, email, role)
     VALUES (?, ?, ?, ?, ?)",
    params = list(username, pw_hash, display_name, email, role)
  )
}

# ── 注册新用户 ────────────────────────────────────────────────────────────────
# 返回 list(ok = TRUE/FALSE, msg = "...")
auth_db_create_user <- function(username, password,
                                display_name = "", email = "",
                                role = "user") {
  con <- .auth_con()
  on.exit(dbDisconnect(con))

  existing <- dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM users WHERE username = ?",
    params = list(username))$n
  if (existing > 0) return(list(ok = FALSE, msg = "该用户名已被注册，请换一个"))

  tryCatch({
    .auth_insert_user(con, username, password, display_name, email, role)
    list(ok = TRUE, msg = "注册成功")
  }, error = function(e) {
    list(ok = FALSE, msg = paste0("注册失败：", conditionMessage(e)))
  })
}

# ── 验证登录 ──────────────────────────────────────────────────────────────────
# 返回：
#   失败 → list(ok = FALSE, msg = "...")
#   成功 → list(ok = TRUE,  msg = "登录成功",
#               user = list(id, username, display_name, role))
auth_db_verify_user <- function(username, password) {
  con <- .auth_con()
  on.exit(dbDisconnect(con))

  row <- dbGetQuery(con,
    "SELECT id, password_hash, display_name, role, is_active
       FROM users WHERE username = ?",
    params = list(username))

  if (nrow(row) == 0)
    return(list(ok = FALSE, msg = "用户名或密码错误"))
  if (row$is_active[1] == 0)
    return(list(ok = FALSE, msg = "账号已被禁用，请联系管理员"))

  ok <- tryCatch(
    sodium::password_verify(row$password_hash[1], password),
    error = function(e) FALSE
  )
  if (!ok) return(list(ok = FALSE, msg = "用户名或密码错误"))

  dbExecute(con,
    "UPDATE users SET last_login = datetime('now','localtime') WHERE username = ?",
    params = list(username))

  list(
    ok   = TRUE,
    msg  = "登录成功",
    user = list(
      id           = row$id[1],
      username     = username,
      display_name = row$display_name[1],
      role         = row$role[1]
    )
  )
}

# ── 列出所有用户（管理员面板用） ──────────────────────────────────────────────
auth_db_list_users <- function() {
  con <- .auth_con()
  on.exit(dbDisconnect(con))
  dbGetQuery(con,
    "SELECT id, username, display_name, email, role,
            created_at, last_login, is_active
       FROM users ORDER BY id")
}

# ── 启用/禁用用户（管理员用） ─────────────────────────────────────────────────
auth_db_set_active <- function(username, is_active) {
  con <- .auth_con()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "UPDATE users SET is_active = ? WHERE username = ?",
    params = list(as.integer(is_active), username))
}
