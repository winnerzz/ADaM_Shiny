# =============================================================================
# app.R
# ADaM 自动化生成平台 — 主程序入口
#
# 职责：
#   1. 安装并加载所有依赖 R 包
#   2. source() 引入各功能模块
#   3. 启动 Shiny 应用
#
# 运行方式（任选其一）：
#   ① 在 RStudio 中打开本文件，点击右上角「Run App」按钮
#   ② 在 R Console 中执行：shiny::runApp(".")
#   ③ 在终端中执行：  Rscript app.R
# =============================================================================

# =============================================================================
# 第一步：自动检查并安装缺失的依赖包
# 首次运行时会自动从 CRAN 安装，之后直接跳过
# =============================================================================
required_packages <- c(
  # ── Shiny 核心 ──────────────────────────────────────────────────────────────
  "shiny",       # Shiny 框架主体
  "bslib",       # Bootstrap 5 主题 + 现代 UI 组件（page_sidebar, value_box 等）
  "bsicons",     # Bootstrap Icons 图标库（配合 bslib 使用）

  # ── 表格与编辑器 ─────────────────────────────────────────────────────────────
  "DT",          # 交互式数据表格（DataTables JS 封装）
  "shinyAce",    # 代码编辑器组件（Ace Editor，支持语法高亮）
  "shinyjs",     # 前端状态控制（reset, runjs）

  # ── 数据处理 ─────────────────────────────────────────────────────────────────
  "dplyr",       # 数据框操作（mutate, filter, left_join 等）
  "readr",       # 高性能 CSV 读写（read_csv, write_csv）
  "lubridate",   # 日期时间解析与运算（ymd, as.Date 等）
  "stringr",     # 字符串处理（str_ends, str_replace 等）
  "tidyr",       # 数据整形（pivot_wider 等，供生成代码使用）

  # ── JSON 与 API ──────────────────────────────────────────────────────────────
  "jsonlite",    # JSON 解析与序列化（fromJSON, toJSON）
  "httr2",       # 现代 HTTP 请求框架（供 llm_api.R 实际调用 API 时使用）
  "digest",      # 生成稳定缓存键，用于复用相同 LLM 请求结果

  # ── 用户认证 ─────────────────────────────────────────────────────────────────
  "DBI",         # 数据库接口（统一 API，供 RSQLite 使用）
  "RSQLite",     # SQLite 驱动（本地用户数据库）
  "sodium"       # libsodium 密码哈希（argon2id，安全存储用户密码）
)

# 找出尚未安装的包
missing_pkgs <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

# [VPS 部署注意] 包安装由 Dockerfile 中 renv::restore() 完成，启动时不再自动安装。
# 如本地开发环境缺包，请手动执行 renv::restore() 或取消注释以下代码块：
# if (length(missing_pkgs) > 0) {
#   install.packages(missing_pkgs, repos = "https://cran.rstudio.com/",
#                    quiet = TRUE, dependencies = TRUE)
# }

# 加载所有包（suppressPackageStartupMessages 屏蔽启动信息，保持控制台整洁）
invisible(
  lapply(required_packages, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  })
)

message("═══════════════════════════════════════════════")
message("  所有依赖包加载完成 ✔")

# =============================================================================
# 第二步：引入功能模块
# local = FALSE（默认），使函数和变量进入全局环境，供 ui.R / server.R 共享
# =============================================================================

# 工具函数模块：load_sdtm_data(), load_spec_json(),
#               summarize_sdtm(), strip_excel_apos(), dy_char() 等
source("data_utils.R")

# 结果校验模块：validate_adam_datasets()
source("validation_utils.R")

# Derivation Plan 模块：normalize_derivation_plan(), validate_plan_against_spec()
source("derivation_plan_utils.R")

# 代码静态检查模块：run_code_static_checks()
source("code_static_checks.R")

# LLM API 模块：call_llm_engine(), format_risk_logs()
# 以及全局配置 MOCK_MODE, LLM_CONFIG, ACTIVE_PROVIDER
source("llm_api.R")

# 认证 UI 模块：auth_overlay_ui() / auth_user_badge_ui()（需在 ui.R 前 source）
source("auth_ui.R")

# UI 定义：生成 ui 对象（page_sidebar + 三个 Tab）
source("ui.R")

# Server 逻辑：定义 server 函数（观察者、响应式、输出渲染）
source("server.R")

# 域注册表：SDTM_DOMAIN_REGISTRY / DOMAIN_GROUP_LABELS / .get_required_domain_ids()
source("domain_registry.R")

message("  模块加载完成：data_utils ✔  validation_utils ✔  derivation_plan_utils ✔  code_static_checks ✔  llm_api ✔  ui ✔  server ✔  domain_registry ✔")
message("  启动应用...")
message("═══════════════════════════════════════════════")

# =============================================================================
# 第三步：启动 Shiny 应用
# =============================================================================
shinyApp(
  ui     = ui,
  server = server,

  # onStart：应用启动时执行一次（适合做环境检查）
  onStart = function() {
    # 检查模拟模式状态，提醒开发者
    if (exists("MOCK_MODE") && isTRUE(MOCK_MODE)) {
      message("  ⚠  LLM 处于【模拟模式】- 返回硬编码响应")
      message("     如需接入真实 API，请在 llm_api.R 中:")
      message("       1. 将 MOCK_MODE <- FALSE")
      message("       2. 设置环境变量（如 OPENAI_API_KEY）")
    } else {
      message("  ✔  LLM 使用真实 API（提供商: ",
              if (exists("ACTIVE_PROVIDER")) ACTIVE_PROVIDER else "未知", "）")
    }
  },

  # options：控制应用行为
  options = list(
    host = "0.0.0.0",  # 绑定所有网络接口（VPS/Docker 部署必须）
    port = 3838         # 固定端口，与 Dockerfile EXPOSE 及 Nginx 反代保持一致
  )
)
