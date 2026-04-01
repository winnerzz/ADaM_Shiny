# =============================================================================
# auth_ui.R
# 用户认证界面 — Shiny modalDialog 实现
#
# 使用 Shiny 原生 modalDialog() 而非 position:fixed 覆盖层，
# 确保表单渲染在 <body> 最高层，不受 bslib 布局容器干扰，
# 键盘输入始终正常。
#
# 导出函数：
#   auth_modal_ui()   — 返回带登录/注册表单的 modalDialog
#   auth_overlay_ui() — 空占位，向后兼容（不再输出 HTML）
# =============================================================================

auth_modal_ui <- function() {
  modalDialog(
    title  = NULL,
    footer = NULL,
    easyClose = FALSE,
    size   = "s",

    # ── 模态框深色主题 CSS ──────────────────────────────────────────────────
    tags$style(HTML("
      /* 模态框本体 */
      .modal-content {
        background: #161b22 !important;
        border: 1px solid rgba(45,212,191,0.25) !important;
        border-radius: 12px !important;
        box-shadow: 0 20px 60px rgba(0,0,0,0.6) !important;
      }
      /* 遮罩层加深 */
      .modal-backdrop { opacity: 0.75 !important; }

      /* Logo */
      .auth-logo { text-align: center; margin-bottom: 1.6rem; }
      .auth-logo-icon { font-size: 2.2rem; line-height:1; display:inline-block; margin-bottom:0.4rem; }
      .auth-logo h2 { color:#2dd4bf; font-size:1.3rem; font-weight:700; margin:0 0 0.15rem; }
      .auth-logo p  { color:#6e7681; font-size:0.74rem; margin:0; letter-spacing:0.03em; }

      /* 自定义字段标签 */
      .auth-field { margin-bottom: 0.9rem; }
      .auth-field > label {
        display:block; font-size:0.71rem; font-weight:600;
        color:#8b949e; text-transform:uppercase;
        letter-spacing:0.07em; margin-bottom:0.26rem;
      }

      /* 覆盖模态框内 Shiny 输入框样式 */
      .modal .form-control {
        background: #0d1117 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
        border-radius: 6px !important;
        font-size: 0.875rem !important;
        padding: 0.48rem 0.72rem !important;
        height: 38px !important;
        transition: border-color 0.18s, box-shadow 0.18s;
      }
      .modal .form-control:focus {
        border-color: #2dd4bf !important;
        box-shadow: 0 0 0 3px rgba(45,212,191,0.18) !important;
        outline: none !important;
      }
      .modal .form-control::placeholder { color: #484f58 !important; }

      /* 隐藏 Shiny 自动生成的 label */
      .modal .shiny-input-container > label,
      .modal .form-group > label:first-child { display: none !important; }

      /* 输入容器全宽 */
      .modal .shiny-input-container,
      .modal .form-group { width: 100% !important; margin-bottom: 0 !important; }

      /* 主按钮 */
      .modal .auth-btn-primary {
        width:100%; padding:0.55rem;
        background:#2dd4bf !important; color:#0d1117 !important;
        border:none !important; border-radius:6px;
        font-size:0.9rem; font-weight:700;
        cursor:pointer; margin-top:0.55rem;
        transition:background 0.15s, transform 0.1s;
        letter-spacing:0.04em; box-shadow:none !important;
      }
      .modal .auth-btn-primary:hover  { background:#5eead4 !important; }
      .modal .auth-btn-primary:active { transform:scale(0.98); }
      .modal .auth-btn-primary:focus  { box-shadow:0 0 0 3px rgba(45,212,191,0.25) !important; }

      /* 切换链接 */
      .auth-switch {
        text-align:center; margin-top:1.1rem;
        font-size:0.79rem; color:#6e7681;
      }
      .auth-switch a { color:#2dd4bf; cursor:pointer; text-decoration:none; font-weight:600; }
      .auth-switch a:hover { text-decoration:underline; }

      /* 消息提示 */
      .auth-msg {
        font-size:0.79rem; padding:0.42rem 0.68rem;
        border-radius:6px; margin-top:0.6rem;
      }
      .auth-msg-error   { background:rgba(248,81,73,0.12); color:#f85149; border:1px solid rgba(248,81,73,0.28); }
      .auth-msg-success { background:rgba(63,185,80,0.12);  color:#3fb950; border:1px solid rgba(63,185,80,0.28); }

      /* 分割线 */
      .auth-divider { height:1px; background:#21262d; margin:0.45rem 0 0.8rem; }
    ")),

    # ── Logo ─────────────────────────────────────────────────────────────────
    div(class = "auth-logo",
      div(class = "auth-logo-icon", "\u2695\ufe0f"),
      tags$h2("ADaM Builder"),
      tags$p("Clinical Data Automation Platform")
    ),

    # ── 登录面板 ──────────────────────────────────────────────────────────────
    div(id = "auth-panel-login",
      div(class = "auth-field",
        tags$label("用户名"),
        textInput("auth_username", label = NULL, placeholder = "输入用户名", width = "100%")
      ),
      div(class = "auth-field",
        tags$label("密码"),
        passwordInput("auth_password", label = NULL, placeholder = "输入密码", width = "100%")
      ),
      uiOutput("auth_login_msg"),
      actionButton("btn_login", "登  录", class = "auth-btn-primary", width = "100%")
    ),

    # ── 注册面板（初始隐藏）──────────────────────────────────────────────────
    shinyjs::hidden(
      div(id = "auth-panel-register",
        div(class = "auth-field",
          tags$label("用户名"),
          textInput("reg_username", label = NULL,
                    placeholder = "4-20 位，字母/数字/下划线", width = "100%")
        ),
        div(class = "auth-field",
          tags$label("显示名称"),
          textInput("reg_display_name", label = NULL,
                    placeholder = "您的姓名或昵称", width = "100%")
        ),
        div(class = "auth-field",
          tags$label("邮箱（选填）"),
          textInput("reg_email", label = NULL,
                    placeholder = "your@email.com", width = "100%")
        ),
        div(class = "auth-divider"),
        div(class = "auth-field",
          tags$label("密码"),
          passwordInput("reg_password", label = NULL,
                        placeholder = "至少 8 位", width = "100%")
        ),
        div(class = "auth-field",
          tags$label("确认密码"),
          passwordInput("reg_password2", label = NULL,
                        placeholder = "再次输入密码", width = "100%")
        ),
        uiOutput("auth_reg_msg"),
        actionButton("btn_register", "注  册  账  号", class = "auth-btn-primary", width = "100%")
      )
    ),

    # ── 面板切换链接 ────────────────────────────────────────────────────────
    div(id = "auth-switch-to-register", class = "auth-switch",
      "没有账号？",
      tags$a(
        onclick = "Shiny.setInputValue('auth_panel_switch','register',{priority:'event'})",
        "立即注册"
      )
    ),
    shinyjs::hidden(
      div(id = "auth-switch-to-login", class = "auth-switch",
        "已有账号？",
        tags$a(
          onclick = "Shiny.setInputValue('auth_panel_switch','login',{priority:'event'})",
          "返回登录"
        )
      )
    )
  )
}

# 向后兼容占位（ui.R 中仍引用，返回空 tagList 不输出任何 HTML）
auth_overlay_ui      <- function() tagList()
auth_login_panel_ui  <- auth_overlay_ui
