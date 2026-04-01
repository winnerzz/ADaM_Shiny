# =============================================================================
# auth_ui.R
# 用户认证界面 — 嵌入式登录 / 注册卡片
#
# 导出函数：
#   auth_login_panel_ui() — 返回居中登录/注册卡片，作为主内容区占位符
#   auth_overlay_ui()     — 别名，向后兼容
# =============================================================================

auth_login_panel_ui <- function() {
  tagList(

    # ── 认证专属 CSS ──────────────────────────────────────────────────────────
    tags$style(HTML("
      /* 登录面板容器：占满主内容区，居中显示卡片 */
      #auth-login-panel-wrap {
        min-height: calc(100vh - 140px);
        background: #0d1117;
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 24px;
      }

      /* 认证卡片 */
      #auth-login-panel-wrap .auth-card {
        width: 100%;
        max-width: 420px;
        background: #161b22;
        border: 1px solid rgba(45, 212, 191, 0.22);
        border-radius: 18px;
        box-shadow: 0 18px 50px rgba(0, 0, 0, 0.35);
        padding: 2.4rem 2rem 2rem;
      }

      /* Logo 区 */
      .auth-logo { text-align: center; margin-bottom: 1.6rem; }
      .auth-logo-icon { font-size: 2.2rem; line-height: 1; display: inline-block; margin-bottom: 0.4rem; }
      .auth-logo h2 { color: #2dd4bf; font-size: 1.3rem; font-weight: 700; margin: 0 0 0.15rem; }
      .auth-logo p  { color: #6e7681; font-size: 0.74rem; margin: 0; letter-spacing: 0.03em; }

      /* 输入组自定义 label */
      .auth-field { margin-bottom: 0.9rem; }
      .auth-field > label {
        display: block; font-size: 0.71rem; font-weight: 600;
        color: #8b949e; text-transform: uppercase;
        letter-spacing: 0.07em; margin-bottom: 0.26rem;
      }

      /* 覆盖 Shiny textInput / passwordInput 样式 */
      #auth-login-panel-wrap .form-control {
        background: #0d1117 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
        border-radius: 8px !important;
        font-size: 0.875rem !important;
        padding: 0.48rem 0.72rem !important;
        height: 40px !important;
        width: 100% !important;
        box-sizing: border-box;
        transition: border-color 0.18s, box-shadow 0.18s;
      }
      #auth-login-panel-wrap .form-control:focus {
        border-color: #2dd4bf !important;
        box-shadow: 0 0 0 3px rgba(45, 212, 191, 0.18) !important;
        outline: none !important;
      }
      #auth-login-panel-wrap .form-control::placeholder { color: #484f58 !important; }

      /* 隐藏 Shiny 自动生成的 label（使用自定义 .auth-field > label） */
      #auth-login-panel-wrap .shiny-input-container > label,
      #auth-login-panel-wrap .form-group > label:first-child { display: none !important; }

      /* 输入容器全宽 */
      #auth-login-panel-wrap .shiny-input-container,
      #auth-login-panel-wrap .form-group { width: 100% !important; margin-bottom: 0 !important; }

      /* 主按钮（actionButton 覆盖） */
      #auth-login-panel-wrap .auth-btn-primary {
        width: 100%; height: 40px;
        background: #2dd4bf !important;
        color: #0d1117 !important;
        border: none !important;
        border-radius: 8px;
        font-size: 0.9rem; font-weight: 700;
        letter-spacing: 0.04em;
        cursor: pointer; margin-top: 0.55rem;
        transition: background 0.15s, transform 0.1s;
        box-shadow: none !important;
      }
      #auth-login-panel-wrap .auth-btn-primary:hover  { background: #5eead4 !important; }
      #auth-login-panel-wrap .auth-btn-primary:active { transform: scale(0.98); }
      #auth-login-panel-wrap .auth-btn-primary:focus  { box-shadow: 0 0 0 3px rgba(45,212,191,0.25) !important; }

      /* 切换链接 */
      .auth-switch {
        text-align: center; margin-top: 1.1rem;
        font-size: 0.79rem; color: #6e7681;
      }
      .auth-switch a {
        color: #2dd4bf; cursor: pointer;
        text-decoration: none; font-weight: 600;
      }
      .auth-switch a:hover { text-decoration: underline; }

      /* 消息提示 */
      .auth-msg {
        font-size: 0.79rem; padding: 0.42rem 0.68rem;
        border-radius: 6px; margin-top: 0.6rem;
      }
      .auth-msg-error   { background: rgba(248,81,73,0.12); color: #f85149; border: 1px solid rgba(248,81,73,0.28); }
      .auth-msg-success { background: rgba(63,185,80,0.12);  color: #3fb950; border: 1px solid rgba(63,185,80,0.28); }

      /* 分割线 */
      .auth-divider { height: 1px; background: #21262d; margin: 0.45rem 0 0.8rem; }
    ")),

    # ── 主容器 ────────────────────────────────────────────────────────────────
    div(id = "auth-login-panel-wrap",
      div(class = "auth-card",

        # Logo
        div(class = "auth-logo",
          div(class = "auth-logo-icon", "\u2695\ufe0f"),
          tags$h2("ADaM Builder"),
          tags$p("Clinical Data Automation Platform")
        ),

        # ── 登录面板 ──────────────────────────────────────────────────────────
        div(id = "auth-panel-login",
          div(class = "auth-field",
            tags$label("用户名"),
            textInput("auth_username", label = NULL,
                      placeholder = "输入用户名", width = "100%")
          ),
          div(class = "auth-field",
            tags$label("密码"),
            passwordInput("auth_password", label = NULL,
                          placeholder = "输入密码", width = "100%")
          ),
          uiOutput("auth_login_msg"),
          actionButton("btn_login", "登  录",
                       class = "auth-btn-primary",
                       width = "100%")
        ),

        # ── 注册面板（初始隐藏）────────────────────────────────────────────────
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
            actionButton("btn_register", "注  册  账  号",
                         class = "auth-btn-primary",
                         width = "100%")
          )
        ),

        # ── 面板切换链接 ───────────────────────────────────────────────────────
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

      ) # .auth-card
    ) # #auth-login-panel-wrap

  ) # tagList
}

# 向后兼容别名（ui.R 中仍引用此名，避免改两处）
auth_overlay_ui <- auth_login_panel_ui
