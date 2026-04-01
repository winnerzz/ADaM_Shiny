# =============================================================================
# auth_ui.R
# 用户认证界面 — 登录 / 注册 全屏覆盖层
# =============================================================================

auth_overlay_ui <- function() {
  tagList(
    tags$style(HTML("
      #auth-overlay-wrap {
        position: fixed; inset: 0;
        background: #0d1117;
        display: flex; align-items: center; justify-content: center;
        z-index: 9999;
      }
      .auth-card {
        background: #161b22;
        border: 1px solid #30363d;
        border-radius: 12px;
        padding: 2.4rem 2rem 2rem;
        width: 100%; max-width: 380px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.5);
      }
      .auth-logo { text-align: center; margin-bottom: 1.8rem; }
      .auth-logo-icon { font-size: 2.4rem; line-height: 1; display: inline-block; margin-bottom: 0.5rem; }
      .auth-logo h2 { color: #2dd4bf; font-size: 1.35rem; font-weight: 700; margin: 0 0 0.18rem; }
      .auth-logo p  { color: #6e7681; font-size: 0.75rem; margin: 0; letter-spacing: 0.03em; }
      .auth-field { margin-bottom: 0.95rem; }
      .auth-field > label {
        display: block; font-size: 0.72rem; font-weight: 600;
        color: #8b949e; text-transform: uppercase;
        letter-spacing: 0.07em; margin-bottom: 0.28rem;
      }
      #auth-overlay-wrap .form-control {
        background: #0d1117 !important;
        border: 1px solid #30363d !important;
        color: #e6edf3 !important;
        border-radius: 6px !important;
        font-size: 0.875rem !important;
        padding: 0.48rem 0.72rem !important;
        width: 100% !important;
        box-sizing: border-box;
        transition: border-color 0.18s, box-shadow 0.18s;
      }
      #auth-overlay-wrap .form-control:focus {
        border-color: #2dd4bf !important;
        box-shadow: 0 0 0 3px rgba(45,212,191,0.18) !important;
        outline: none !important;
      }
      #auth-overlay-wrap .form-control::placeholder { color: #484f58 !important; }
      #auth-overlay-wrap .shiny-input-container > label,
      #auth-overlay-wrap .form-group > label:first-child { display: none !important; }
      #auth-overlay-wrap .shiny-input-container,
      #auth-overlay-wrap .form-group { width: 100% !important; margin-bottom: 0 !important; }
      #auth-overlay-wrap .auth-btn-primary {
        width: 100%; padding: 0.58rem;
        background: #2dd4bf !important; color: #0d1117 !important;
        border: none !important; border-radius: 6px;
        font-size: 0.9rem; font-weight: 700;
        cursor: pointer; margin-top: 0.6rem;
        transition: background 0.18s, transform 0.1s;
        letter-spacing: 0.04em;
        box-shadow: none !important;
      }
      #auth-overlay-wrap .auth-btn-primary:hover  { background: #5eead4 !important; }
      #auth-overlay-wrap .auth-btn-primary:active { transform: scale(0.98); }
      #auth-overlay-wrap .auth-btn-primary:focus  { box-shadow: 0 0 0 3px rgba(45,212,191,0.25) !important; }
      .auth-switch {
        text-align: center; margin-top: 1.15rem;
        font-size: 0.79rem; color: #6e7681;
      }
      .auth-switch a { color: #2dd4bf; cursor: pointer; text-decoration: none; font-weight: 600; }
      .auth-switch a:hover { text-decoration: underline; }
      .auth-msg {
        font-size: 0.79rem; padding: 0.45rem 0.7rem;
        border-radius: 6px; margin-top: 0.65rem;
      }
      .auth-msg-error   { background: rgba(248,81,73,0.12); color: #f85149; border: 1px solid rgba(248,81,73,0.28); }
      .auth-msg-success { background: rgba(63,185,80,0.12);  color: #3fb950; border: 1px solid rgba(63,185,80,0.28); }
      .auth-divider { height: 1px; background: #21262d; margin: 0.5rem 0 0.85rem; }
    ")),

    div(id = "auth-overlay-wrap",
      div(class = "auth-card",
        div(class = "auth-logo",
          div(class = "auth-logo-icon", "\u2695\ufe0f"),
          tags$h2("ADaM Builder"),
          tags$p("Clinical Data Automation Platform")
        ),
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
        shinyjs::hidden(
          div(id = "auth-panel-register",
            div(class = "auth-field",
              tags$label("用户名"),
              textInput("reg_username", label = NULL, placeholder = "4-20 位，字母/数字/下划线", width = "100%")
            ),
            div(class = "auth-field",
              tags$label("显示名称"),
              textInput("reg_display_name", label = NULL, placeholder = "您的姓名或昵称", width = "100%")
            ),
            div(class = "auth-field",
              tags$label("邮箱（选填）"),
              textInput("reg_email", label = NULL, placeholder = "your@email.com", width = "100%")
            ),
            div(class = "auth-divider"),
            div(class = "auth-field",
              tags$label("密码"),
              passwordInput("reg_password", label = NULL, placeholder = "至少 8 位", width = "100%")
            ),
            div(class = "auth-field",
              tags$label("确认密码"),
              passwordInput("reg_password2", label = NULL, placeholder = "再次输入密码", width = "100%")
            ),
            uiOutput("auth_reg_msg"),
            actionButton("btn_register", "注  册  账  号", class = "auth-btn-primary", width = "100%")
          )
        ),
        div(id = "auth-switch-to-register", class = "auth-switch",
          "没有账号？",
          tags$a(onclick = "Shiny.setInputValue('auth_panel_switch','register',{priority:'event'})", "立即注册")
        ),
        shinyjs::hidden(
          div(id = "auth-switch-to-login", class = "auth-switch",
            "已有账号？",
            tags$a(onclick = "Shiny.setInputValue('auth_panel_switch','login',{priority:'event'})", "返回登录")
          )
        )
      )
    )
  )
}

auth_login_panel_ui <- auth_overlay_ui
