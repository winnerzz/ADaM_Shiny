# =============================================================================
# auth_ui.R
# 用户认证界面 — 登录 / 注册 全屏覆盖层
#
# 导出函数：
#   auth_overlay_ui() — 返回覆盖整个视口的认证卡片 tagList
#
# 设计：
#   • position:fixed + z-index:9999，视觉上覆盖主应用
#   • 同一卡片内切换"登录"/"注册"面板（shinyjs show/hide）
#   • 风格与主应用 GitHub Dark 主题完全一致
# =============================================================================

auth_overlay_ui <- function() {
  tagList(

    # ── 认证专属 CSS ──────────────────────────────────────────────────────────
    tags$style(HTML("
      /* 全屏遮罩 */
      #auth-overlay-wrap {
        position: fixed; inset: 0;
        background: #0d1117;
        display: flex; align-items: center; justify-content: center;
        z-index: 9999;
      }

      /* 认证卡片 */
      .auth-card {
        background: #161b22;
        border: 1px solid #30363d;
        border-radius: 12px;
        padding: 2.4rem 2rem 2rem;
        width: 100%; max-width: 380px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.5);
      }

      /* Logo 区 */
      .auth-logo { text-align: center; margin-bottom: 1.8rem; }
      .auth-logo-icon {
        font-size: 2.4rem; line-height: 1;
        display: inline-block; margin-bottom: 0.5rem;
      }
      .auth-logo h2 {
        color: #2dd4bf; font-size: 1.35rem;
        font-weight: 700; margin: 0 0 0.18rem;
      }
      .auth-logo p {
        color: #6e7681; font-size: 0.75rem; margin: 0;
        letter-spacing: 0.03em;
      }

      /* 输入组 */
      .auth-field { margin-bottom: 0.95rem; }
      .auth-field label {
        display: block; font-size: 0.72rem; font-weight: 600;
        color: #8b949e; text-transform: uppercase;
        letter-spacing: 0.07em; margin-bottom: 0.28rem;
      }
      /* 覆盖 Shiny 默认 textInput / passwordInput 样式 */
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
      /* 隐藏 Shiny 默认 label（我们用自定义 label） */
      #auth-overlay-wrap .form-group > label:first-child,
      #auth-overlay-wrap .shiny-input-container > label { display: none !important; }
      /* 全宽 */
      #auth-overlay-wrap .shiny-input-container,
      #auth-overlay-wrap .form-group { width: 100% !important; margin-bottom: 0 !important; }

      /* 按钮 */
      .auth-btn-primary {
        width: 100%; padding: 0.58rem;
        background: #2dd4bf; color: #0d1117;
        border: none; border-radius: 6px;
        font-size: 0.9rem; font-weight: 700;
        cursor: pointer; margin-top: 0.6rem;
        transition: background 0.18s, transform 0.1s;
        letter-spacing: 0.04em;
      }
      .auth-btn-primary:hover  { background: #5eead4; }
      .auth-btn-primary:active { transform: scale(0.98); }

      /* 底部切换链接 */
      .auth-switch {
        text-align: center; margin-top: 1.15rem;
        font-size: 0.79rem; color: #6e7681;
      }
      .auth-switch a {
        color: #2dd4bf; cursor: pointer;
        text-decoration: none; font-weight: 600;
      }
      .auth-switch a:hover { text-decoration: underline; }

      /* 消息提示 */
      .auth-msg {
        font-size: 0.79rem; padding: 0.45rem 0.7rem;
        border-radius: 6px; margin-top: 0.65rem;
      }
      .auth-msg-error   { background: rgba(248,81,73,0.12); color: #f85149; border: 1px solid rgba(248,81,73,0.28); }
      .auth-msg-success { background: rgba(63,185,80,0.12); color: #3fb950; border: 1px solid rgba(63,185,80,0.28); }

      /* 分割线 */
      .auth-divider {
        height: 1px; background: #21262d; margin: 0.5rem 0 0.85rem;
      }

      /* 隐藏 Shiny busy 提示（认证过程中不需要） */
      #auth-overlay-wrap .shiny-busy-message { display: none !important; }
    ")),

    # ── 全屏覆盖层 ─────────────────────────────────────────────────────────────
    div(id = "auth-overlay-wrap",

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
            textInput("auth_username", label = NULL, placeholder = "输入用户名",
                      width = "100%")
          ),
          div(class = "auth-field",
            tags$label("密码"),
            passwordInput("auth_password", label = NULL, placeholder = "输入密码",
                          width = "100%")
          ),
          uiOutput("auth_login_msg"),
          tags$button(
            id = "btn_login", type = "button",
            class = "auth-btn-primary action-button shiny-bound-input",
            "登  录"
          )
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
            tags$button(
              id = "btn_register", type = "button",
              class = "auth-btn-primary action-button shiny-bound-input",
              "注  册  账  号"
            )
          )
        ),

        # ── 面板切换链接 ───────────────────────────────────────────────────────
        div(id = "auth-switch-to-register", class = "auth-switch",
          "没有账号？",
          tags$a(
            onclick = "Shiny.setInputValue('auth_panel_switch', 'register', {priority:'event'})",
            "立即注册"
          )
        ),
        shinyjs::hidden(
          div(id = "auth-switch-to-login", class = "auth-switch",
            "已有账号？",
            tags$a(
              onclick = "Shiny.setInputValue('auth_panel_switch', 'login', {priority:'event'})",
              "返回登录"
            )
          )
        )

      ) # .auth-card
    ) # #auth-overlay-wrap

  ) # tagList
}


# ── 侧边栏用户信息徽标 UI ─────────────────────────────────────────────────────
# 插入到主应用侧边栏顶部，显示当前用户 + 登出按钮
auth_user_badge_ui <- function() {
  uiOutput("auth_user_badge")
}
