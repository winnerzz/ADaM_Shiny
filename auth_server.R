# =============================================================================
# auth_server.R
# 用户认证服务端逻辑
#
# 调用方式（在 server.R 的 server 函数内部）：
#   current_user <- reactiveVal(NULL)
#   auth_server(input, output, session, current_user)
#
# current_user() 返回值：
#   NULL                                    — 未登录
#   list(id, username, display_name, role)  — 已登录用户信息
# =============================================================================

auth_server <- function(input, output, session, current_user) {

  # ── 切换登录 / 注册面板 ──────────────────────────────────────────────────────
  observeEvent(input$auth_panel_switch, {
    if (input$auth_panel_switch == "register") {
      shinyjs::hide("auth-panel-login")
      shinyjs::show("auth-panel-register")
      shinyjs::hide("auth-switch-to-register")
      shinyjs::show("auth-switch-to-login")
      # 清空登录错误提示
      output$auth_login_msg <- renderUI(NULL)
    } else {
      shinyjs::show("auth-panel-login")
      shinyjs::hide("auth-panel-register")
      shinyjs::show("auth-switch-to-register")
      shinyjs::hide("auth-switch-to-login")
      # 清空注册错误提示
      output$auth_reg_msg <- renderUI(NULL)
    }
  }, ignoreInit = TRUE)

  # ── 登录处理 ─────────────────────────────────────────────────────────────────
  output$auth_login_msg <- renderUI(NULL)

  observeEvent(input$btn_login, {
    username <- trimws(input$auth_username %||% "")
    password <- input$auth_password %||% ""

    if (!nzchar(username) || !nzchar(password)) {
      output$auth_login_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", "请填写用户名和密码")
      )
      return()
    }

    result <- auth_db_verify_user(username, password)

    if (!result$ok) {
      output$auth_login_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", result$msg)
      )
    } else {
      # 登录成功：更新用户状态（main_content_area 的 renderUI 会自动切换界面）
      current_user(result$user)
    }
  })

  # ── 注册处理 ─────────────────────────────────────────────────────────────────
  output$auth_reg_msg <- renderUI(NULL)

  observeEvent(input$btn_register, {
    username     <- trimws(input$reg_username     %||% "")
    display_name <- trimws(input$reg_display_name %||% "")
    email        <- trimws(input$reg_email        %||% "")
    password     <- input$reg_password  %||% ""
    password2    <- input$reg_password2 %||% ""

    # 校验
    if (!grepl("^[a-zA-Z0-9_]{4,20}$", username)) {
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-error",
            "用户名须为 4-20 位，仅限字母、数字、下划线")
      )
      return()
    }
    if (!nzchar(display_name)) {
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", "请填写显示名称")
      )
      return()
    }
    if (nchar(password) < 8) {
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", "密码长度至少 8 位")
      )
      return()
    }
    if (password != password2) {
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", "两次输入的密码不一致")
      )
      return()
    }

    result <- auth_db_create_user(username, password, display_name, email)

    if (!result$ok) {
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-error", result$msg)
      )
    } else {
      # 注册成功后自动登录
      output$auth_reg_msg <- renderUI(
        div(class = "auth-msg auth-msg-success", "注册成功！正在登录...")
      )
      login_result <- auth_db_verify_user(username, password)
      if (login_result$ok) {
        current_user(login_result$user)
      }
    }
  })

  # ── 登出处理 ─────────────────────────────────────────────────────────────────
  observeEvent(input$btn_logout, {
    current_user(NULL)
    # 清空表单
    updateTextInput(session, "auth_username", value = "")
    updateTextInput(session, "auth_password", value = "")
    # 重置为登录面板
    shinyjs::show("auth-panel-login")
    shinyjs::hide("auth-panel-register")
    shinyjs::show("auth-switch-to-register")
    shinyjs::hide("auth-switch-to-login")
    output$auth_login_msg <- renderUI(NULL)
    # current_user(NULL) 已触发 renderUI 自动切回登录面板，无需手动操作
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── 侧边栏用户信息徽标 ───────────────────────────────────────────────────────
  output$auth_user_badge <- renderUI({
    user <- current_user()
    if (is.null(user)) return(NULL)

    role_label <- if (identical(user$role, "admin")) {
      span(style = paste0(
        "background:rgba(210,153,34,0.15);color:#d29922;",
        "border:1px solid rgba(210,153,34,0.3);border-radius:3px;",
        "font-size:0.62rem;padding:1px 6px;font-weight:700;",
        "text-transform:uppercase;letter-spacing:0.06em;"
      ), "ADMIN")
    } else {
      span(style = paste0(
        "background:rgba(45,212,191,0.12);color:#2dd4bf;",
        "border:1px solid rgba(45,212,191,0.28);border-radius:3px;",
        "font-size:0.62rem;padding:1px 6px;font-weight:700;",
        "text-transform:uppercase;letter-spacing:0.06em;"
      ), "USER")
    }

    div(
      style = paste0(
        "display:flex;align-items:center;justify-content:space-between;",
        "background:#0d1117;border:1px solid #21262d;border-radius:6px;",
        "padding:0.45rem 0.65rem;margin-bottom:0.9rem;"
      ),
      div(style = "display:flex;align-items:center;gap:0.5rem;min-width:0;",
        div(style = paste0(
          "width:28px;height:28px;border-radius:50%;",
          "background:linear-gradient(135deg,#2dd4bf,#0ea5e9);",
          "display:flex;align-items:center;justify-content:center;",
          "font-size:0.75rem;font-weight:700;color:#0d1117;flex-shrink:0;"
        ),
          toupper(substr(user$display_name, 1, 1))
        ),
        div(style = "min-width:0;",
          div(style = paste0(
            "font-size:0.78rem;font-weight:600;color:#e6edf3;",
            "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"
          ), user$display_name),
          div(style = "margin-top:2px;", role_label)
        )
      ),
      # 登出按钮
      actionButton(
        "btn_logout",
        label    = bsicons::bs_icon("box-arrow-right", size = "0.85rem"),
        class    = "btn-sm",
        title    = "退出登录",
        style    = paste0(
          "background:transparent;border:none;color:#6e7681;padding:2px 5px;",
          "line-height:1;cursor:pointer;flex-shrink:0;"
        )
      )
    )
  })
}
