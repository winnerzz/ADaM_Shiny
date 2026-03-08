# server.R
# ============================================================================
# 临床数据自动化 ADaM 生成应用 — 服务端逻辑
# 核心改动：
#   1. 全局清空：shinyjs::reset() + reactiveVal 重置，恢复初始状态
#   2. 智能排序：Spec 按 ADSL → ADAE 顺序解析并依次发送给 LLM
#   3. R 环境隔离：所有 LLM 代码在共享隔离 environment() 中执行，
#      ADSL 数据框留存，供 ADAE 代码直接引用
#   4. 流水线状态实时更新
# ============================================================================

library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(stringr)

# 加载 LLM API 模块
source("llm_api.R", local = TRUE)

server <- function(input, output, session) {

  # ================================================================
  # 一、响应式状态变量
  # ================================================================

  # --- 已上传的 SDTM 文件列表：named list（key=域名, value=data.frame） ---
  rv_sdtm   <- reactiveVal(NULL)

 # --- 已上传的 Spec 文件列表：named list（key=数据集名, value=data.frame） ---
  rv_specs  <- reactiveVal(NULL)

  # --- LLM 返回的代码（合并文本） ---
  rv_code   <- reactiveVal("")

  # --- 执行日志 ---
  rv_log    <- reactiveVal("")

  # --- 生成的 ADSL / ADAE 数据框（用于预览与下载） ---
  rv_adsl   <- reactiveVal(NULL)
  rv_adae   <- reactiveVal(NULL)

  # --- 流水线状态文本 ---
  rv_status <- reactiveVal("⏳ 等待数据就绪...")

  # ================================================================
  # 二、辅助函数：追加日志
  # ================================================================
  append_log <- function(msg) {
    old <- isolate(rv_log())
    rv_log(paste0(old, "[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n"))
  }

  # ================================================================
  # 二·五、Provider 切换时自动更新默认模型名
  # ================================================================
  observeEvent(input$llm_provider, {
    defaults <- list(
      kimi     = "kimi-k2.5",
      deepseek = "deepseek-chat",
      openai   = "gpt-4",
      qwen     = "qwen-plus"
    )
    val <- defaults[[input$llm_provider]]
    updateTextInput(session, "model_name",
                    value = if (is.null(val)) "" else val)
  })

  # ================================================================
  # 三、文件上传处理
  # ================================================================

  # ---------- SDTM 文件上传 ----------
  observeEvent(input$sdtm_files, {
    req(input$sdtm_files)
    file_info <- input$sdtm_files
    sdtm_list <- list()

    for (i in seq_len(nrow(file_info))) {
      fname <- file_info$name[i]
      fpath <- file_info$datapath[i]
      # 域名取文件名（去掉 .csv）
      domain <- tolower(tools::file_path_sans_ext(fname))
      tryCatch({
        df <- read_csv(fpath, col_types = cols(.default = col_character()),
                       show_col_types = FALSE)
        sdtm_list[[domain]] <- df
        append_log(sprintf("✅ SDTM 已读入: %s (%d 行, %d 列)",
                           fname, nrow(df), ncol(df)))
      }, error = function(e) {
        append_log(sprintf("❌ SDTM 读取失败 [%s]: %s", fname, e$message))
      })
    }

    # 合并到已有数据（支持分批上传）
    existing <- rv_sdtm()
    if (!is.null(existing)) {
      sdtm_list <- modifyList(existing, sdtm_list)
    }
    rv_sdtm(sdtm_list)
    update_pipeline_status()
  })

  # ---------- Specification 文件上传 ----------
  observeEvent(input$spec_files, {
    req(input$spec_files)
    file_info <- input$spec_files
    spec_list <- list()

    for (i in seq_len(nrow(file_info))) {
      fname <- file_info$name[i]
      fpath <- file_info$datapath[i]
      tryCatch({
        df <- read_csv(fpath, col_types = cols(.default = col_character()),
                       show_col_types = FALSE)
        # 尝试从 Spec 中检测数据集名称
        ds_name <- detect_dataset_name(df)
        if (is.na(ds_name)) {
          # 回退：从文件名推断（如 ads_adsl_full.csv → ADSL）
          ds_name <- toupper(str_extract(fname, "(?i)ad[a-z]+"))
          if (is.na(ds_name)) ds_name <- toupper(tools::file_path_sans_ext(fname))
        }
        spec_list[[ds_name]] <- df
        append_log(sprintf("✅ Spec 已读入: %s → 数据集 %s (%d 个变量)",
                           fname, ds_name, nrow(df)))
      }, error = function(e) {
        append_log(sprintf("❌ Spec 读取失败 [%s]: %s", fname, e$message))
      })
    }

    existing <- rv_specs()
    if (!is.null(existing)) {
      spec_list <- modifyList(existing, spec_list)
    }
    rv_specs(spec_list)
    update_pipeline_status()
  })

  # ================================================================
  # 四、UI 渲染：已上传文件列表
  # ================================================================

  output$sdtm_file_list <- renderUI({
    sdtm <- rv_sdtm()
    if (is.null(sdtm) || length(sdtm) == 0) return(NULL)
    tags$div(
      class = "small text-success",
      tags$strong("已加载 SDTM 域："),
      paste(toupper(names(sdtm)), collapse = "、")
    )
  })

  output$spec_file_list <- renderUI({
    specs <- rv_specs()
    if (is.null(specs) || length(specs) == 0) return(NULL)
    tags$div(
      class = "small text-success",
      tags$strong("已加载 Spec："),
      paste(names(specs), collapse = "、")
    )
  })

  # ================================================================
  # 五、流水线状态更新
  # ================================================================
  update_pipeline_status <- function() {
    sdtm  <- isolate(rv_sdtm())
    specs <- isolate(rv_specs())

    has_sdtm <- !is.null(sdtm) && length(sdtm) > 0
    has_spec <- !is.null(specs) && length(specs) > 0

    status <- if (!has_sdtm && !has_spec) {
      "⏳ 等待数据就绪..."
    } else if (has_sdtm && !has_spec) {
      "📂 SDTM 已上传，请上传 Specification"
    } else if (!has_sdtm && has_spec) {
      "📂 Spec 已上传，请上传 SDTM 数据"
    } else {
      "✅ 数据就绪，可以点击「生成 ADaM 与代码」"
    }
    rv_status(status)
  }

  # 流水线状态文本渲染（通过 JS 更新侧边栏底部文本）
  observe({
    status_text <- rv_status()
    shinyjs::html("pipeline_status_text", html = status_text)
  })

  # ================================================================
  # 六、全局清空逻辑
  #     使用 shinyjs::reset() 清空上传控件 +
  #     重置所有 reactiveVal 变量
  # ================================================================
  observeEvent(input$btn_clear_all, {
    # 1. 使用 shinyjs::reset() 清空两个 fileInput 控件
    shinyjs::reset("sdtm_files")
    shinyjs::reset("spec_files")

    # 2. 重置所有存储状态的 reactiveVal 为初始值
    rv_sdtm(NULL)
    rv_specs(NULL)
    rv_code("")
    rv_log("")
    rv_adsl(NULL)
    rv_adae(NULL)
    rv_status("⏳ 等待数据就绪...")

    # 3. 提示用户
    showNotification("已全局清空所有上传和结果", type = "warning", duration = 3)
    append_log("🗑️ 用户执行了全局清空操作")
  })

  # ================================================================
  # 七、核心流水线：生成 ADaM 与代码
  # ================================================================
  observeEvent(input$btn_run_pipeline, {

    # ---------- 前置校验 ----------
    sdtm_data <- rv_sdtm()
    spec_data <- rv_specs()

    if (is.null(sdtm_data) || length(sdtm_data) == 0) {
      showNotification("请先上传 SDTM 文件", type = "error")
      return()
    }
    if (is.null(spec_data) || length(spec_data) == 0) {
      showNotification("请先上传 Specification 文件", type = "error")
      return()
    }
    if (nchar(trimws(input$api_key)) == 0) {
      showNotification("请输入 API Key", type = "error")
      return()
    }

    # ---------- 智能排序：确保 ADSL 在 ADAE 之前 ----------
    # 定义优先级：ADSL 最高，其余按字母序
    ds_names   <- names(spec_data)
    priority   <- c("ADSL" = 1, "ADAE" = 2)
    order_vals <- ifelse(ds_names %in% names(priority),
                         priority[ds_names],
                         100 + seq_along(ds_names))
    sorted_ds  <- ds_names[order(order_vals)]

    append_log(sprintf("📋 数据集处理顺序: %s", paste(sorted_ds, collapse = " → ")))

    # ---------- 汇总 SDTM 列名信息 ----------
    sdtm_info <- summarize_sdtm_columns(sdtm_data)

    # ---------- 创建隔离 R 执行环境 ----------
    # 关键设计：所有 LLM 生成的代码在此共享 environment 中执行，
    # 使得 ADSL 数据框留存，供后续 ADAE 代码引用
    exec_env <- new.env(parent = globalenv())

    # 预加载常用包到执行环境（避免 LLM 代码中 library() 失败）
    eval(parse(text = "
      library(readr)
      library(dplyr)
      library(lubridate)
      library(stringr)
    "), envir = exec_env)

    # 将 SDTM 数据注入执行环境（LLM 代码可直接引用 dm, ex, ae 等）
    for (nm in names(sdtm_data)) {
      assign(nm, sdtm_data[[nm]], envir = exec_env)
    }

    # ---------- 禁用按钮，防止重复点击 ----------
    shinyjs::disable("btn_run_pipeline")
    on.exit(shinyjs::enable("btn_run_pipeline"), add = TRUE)

    # ---------- 清空上次结果 ----------
    rv_code("")
    rv_log("")
    rv_adsl(NULL)
    rv_adae(NULL)
    all_code <- character(0)

    # ---------- 逐个数据集：调用 LLM → 执行代码 ----------
    for (ds in sorted_ds) {
      rv_status(sprintf("🔄 正在处理 %s ...", ds))
      append_log(sprintf("━━━ 开始处理 %s ━━━", ds))

      spec_df   <- spec_data[[ds]]
      spec_text <- format_spec_text(spec_df)

      # 构建额外上下文（对 ADAE 等后续数据集，说明 adsl 已可用）
      extra_ctx <- ""
      if (ds != "ADSL" && "ADSL" %in% sorted_ds) {
        extra_ctx <- paste0(
          "重要：`adsl` 数据框已在当前 R 环境中生成并可用。",
          "请直接使用 left_join(ae, adsl, by = \"USUBJID\") 获取受试者级别变量。",
          "不要重新从 CSV 读取 ADSL。"
        )
      }

      # --- 调用 LLM ---
      append_log(sprintf("📡 正在调用 %s API ...", input$llm_provider))
      tryCatch({
        user_prompt <- build_user_prompt(
          dataset_name = ds,
          spec_text    = spec_text,
          sdtm_info    = sdtm_info,
          extra_ctx    = extra_ctx
        )

        r_code <- call_llm(
          provider_key   = input$llm_provider,
          api_key        = input$api_key,
          user_prompt    = user_prompt,
          model_override = input$model_name
        )

        append_log(sprintf("✅ LLM 返回 %s 代码 (%d 字符)", ds, nchar(r_code)))
        all_code <- c(all_code, sprintf("# ===== %s =====", ds), r_code, "")

      }, error = function(e) {
        append_log(sprintf("❌ LLM 调用失败 [%s]: %s", ds, e$message))
        showNotification(sprintf("%s 代码生成失败: %s", ds, e$message),
                         type = "error", duration = 8)
        r_code <<- NULL
      })

      if (is.null(r_code) || nchar(trimws(r_code)) == 0) {
        append_log(sprintf("⚠️ 跳过 %s（无有效代码）", ds))
        next
      }

      # --- 在隔离环境中执行代码 ---
      append_log(sprintf("⚙️ 正在执行 %s 代码 ...", ds))
      rv_status(sprintf("⚙️ 正在执行 %s 代码 ...", ds))

      tryCatch({
        # 捕获 stdout 输出
        exec_output <- capture.output({
          eval(parse(text = r_code), envir = exec_env)
        })
        if (length(exec_output) > 0) {
          append_log(paste("  [stdout]", paste(exec_output, collapse = "\n  ")))
        }
        append_log(sprintf("✅ %s 代码执行成功", ds))

        # --- 从隔离环境中提取结果数据框 ---
        if (ds == "ADSL" && exists("adsl", envir = exec_env)) {
          rv_adsl(get("adsl", envir = exec_env))
          append_log(sprintf("📊 ADSL 已生成: %d 行 × %d 列",
                             nrow(rv_adsl()), ncol(rv_adsl())))
        }
        if (ds == "ADAE" && exists("adae", envir = exec_env)) {
          rv_adae(get("adae", envir = exec_env))
          append_log(sprintf("📊 ADAE 已生成: %d 行 × %d 列",
                             nrow(rv_adae()), ncol(rv_adae())))
        }

      }, error = function(e) {
        append_log(sprintf("❌ %s 代码执行报错:\n  %s", ds, e$message))
        showNotification(sprintf("%s 代码执行失败", ds),
                         type = "error", duration = 8)
      })
    }

    # ---------- 汇总代码 ----------
    rv_code(paste(all_code, collapse = "\n"))
    rv_status("✅ 流水线完成")
    append_log("━━━ 流水线全部完成 ━━━")
    showNotification("ADaM 生成流水线已完成！", type = "message", duration = 5)
  })

  # ================================================================
  # 八、输出渲染
  # ================================================================

  # 生成的 R 代码
  output$code_output <- renderText({ rv_code() })

  # 执行日志
  output$exec_log <- renderText({ rv_log() })

  # ADSL 预览
  output$adsl_preview <- renderTable({
    req(rv_adsl())
    head(rv_adsl(), 200)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # ADAE 预览
  output$adae_preview <- renderTable({
    req(rv_adae())
    head(rv_adae(), 200)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

  # ================================================================
  # 九、下载处理
  # ================================================================

  output$dl_adsl <- downloadHandler(
    filename = function() paste0("adsl_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv_adsl())
      write_csv(rv_adsl(), file, na = "")
    }
  )

  output$dl_adae <- downloadHandler(
    filename = function() paste0("adae_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content  = function(file) {
      req(rv_adae())
      write_csv(rv_adae(), file, na = "")
    }
  )

  output$dl_rcode <- downloadHandler(
    filename = function() paste0("build_adam_", format(Sys.Date(), "%Y%m%d"), ".R"),
    content  = function(file) {
      writeLines(rv_code(), file)
    }
  )
}
