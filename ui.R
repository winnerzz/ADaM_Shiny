# ui.R
# ============================================================================
# 临床数据自动化 ADaM 生成应用 — UI 层
# 核心改动：
#   1. 使用 bslib::accordion() 折叠面板重构侧边栏布局
#   2. 增加 Kimi / DeepSeek / OpenAI / 通义千问 四种 LLM 选项
#   3. 侧边栏底部固定"流水线状态"和主操作按钮
#   4. 红色警告样式"全局清空上传"按钮
# ============================================================================

library(shiny)
library(bslib)
library(shinyjs)

ui <- page_sidebar(
  title = "临床数据自动化 · ADaM 数据集与代码生成",
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary   = "#2c3e50",
    "sidebar-bg" = "#f8f9fa"
  ),

  # ---------- 启用 shinyjs（全局清空需要） ----------
  useShinyjs(),

  # ================================================================
  # 侧边栏
  # ================================================================
  sidebar = sidebar(
    width = 380,

    # ---- 折叠面板组 ----
    accordion(
      id = "sidebar_accordion",
      open = "panel_data",           # 默认只展开"数据准备"

      # ============================================================
      # 折叠面板 1：数据准备 (Data Setup)
      # ============================================================
      accordion_panel(
        title = "📂 数据准备 (Data Setup)",
        value = "panel_data",

        # --- SDTM 上传（支持多选） ---
        fileInput(
          inputId  = "sdtm_files",
          label    = "上传 SDTM CSV（可多选）",
          multiple = TRUE,
          accept   = c(".csv"),
          placeholder = "dm.csv, ex.csv, ae.csv ..."
        ),
        # 已上传 SDTM 文件列表（动态渲染）
        uiOutput("sdtm_file_list"),

        tags$hr(style = "margin:8px 0;"),

        # --- Specification 上传（支持多选） ---
        fileInput(
          inputId  = "spec_files",
          label    = "上传 Specification CSV（可多选）",
          multiple = TRUE,
          accept   = c(".csv"),
          placeholder = "ads_adsl_full.csv, ads_adae_full.csv ..."
        ),
        # 已上传 Spec 文件列表（动态渲染）
        uiOutput("spec_file_list"),

        tags$hr(style = "margin:8px 0;"),

        # --- 红色警告样式"全局清空上传"按钮 ---
        actionButton(
          inputId = "btn_clear_all",
          label   = "🗑️ 全局清空上传",
          class   = "btn-danger btn-sm w-100",
          icon    = icon("trash")
        )
      ),

      # ============================================================
      # 折叠面板 2：AI 引擎配置（默认折叠）
      # ============================================================
      accordion_panel(
        title = "🤖 AI 引擎配置 (AI Engine Settings)",
        value = "panel_ai",

        # --- 模型选择下拉菜单 ---
        selectInput(
          inputId  = "llm_provider",
          label    = "选择大模型",
          choices  = c(
            "Kimi (Moonshot AI)"  = "kimi",
            "DeepSeek"            = "deepseek",
            "OpenAI (GPT-4)"     = "openai",
            "通义千问 (Qwen)"      = "qwen"
          ),
          selected = "deepseek"
        ),

        # --- 模型名称（可修改） ---
        textInput(
          inputId     = "model_name",
          label       = "模型名称（可修改）",
          value       = "kimi-k2.5",
          placeholder = "e.g. kimi-k2.5"
        ),

        # --- API Key 密码输入框 ---
        passwordInput(
          inputId     = "api_key",
          label       = "API Key",
          placeholder = "sk-..."
        ),

        tags$small(
          class = "text-muted",
          "密钥仅在本次会话中使用，不会被存储。"
        )
      )
    ),
    # ---- 折叠面板结束 ----

    tags$div(style = "flex-grow:1;"),   # 弹性占位，把下方区域推到底部

    # ============================================================
    # 侧边栏底部固定区：流水线状态 + 主操作按钮
    # ============================================================
    tags$hr(),

    # 流水线状态文本提示
    tags$div(
      id    = "pipeline_status_area",
      style = "text-align:center; margin-bottom:10px;",
      tags$small(
        id    = "pipeline_status_text",
        class = "text-muted",
        "⏳ 等待数据就绪..."
      )
    ),

    # 主操作按钮
    actionButton(
      inputId = "btn_run_pipeline",
      label   = "🚀 生成 ADaM 与代码",
      class   = "btn-primary btn-lg w-100",
      style   = "font-weight:bold; font-size:1.15em; padding:12px;"
    )
  ),

  # ================================================================
  # 主面板（右侧）
  # ================================================================
  navset_card_tab(
    id = "main_tabs",

    # --- Tab 1：生成的 R 代码 ---
    nav_panel(
      title = "📝 生成的 R 代码",
      card(
        card_header("LLM 返回的 R 代码"),
        verbatimTextOutput("code_output", placeholder = TRUE)
      )
    ),

    # --- Tab 2：执行日志 ---
    nav_panel(
      title = "📋 执行日志",
      card(
        card_header("代码执行过程与结果"),
        verbatimTextOutput("exec_log", placeholder = TRUE)
      )
    ),

    # --- Tab 3：ADSL 预览 ---
    nav_panel(
      title = "📊 ADSL 预览",
      card(
        card_header("ADSL 数据集（前 200 行）"),
        tableOutput("adsl_preview")
      )
    ),

    # --- Tab 4：ADAE 预览 ---
    nav_panel(
      title = "📊 ADAE 预览",
      card(
        card_header("ADAE 数据集（前 200 行）"),
        tableOutput("adae_preview")
      )
    ),

    # --- Tab 5：下载 ---
    nav_panel(
      title = "⬇️ 下载",
      card(
        card_header("下载生成的文件"),
        layout_columns(
          col_widths = c(4, 4, 4),
          downloadButton("dl_adsl",   "下载 ADSL CSV",   class = "btn-outline-success w-100"),
          downloadButton("dl_adae",   "下载 ADAE CSV",   class = "btn-outline-success w-100"),
          downloadButton("dl_rcode",  "下载 R 脚本",      class = "btn-outline-primary w-100")
        )
      )
    )
  )
)
