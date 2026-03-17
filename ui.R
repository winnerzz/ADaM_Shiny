# =============================================================================
# ui.R
# ADaM 自动化生成平台 — Shiny 前端界面
#
# 依赖包：shiny, bslib, bsicons, DT, shinyAce
#
# ── 本次修改摘要 ─────────────────────────────────────────────────────────────
#   [修改 1] file_spec fileInput：accept .json → .csv，更新提示文字与图标
#   [新增 2] uiOutput("spec_parse_status")：侧边栏 Spec 解析状态卡片
#   [新增 3] CSS：Modal 解析报告 / 置信度徽标 / Spec 状态卡片样式
# =============================================================================

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(shinyAce)
library(shinyjs)   # [新增] 用于 useShinyjs() + reset() 重置 fileInput

adam_theme <- bs_theme(
  version       = 5,
  bg            = "#0d1117",
  fg            = "#e6edf3",
  primary       = "#2dd4bf",
  secondary     = "#1c2333",
  success       = "#3fb950",
  warning       = "#d29922",
  danger        = "#f85149",
  info          = "#58a6ff",
  border_radius = "6px",
  font_scale    = 0.95,
  base_font    = font_google("DM Sans",        wght = "300;400;500;600"),
  heading_font = font_google("Syne",           wght = "600;700;800"),
  code_font    = font_google("JetBrains Mono", wght = "400;500")
) |>
  bs_add_rules("
    body { letter-spacing: 0.01em; background-color: #0d1117; }

    .bslib-sidebar-layout > .sidebar {
      background: #161b22 !important;
      border-right: 1px solid #30363d !important;
      padding: 1.4rem 1.2rem !important;
    }
    .form-control[type='file'] {
      background: #0d1117; border: 1.5px dashed #30363d; border-radius: 6px;
      color: #8b949e; font-size: 0.82rem; transition: border-color 0.2s; cursor: pointer;
    }
    .form-control[type='file']:hover { border-color: #2dd4bf; color: #e6edf3; }

    .upload-label {
      font-size: 0.68rem; font-weight: 600; letter-spacing: 0.1em;
      text-transform: uppercase; color: #8b949e; margin-bottom: 0.3rem;
    }

    #btn_generate {
      background: linear-gradient(135deg, #2dd4bf 0%, #0ea5e9 100%);
      border: none; border-radius: 6px; color: #0d1117; font-weight: 700;
      font-family: 'Syne', sans-serif; font-size: 0.9rem; letter-spacing: 0.05em;
      padding: 0.65rem 1.2rem; width: 100%;
      transition: opacity 0.2s, transform 0.1s;
      box-shadow: 0 0 20px rgba(45, 212, 191, 0.25);
    }
    #btn_generate:hover  { opacity: 0.88; transform: translateY(-1px); }
    #btn_generate:active { transform: translateY(0); }

    #btn_run_code {
      background: #238636; border: 1px solid #2ea043; color: #fff;
      font-weight: 600; border-radius: 6px; padding: 0.5rem 1.4rem;
      transition: background 0.2s;
    }
    #btn_run_code:hover { background: #2ea043; }

    .btn-download {
      background: transparent; border: 1px solid #30363d; color: #8b949e;
      border-radius: 6px; font-size: 0.8rem; padding: 0.35rem 0.9rem;
      transition: border-color 0.2s, color 0.2s;
    }
    .btn-download:hover { border-color: #2dd4bf; color: #2dd4bf; }

    .nav-tabs { border-bottom: 1px solid #30363d !important; }
    .nav-tabs .nav-link {
      color: #8b949e !important; font-size: 0.83rem; font-weight: 500;
      letter-spacing: 0.04em; border: none !important;
      border-bottom: 2px solid transparent !important; padding: 0.6rem 1.1rem;
      transition: color 0.2s, border-color 0.2s;
    }
    .nav-tabs .nav-link:hover  { color: #e6edf3 !important; }
    .nav-tabs .nav-link.active {
      color: #2dd4bf !important; border-bottom-color: #2dd4bf !important;
      background: transparent !important;
    }

    #run_status {
      font-family: 'JetBrains Mono', monospace; font-size: 0.78rem;
      background: #0d1117; border: 1px solid #21262d; border-radius: 6px;
      color: #7ee787; padding: 1rem; min-height: 90px;
      line-height: 1.8; white-space: pre-wrap;
    }
    .ace_editor {
      border: 1px solid #30363d !important; border-radius: 6px !important;
      font-size: 13px !important;
    }

    .badge-warning-custom {
      background: rgba(210,153,34,0.18); color: #d29922;
      border: 1px solid rgba(210,153,34,0.4);
      font-family: 'JetBrains Mono',monospace; font-size: 0.7rem;
      padding: 2px 7px; border-radius: 4px;
    }
    .badge-info-custom {
      background: rgba(88,166,255,0.15); color: #58a6ff;
      border: 1px solid rgba(88,166,255,0.35);
      font-family: 'JetBrains Mono',monospace; font-size: 0.7rem;
      padding: 2px 7px; border-radius: 4px;
    }
    .badge-error-custom {
      background: rgba(248,81,73,0.15); color: #f85149;
      border: 1px solid rgba(248,81,73,0.35);
      font-family: 'JetBrains Mono',monospace; font-size: 0.7rem;
      padding: 2px 7px; border-radius: 4px;
    }

    /* ══════════════════════════════════════════════════════════════════════
       [新增 3a] 解析报告 Modal 样式
       ══════════════════════════════════════════════════════════════════════ */
    .modal-content {
      background: #161b22 !important; border: 1px solid #30363d !important;
      border-radius: 8px !important;
    }
    .modal-header {
      background: #0d1117 !important; border-bottom: 1px solid #21262d !important;
      padding: 0.9rem 1.2rem !important;
    }
    .modal-title {
      font-family: 'Syne', sans-serif !important; font-size: 0.95rem !important;
      font-weight: 700 !important; color: #e6edf3 !important;
    }
    .modal-footer {
      background: #0d1117 !important; border-top: 1px solid #21262d !important;
    }
    .btn-close { filter: invert(1) brightness(0.5); }

    /* 列映射表 */
    .parse-map-table {
      width: 100%; border-collapse: collapse;
      font-size: 0.8rem; font-family: 'DM Sans', sans-serif;
    }
    .parse-map-table th {
      background: #0d1117; color: #8b949e; font-size: 0.68rem;
      letter-spacing: 0.08em; text-transform: uppercase;
      padding: 0.4rem 0.8rem; border-bottom: 1px solid #21262d; text-align: left;
    }
    .parse-map-table td {
      padding: 0.4rem 0.8rem; color: #e6edf3; border-bottom: 1px solid #161b22;
    }
    .parse-map-table tr:last-child td { border-bottom: none; }
    .parse-map-table tr:hover td { background: rgba(255,255,255,0.02); }

    /* 置信度徽标 */
    .conf-high {
      background: rgba(63,185,80,0.15); color: #3fb950;
      border: 1px solid rgba(63,185,80,0.35);
      font-family: 'JetBrains Mono',monospace; font-size: 0.68rem;
      padding: 1px 6px; border-radius: 3px;
    }
    .conf-medium {
      background: rgba(210,153,34,0.15); color: #d29922;
      border: 1px solid rgba(210,153,34,0.35);
      font-family: 'JetBrains Mono',monospace; font-size: 0.68rem;
      padding: 1px 6px; border-radius: 3px;
    }
    .conf-low {
      background: rgba(248,81,73,0.12); color: #f85149;
      border: 1px solid rgba(248,81,73,0.3);
      font-family: 'JetBrains Mono',monospace; font-size: 0.68rem;
      padding: 1px 6px; border-radius: 3px;
    }

    /* 行级风险条目 */
    .parse-risk-item {
      display: flex; gap: 0.6rem; align-items: flex-start;
      padding: 0.4rem 0.7rem; border-radius: 4px; margin-bottom: 0.3rem;
      font-size: 0.78rem; line-height: 1.45;
    }
    .parse-risk-item.risk-warn { background: rgba(210,153,34,0.07); border-left: 2px solid #d29922; }
    .parse-risk-item.risk-info { background: rgba(88,166,255,0.05);  border-left: 2px solid #58a6ff; }
    .parse-risk-item .risk-tag {
      font-family: 'JetBrains Mono',monospace; font-size: 0.65rem;
      font-weight: 600; flex-shrink: 0; padding-top: 1px;
    }
    .parse-risk-item.risk-warn .risk-tag { color: #d29922; }
    .parse-risk-item.risk-info .risk-tag { color: #58a6ff; }

    /* 预览表格 */
    .preview-scroll { overflow-x: auto; border: 1px solid #21262d; border-radius: 4px; }
    .preview-table {
      width: 100%; border-collapse: collapse;
      font-size: 0.72rem; font-family: 'JetBrains Mono',monospace; white-space: nowrap;
    }
    .preview-table th {
      background: #0d1117; color: #2dd4bf; padding: 0.32rem 0.7rem;
      border-bottom: 1px solid #21262d; text-align: left;
    }
    .preview-table td {
      color: #8b949e; padding: 0.28rem 0.7rem; border-bottom: 1px solid #0d1117;
      max-width: 180px; overflow: hidden; text-overflow: ellipsis;
    }
    .preview-table tr:last-child td { border-bottom: none; }

    /* Modal 内分段标题 */
    .modal-section-title {
      font-size: 0.7rem; font-weight: 600; letter-spacing: 0.09em;
      text-transform: uppercase; color: #8b949e; margin: 1rem 0 0.5rem 0;
    }
    .modal-section-title:first-child { margin-top: 0; }

    /* ══════════════════════════════════════════════════════════════════════
       [新增 3b] 侧边栏 Spec 解析状态卡片
       ══════════════════════════════════════════════════════════════════════ */
    .spec-status-card {
      background: #0d1117; border: 1px solid #21262d; border-radius: 6px;
      padding: 0.55rem 0.8rem; margin-top: 0.4rem; font-size: 0.75rem;
    }
    .spec-status-card .status-row {
      display: flex; align-items: center; gap: 0.5rem;
    }
    .spec-status-card .dot {
      width: 7px; height: 7px; border-radius: 50%; flex-shrink: 0;
    }
    .dot-idle    { background: #30363d; }
    .dot-parsing { background: #d29922; box-shadow: 0 0 5px #d29922; }
    .dot-ok      { background: #3fb950; box-shadow: 0 0 4px #3fb950; }
    .dot-warn    { background: #d29922; }
    .dot-error   { background: #f85149; }
    .spec-status-card .status-text  { color: #8b949e; line-height: 1.35; }
    .spec-status-card .reopen-link  {
      margin-top: 0.35rem; font-size: 0.7rem; color: #2dd4bf;
      cursor: pointer; text-decoration: underline; text-underline-offset: 2px;
    }

    /* DT 深色 */
    .dataTables_wrapper,
    table.dataTable thead th,
    table.dataTable tbody td { color: #e6edf3 !important; }
    table.dataTable thead th {
      background: #161b22 !important; border-bottom: 1px solid #30363d !important;
      font-size: 0.75rem; letter-spacing: 0.06em; text-transform: uppercase; font-weight: 600;
    }
    table.dataTable tbody tr         { background: #0d1117 !important; }
    table.dataTable tbody tr:hover td { background: #161b22 !important; }
    table.dataTable tbody tr.even td  { background: #0d1117 !important; }
    .dataTables_info, .dataTables_length label,
    .dataTables_filter label { color: #8b949e !important; font-size: 0.78rem; }
    .dataTables_paginate .paginate_button { color: #8b949e !important; }
    .dataTables_paginate .paginate_button.current {
      color: #2dd4bf !important; background: rgba(45,212,191,0.1) !important;
      border: 1px solid #2dd4bf !important; border-radius: 4px !important;
    }
    .bslib-value-box { border: 1px solid #21262d !important; }
    .bslib-value-box .value-box-value {
      font-family: 'Syne', sans-serif; font-size: 1.8rem !important; font-weight: 700;
    }
    .card { background: #161b22 !important; border: 1px solid #21262d !important; }
    .card-header {
      background: #0d1117 !important; border-bottom: 1px solid #21262d !important;
      font-size: 0.78rem; font-weight: 600; letter-spacing: 0.08em;
      text-transform: uppercase; color: #8b949e;
    }
    hr.section-divider { border-color: #21262d; margin: 1.2rem 0; }
    .brand-bar {
      display: flex; align-items: center; gap: 0.6rem;
      padding: 0.5rem 0 1.2rem 0; border-bottom: 1px solid #21262d; margin-bottom: 1.4rem;
    }
    .brand-icon {
      width: 28px; height: 28px;
      background: linear-gradient(135deg, #2dd4bf, #0ea5e9);
      border-radius: 6px; display: flex; align-items: center; justify-content: center;
    }
    .brand-title { font-family:'Syne',sans-serif; font-size:0.95rem; font-weight:700; color:#e6edf3; line-height:1.1; }
    .brand-sub   { font-size:0.65rem; color:#8b949e; letter-spacing:0.07em; text-transform:uppercase; }
    .step-indicator { display:flex; gap:0.5rem; align-items:center; margin-bottom:1rem; }
    .step-dot { width:8px; height:8px; border-radius:50%; background:#21262d; flex-shrink:0; }
    .step-dot.active  { background:#2dd4bf; box-shadow:0 0 6px #2dd4bf; }
    .step-dot.done    { background:#3fb950; }
    .step-dot.warning { background:#d29922; }
    .step-label { font-size:0.72rem; color:#8b949e; letter-spacing:0.04em; }
    .hint-text  { font-size:0.72rem; color:#6e7681; line-height:1.5; margin-top:0.25rem; }

    /* ── [新增] 已上传文件状态卡片 ── */
    .uploaded-files-card {
      background: #0d1117;
      border: 1px solid #21262d;
      border-radius: 6px;
      padding: 0.5rem 0.75rem;
      margin-top: 0.5rem;
      font-size: 0.74rem;
    }
    .uploaded-files-card .uf-title {
      font-size: 0.65rem;
      font-weight: 600;
      letter-spacing: 0.09em;
      text-transform: uppercase;
      color: #6e7681;
      margin-bottom: 0.35rem;
    }
    .uploaded-files-card .uf-item {
      display: flex;
      align-items: center;
      gap: 0.45rem;
      padding: 0.18rem 0;
      color: #c9d1d9;
      line-height: 1.4;
    }
    .uploaded-files-card .uf-item .uf-dot {
      width: 6px; height: 6px;
      border-radius: 50%;
      flex-shrink: 0;
    }
    .uploaded-files-card .uf-item .uf-dot.ok      { background: #3fb950; }
    .uploaded-files-card .uf-item .uf-dot.missing { background: #30363d; }
    .uploaded-files-card .uf-item.missing-item    { color: #6e7681; font-style: italic; }
    .uploaded-files-card .uf-rows   { color: #6e7681; font-size: 0.68rem; margin-left: auto; font-family: 'JetBrains Mono',monospace; }

    /* ── [新增] 清空上传按钮 ── */
    #btn_clear_uploads {
      background: transparent;
      border: 1px solid #30363d;
      color: #6e7681;
      border-radius: 5px;
      font-size: 0.72rem;
      padding: 0.28rem 0.7rem;
      width: 100%;
      margin-top: 0.4rem;
      transition: border-color 0.2s, color 0.2s;
    }
    #btn_clear_uploads:hover { border-color: #f85149; color: #f85149; }

    /* ── [新增] API 配置区域 ── */
    .api-config-section .form-control,
    .api-config-section .form-select {
      background: #0d1117 !important;
      border: 1px solid #30363d !important;
      color: #c9d1d9 !important;
      border-radius: 5px !important;
      font-size: 0.8rem !important;
      padding: 0.35rem 0.6rem !important;
    }
    .api-config-section .form-control:focus,
    .api-config-section .form-select:focus {
      border-color: #2dd4bf !important;
      box-shadow: 0 0 0 2px rgba(45,212,191,0.15) !important;
    }
    .api-config-section label {
      font-size: 0.7rem !important;
      color: #8b949e !important;
      margin-bottom: 0.2rem !important;
    }
    .api-config-section input[type='password'] { letter-spacing: 0.1em; }
    .api-config-section .local-url-input {
      font-family: 'JetBrains Mono', monospace; font-size: 0.75rem;
    }

    /* ── 故障转移折叠区 ── */
    .failover-section {
      border-top: 1px solid #21262d; margin-top: 0.5rem; padding-top: 0.5rem;
    }

    /* ── 单文件删除按钮 ── */
    .btn-remove-file {
      background: transparent; border: none; color: #6e7681;
      padding: 0.1rem 0.3rem; border-radius: 3px; flex-shrink: 0;
      transition: color 0.15s, background 0.15s; line-height: 1;
      cursor: pointer;
    }
    .btn-remove-file:hover { color: #f85149; background: rgba(248,81,73,0.12); }

    /* ── 文件元数据分隔符 ── */
    .uf-meta-sep { color: #30363d; font-size: 0.65rem; }
    .uf-meta     { color: #6e7681; font-size: 0.67rem; font-family: 'JetBrains Mono',monospace; }

    /* ── API 调用进度条 ── */
    #adam-progress-wrap {
      padding: 0.5rem 1rem 0.6rem 1rem;
      border-bottom: 1px solid #21262d;
      margin-bottom: 0.5rem;
    }
    #adam-progress-header {
      display: flex; justify-content: space-between; align-items: center;
      margin-bottom: 0.4rem;
    }
    #adam-progress-stage {
      font-size: 0.75rem; color: #8b949e;
      font-family: 'DM Sans', sans-serif;
    }
    #adam-progress-pct {
      font-size: 0.7rem; font-family: 'JetBrains Mono', monospace; color: #6e7681;
    }
    #adam-progress-track {
      height: 3px; background: #21262d; border-radius: 2px; overflow: hidden;
    }
    #adam-progress-fill {
      height: 100%; width: 0%;
      background: linear-gradient(90deg, #2dd4bf, #0ea5e9);
      border-radius: 2px;
      transition: width 0.5s ease, background 0.4s;
    }
    @keyframes adam-shimmer {
      0%   { background-position: -300px 0; }
      100% { background-position: 300px 0; }
    }
    #adam-progress-fill.shimmer {
      background: linear-gradient(90deg, #2dd4bf 0%, #0ea5e9 40%, #2dd4bf 80%);
      background-size: 600px 100%;
      animation: adam-shimmer 1.8s infinite linear;
    }
    #adam-progress-footer {
      display: flex; justify-content: space-between; align-items: center;
      margin-top: 0.32rem;
    }
    #adam-progress-time, #adam-progress-tokens {
      font-size: 0.68rem; font-family: 'JetBrains Mono', monospace; color: #6e7681;
    }
    html.light-theme #adam-progress-wrap   { border-bottom-color: #d0d7de; }
    html.light-theme #adam-progress-stage  { color: #57606a; }
    html.light-theme #adam-progress-pct    { color: #6e7681; }
    html.light-theme #adam-progress-track  { background: #eaeef2; }
    html.light-theme #adam-progress-time,
    html.light-theme #adam-progress-tokens { color: #8c959f; }

    /* ── 主题切换按钮 ── */
    .theme-toggle-btn {
      background: transparent; border: 1px solid #30363d; color: #8b949e;
      border-radius: 5px; padding: 0.18rem 0.45rem; cursor: pointer;
      transition: border-color 0.2s, color 0.2s; line-height: 1;
      margin-left: auto; flex-shrink: 0; font-size: 0.85rem;
    }
    .theme-toggle-btn:hover { border-color: #2dd4bf; color: #2dd4bf; }
    .icon-light-mode { display: none; }

    /* ═══════════════════════════════════════════════════════════════
       浅色主题覆盖（html.light-theme 类激活时生效）
       ═══════════════════════════════════════════════════════════════ */
    html.light-theme {
      --bs-body-bg: #ffffff; --bs-body-color: #24292f;
      --bs-secondary-bg: #f6f8fa; --bs-border-color: #d0d7de;
      --bs-card-bg: #f6f8fa; color-scheme: light;
    }
    html.light-theme body { background-color: #ffffff !important; color: #24292f !important; }

    /* 图标切换 */
    html.light-theme .icon-dark-mode  { display: none; }
    html.light-theme .icon-light-mode { display: inline; }
    html.light-theme .theme-toggle-btn { border-color: #d0d7de; color: #57606a; }
    html.light-theme .theme-toggle-btn:hover { border-color: #0d9488; color: #0d9488; }

    /* 侧边栏 */
    html.light-theme .bslib-sidebar-layout > .sidebar {
      background: #f6f8fa !important; border-right-color: #d0d7de !important;
    }
    html.light-theme .form-control[type='file'] {
      background: #ffffff; border-color: #d0d7de; color: #57606a;
    }
    html.light-theme .form-control[type='file']:hover { border-color: #0d9488; color: #24292f; }
    html.light-theme .upload-label { color: #57606a; }

    /* 按钮 */
    html.light-theme #btn_clear_uploads { border-color: #d0d7de; color: #6e7681; }
    html.light-theme #btn_clear_uploads:hover { border-color: #f85149; color: #f85149; }
    html.light-theme .btn-download { border-color: #d0d7de; color: #57606a; }
    html.light-theme .btn-download:hover { border-color: #0d9488; color: #0d9488; }

    /* 导航 Tab */
    html.light-theme .nav-tabs { border-bottom-color: #d0d7de !important; }
    html.light-theme .nav-tabs .nav-link { color: #57606a !important; }
    html.light-theme .nav-tabs .nav-link:hover { color: #24292f !important; }
    html.light-theme .nav-tabs .nav-link.active {
      color: #0d9488 !important; border-bottom-color: #0d9488 !important;
    }

    /* 终端日志 */
    html.light-theme #run_status {
      background: #f6f8fa; border-color: #d0d7de; color: #1a7f37;
    }

    /* 卡片 */
    html.light-theme .card { background: #f6f8fa !important; border-color: #d0d7de !important; }
    html.light-theme .card-header {
      background: #ffffff !important; border-bottom-color: #d0d7de !important; color: #57606a;
    }

    /* Modal */
    html.light-theme .modal-content { background: #f6f8fa !important; border-color: #d0d7de !important; }
    html.light-theme .modal-header  { background: #ffffff !important; border-bottom-color: #d0d7de !important; }
    html.light-theme .modal-title   { color: #24292f !important; }
    html.light-theme .modal-footer  { background: #ffffff !important; border-top-color: #d0d7de !important; }
    html.light-theme .btn-close     { filter: none; }

    /* 解析报告表格 */
    html.light-theme .parse-map-table th {
      background: #ffffff; color: #57606a; border-bottom-color: #d0d7de;
    }
    html.light-theme .parse-map-table td { color: #24292f; border-bottom-color: #eaeef2; }
    html.light-theme .parse-map-table tr:hover td { background: rgba(0,0,0,0.02); }
    html.light-theme .preview-scroll { border-color: #d0d7de; }
    html.light-theme .preview-table th { background: #ffffff; color: #0d9488; border-bottom-color: #d0d7de; }
    html.light-theme .preview-table td { color: #57606a; border-bottom-color: #eaeef2; }
    html.light-theme .modal-section-title { color: #57606a; }

    /* Spec 状态卡片 */
    html.light-theme .spec-status-card { background: #ffffff; border-color: #d0d7de; }
    html.light-theme .spec-status-card .status-text { color: #57606a; }
    html.light-theme .spec-status-card .reopen-link { color: #0d9488; }

    /* DT 表格 */
    html.light-theme .dataTables_wrapper,
    html.light-theme table.dataTable thead th,
    html.light-theme table.dataTable tbody td { color: #24292f !important; }
    html.light-theme table.dataTable thead th {
      background: #f6f8fa !important; border-bottom-color: #d0d7de !important;
    }
    html.light-theme table.dataTable tbody tr         { background: #ffffff !important; }
    html.light-theme table.dataTable tbody tr:hover td { background: #f0f3f6 !important; }
    html.light-theme table.dataTable tbody tr.even td  { background: #ffffff !important; }
    html.light-theme .dataTables_info,
    html.light-theme .dataTables_length label,
    html.light-theme .dataTables_filter label { color: #57606a !important; }
    html.light-theme .dataTables_filter input,
    html.light-theme .dataTables_length select {
      background: #ffffff !important; border-color: #d0d7de !important; color: #24292f !important;
    }
    html.light-theme .dataTables_paginate .paginate_button { color: #57606a !important; }
    html.light-theme .dataTables_paginate .paginate_button.current {
      color: #0d9488 !important; background: rgba(13,148,136,0.1) !important;
      border-color: #0d9488 !important;
    }
    html.light-theme .bslib-value-box { border-color: #d0d7de !important; }

    /* 品牌栏 & 流水线 */
    html.light-theme .brand-bar   { border-bottom-color: #d0d7de; }
    html.light-theme .brand-title { color: #24292f; }
    html.light-theme .brand-sub   { color: #57606a; }
    html.light-theme hr.section-divider { border-color: #d0d7de; }
    html.light-theme .step-dot  { background: #d0d7de; }
    html.light-theme .step-label { color: #57606a; }
    html.light-theme .hint-text  { color: #6e7681; }

    /* 已上传文件卡片 */
    html.light-theme .uploaded-files-card { background: #ffffff; border-color: #d0d7de; }
    html.light-theme .uploaded-files-card .uf-title { color: #57606a; }
    html.light-theme .uploaded-files-card .uf-item  { color: #24292f; }
    html.light-theme .uploaded-files-card .uf-item.missing-item { color: #6e7681; }
    html.light-theme .uf-meta     { color: #57606a; }
    html.light-theme .uf-meta-sep { color: #d0d7de; }
    html.light-theme .btn-remove-file { color: #6e7681; }
    html.light-theme .btn-remove-file:hover { color: #f85149; }

    /* API 配置 */
    html.light-theme .api-config-section .form-control,
    html.light-theme .api-config-section .form-select {
      background: #ffffff !important; border-color: #d0d7de !important; color: #24292f !important;
    }
    html.light-theme .api-config-section .form-control:focus,
    html.light-theme .api-config-section .form-select:focus {
      border-color: #0d9488 !important; box-shadow: 0 0 0 2px rgba(13,148,136,0.15) !important;
    }
    html.light-theme .api-config-section label { color: #57606a !important; }
    html.light-theme .failover-section { border-top-color: #d0d7de; }
  ")

# =============================================================================
# UI 主体
# =============================================================================
ui <- page_sidebar(
  theme = adam_theme,
  title = NULL,

  # [新增] shinyjs 初始化，必须在 UI 顶层调用一次
  shinyjs::useShinyjs(),

  sidebar = sidebar(
    width = 280, open = TRUE,

    # ── 品牌栏 ───────────────────────────────────────────────────────────────
    tags$script(HTML("
      /* ── 主题切换 ── */
      function toggleAdamTheme() {
        var isLight = document.documentElement.classList.toggle('light-theme');
        Shiny.setInputValue('theme_is_light', isLight, {priority: 'event'});
      }

      /* ── API 进度条 ── */
      window.adamProgress = (function() {
        var active = false, crawlTimer = null, clockTimer = null;
        var pct = 0, startTime = null;

        function el(id) { return document.getElementById(id); }

        function _fmtTime(s) {
          if (s < 60) return s.toFixed(1) + 's';
          return Math.floor(s / 60) + 'm ' + Math.round(s % 60) + 's';
        }
        function _fmtNum(n) {
          if (!n || n <= 0) return '0';
          return n >= 1000 ? (n / 1000).toFixed(1) + 'k' : String(n);
        }

        function set(p, label, shimmer) {
          pct = p;
          var fill  = el('adam-progress-fill');
          var stage = el('adam-progress-stage');
          var pctEl = el('adam-progress-pct');
          if (!fill) return;
          if (p !== null) {
            fill.style.width = p + '%';
            if (pctEl) pctEl.textContent = Math.round(p) + '%';
          }
          if (label !== null && stage) stage.textContent = label;
          if (shimmer) fill.classList.add('shimmer');
          else         fill.classList.remove('shimmer');
        }

        function _stopTimers() {
          if (crawlTimer) { clearInterval(crawlTimer); crawlTimer = null; }
          if (clockTimer) { clearInterval(clockTimer); clockTimer = null; }
        }

        function start() {
          _stopTimers();
          active = true; pct = 0; startTime = Date.now();
          var fill = el('adam-progress-fill');
          if (fill) { fill.style.background = ''; fill.style.width = '0%'; }
          var timeEl = el('adam-progress-time'), tokEl = el('adam-progress-tokens');
          if (timeEl)   timeEl.textContent   = '';
          if (tokEl)    tokEl.textContent    = '';
          var wrap = el('adam-progress-wrap');
          if (wrap) { wrap.style.display = ''; wrap.style.opacity = '1'; wrap.style.transition = ''; }

          /* 实时时钟 */
          clockTimer = setInterval(function() {
            if (!active || !startTime) return;
            var t = el('adam-progress-time');
            if (t) t.textContent = '⏱ ' + _fmtTime((Date.now() - startTime) / 1000);
          }, 100);

          /* 阶段推进 */
          setTimeout(function() { if (active) set(8,  '✔ 验证输入...', false); }, 100);
          setTimeout(function() { if (active) set(22, '✔ 加载 SDTM 文件...', false); }, 700);
          setTimeout(function() { if (active) set(38, '构建 LLM Prompt...', false); }, 1400);
          setTimeout(function() {
            if (!active) return;
            set(48, '调用 LLM API，等待响应...', true);
            crawlTimer = setInterval(function() {
              if (!active) { clearInterval(crawlTimer); return; }
              var inc = pct < 65 ? 0.25 : 0.08;
              if (pct < 88) set(Math.min(88, pct + inc), null, true);
            }, 400);
          }, 2200);
        }

        function complete(msg, inputTok, outputTok) {
          active = false;
          _stopTimers();
          var elapsed = startTime ? (Date.now() - startTime) / 1000 : null;
          var fill = el('adam-progress-fill');
          if (fill) { fill.classList.remove('shimmer'); fill.style.background = '#3fb950'; }
          set(100, msg || '✔ LLM 生成完成', false);

          /* 最终时间 */
          var timeEl = el('adam-progress-time');
          if (timeEl && elapsed) timeEl.textContent = '⏱ ' + _fmtTime(elapsed);

          /* token 统计 */
          var tokEl = el('adam-progress-tokens');
          if (tokEl) {
            var total = (inputTok || 0) + (outputTok || 0);
            if (total > 0) {
              tokEl.textContent = _fmtNum(total) + ' tokens'
                + '  (↑' + _fmtNum(inputTok) + ' / ↓' + _fmtNum(outputTok) + ')';
            }
          }

          setTimeout(function() {
            var wrap = el('adam-progress-wrap');
            if (!wrap) return;
            wrap.style.transition = 'opacity 0.7s';
            wrap.style.opacity = '0';
            setTimeout(function() {
              wrap.style.display = 'none'; wrap.style.opacity = '1'; wrap.style.transition = '';
              if (fill) { fill.style.background = ''; fill.style.width = '0%'; }
            }, 700);
          }, 3500);
        }

        function error(msg) {
          active = false;
          _stopTimers();
          var elapsed = startTime ? (Date.now() - startTime) / 1000 : null;
          var fill = el('adam-progress-fill');
          if (fill) { fill.classList.remove('shimmer'); fill.style.background = '#f85149'; }
          set(null, msg || '✖ 调用失败', false);
          var timeEl = el('adam-progress-time');
          if (timeEl && elapsed) timeEl.textContent = '⏱ ' + _fmtTime(elapsed);
        }

        /* 按钮点击 → 启动进度条 */
        $(document).on('click', '#btn_generate', function() { start(); });

        /* 完成/失败信号由 server.R 通过 shinyjs::runjs() 直接调用 */

        return { start: start, complete: complete, error: error };
      })();
    ")),
    div(class = "brand-bar",
      div(class = "brand-icon", bs_icon("activity", size="1rem", color="#0d1117")),
      div(div(class="brand-title","ADaM Builder"), div(class="brand-sub","SDTM → ADaM  ·  AI-Assisted")),
      tags$button(
        class   = "theme-toggle-btn",
        title   = "切换明暗主题",
        onclick = "toggleAdamTheme()",
        tags$span(class="icon-dark-mode",  "☀"),   # 深色模式下显示（点击切换到浅色）
        tags$span(class="icon-light-mode", "🌙")   # 浅色模式下显示（点击切换回深色）
      )
    ),

    # ── [U-1] SDTM 上传（配置驱动，动态渲染）────────────────────────────────
    div(class="upload-label", bs_icon("database",size="0.7rem"), " SDTM 数据集"),
    # 域选择复选框（基础域 / 扩展域），由 server.R output$sdtm_domain_selector 渲染
    uiOutput("sdtm_domain_selector"),
    # 文件上传面板，依 rv$active_domains 动态生成，由 server.R output$sdtm_upload_panel 渲染
    uiOutput("sdtm_upload_panel"),

    # ── [新增] 已上传文件状态卡片 ──────────────────────────────────────────
    uiOutput("uploaded_files_list"),

    # ── [新增] 清空上传按钮 ────────────────────────────────────────────────
    # 点击后通过 shinyjs::reset() 重置所有 fileInput，并清空后台状态
    actionButton(
      inputId = "btn_clear_uploads",
      label   = tagList(bs_icon("trash3", size="0.72rem"), " 清空上传 (Clear Uploads)"),
      class   = "btn-outline-secondary"
    ),

    hr(class="section-divider"),

    # ── [修改 1] Specification 上传：.json → .csv ─────────────────────────────
    # 改动点：
    #   • bs_icon 从 file-earmark-code 改为 file-earmark-spreadsheet（表格图标）
    #   • fileInput accept 从 ".json" 改为 ".csv"
    #   • placeholder / hint-text 文字更新
    div(class="upload-label",
        bs_icon("file-earmark-spreadsheet", size="0.7rem"),   # [修改 1] 图标
        " Analysis Specification"),

    fileInput(
      inputId     = "file_spec",
      label       = NULL,
      accept      = ".csv",
      multiple    = TRUE,                                      # 支持同时上传多个 Spec CSV
      placeholder = "ADaM Spec (.csv)"
    ),
    div(class="hint-text",
        "可同时选择多个 CSV（如 ADSL Spec + ADAE Spec），AI 自动识别各文件的列结构"),

    # ── [新增 2] Spec 解析状态卡片 ───────────────────────────────────────────
    # 上传 CSV 后由 server 的 output$spec_parse_status 驱动渲染
    # 状态：idle / parsing / ok(N 变量已确认) / warn(有低置信度映射) / error
    uiOutput("spec_parse_status"),                             # [新增 2]

    hr(class="section-divider"),

    # ── LLM API 配置（动态面板，根据选中提供商切换内容）────────────────────
    div(class = "upload-label",
        bs_icon("key", size="0.7rem"), " LLM API 配置"),

    uiOutput("api_config_panel"),

    hr(class="section-divider"),

    # ── 流水线进度 ───────────────────────────────────────────────────────────
    div(class="upload-label", bs_icon("reception-4",size="0.7rem"), " 流水线状态"),
    uiOutput("pipeline_steps"),

    hr(class="section-divider"),

    # ── 生成按钮 ─────────────────────────────────────────────────────────────
    actionButton("btn_generate",
      tagList(bs_icon("cpu",size="0.9rem"), " 生成 ADaM 与代码"),
      class = "btn-primary"),
    div(class="hint-text", style="text-align:center;margin-top:0.5rem;",
        "需先完成 Spec 解析确认并填写 API Key")
  ),

  # ===========================================================================
  # 主面板（三个 Tab — 与上一版本完全相同，无修改）
  # ===========================================================================
  navset_tab(
    id = "main_tabs",

    # Tab 1 ── 运行与风险日志
    nav_panel(
      title = tagList(bs_icon("terminal"), " 运行与风险日志"),
      value = "tab_logs",
      layout_columns(col_widths=c(12), gap="1rem",
        layout_columns(col_widths=c(3,3,3,3), gap="0.75rem",
          value_box("ADSL 受试者数",  textOutput("vb_n_adsl",    inline=TRUE), showcase=bs_icon("people"),               theme="secondary", height="110px"),
          value_box("AE 总记录数",    textOutput("vb_n_adae",    inline=TRUE), showcase=bs_icon("clipboard2-pulse"),     theme="secondary", height="110px"),
          value_box("风险点识别",     textOutput("vb_n_risks",   inline=TRUE), showcase=bs_icon("exclamation-triangle"), theme="secondary", height="110px"),
          value_box("LLM 状态",       textOutput("vb_llm_status",inline=TRUE), showcase=bs_icon("robot"),                theme="secondary", height="110px")
        ),
        card(
          card_header(tagList(bs_icon("terminal-fill",size="0.75rem"),"  实时日志")),
          div(id="adam-progress-wrap", style="display:none;",
            div(id="adam-progress-header",
              span(id="adam-progress-stage", "准备中..."),
              span(id="adam-progress-pct",   "0%")
            ),
            div(id="adam-progress-track",
              div(id="adam-progress-fill")
            ),
            div(id="adam-progress-footer",
              span(id="adam-progress-time",   ""),
              span(id="adam-progress-tokens", "")
            )
          ),
          verbatimTextOutput("run_status")
        ),
        card(
          card_header(layout_columns(col_widths=c(8,4),
            div(tagList(bs_icon("shield-exclamation",size="0.75rem"),"  LLM 自主推断风险点")),
            div(style="text-align:right;",
              selectInput("filter_risk_level",NULL,
                choices=c("全部"="ALL","ERROR"="ERROR","WARNING"="WARNING","INFO"="INFO"),
                selected="ALL",width="130px"))
          )),
          DTOutput("tbl_risk_logs"),
          uiOutput("risk_logs_placeholder")
        )
      )
    ),

    # Tab 2 ── 代码审查与回档
    nav_panel(
      title = tagList(bs_icon("code-slash"), " 代码审查与回档"),
      value = "tab_code",
      layout_columns(col_widths=c(12), gap="1rem",
        card(
          style="background:rgba(45,212,191,0.06)!important;border:1px solid rgba(45,212,191,0.25)!important;padding:0.6rem 1rem;",
          p(style="margin:0;font-size:0.8rem;color:#8b949e;",
            bs_icon("info-circle",size="0.8rem",color="#2dd4bf"),
            " 下方代码由 LLM 自动生成。",
            tags$strong(style="color:#e6edf3;","请人工审阅后"),
            "再点击「确认并运行代码」执行。")),
        card(
          card_header(layout_columns(col_widths=c(7,5),
            div(tagList(bs_icon("file-earmark-code",size="0.75rem"),"  R 代码编辑器")),
            div(style="text-align:right;display:flex;gap:0.5rem;justify-content:flex-end;",
              actionButton("btn_reset_code",tagList(bs_icon("arrow-counterclockwise")," 重置"),
                class="btn-sm btn-outline-secondary",style="font-size:0.75rem;padding:0.3rem 0.7rem;"),
              textOutput("code_line_count",inline=TRUE)|>
                tagAppendAttributes(style="font-size:0.72rem;color:#6e7681;align-self:center;font-family:'JetBrains Mono',monospace;"))
          )),
          aceEditor("code_editor",value="# 请先上传文件并点击「生成 ADaM 与代码」...",
            mode="r",theme="tomorrow_night",height="460px",fontSize=13,
            showLineNumbers=TRUE,highlightActiveLine=TRUE,
            autoComplete="live",wordWrap=FALSE,readOnly=FALSE,debounce=500)
        ),
        layout_columns(col_widths=c(6,6), gap="0.75rem",
          uiOutput("run_code_status"),
          div(style="text-align:right;",
            actionButton("btn_run_code",tagList(bs_icon("play-circle-fill",size="1rem"),"  确认并运行代码")))
        )
      )
    ),

    # Tab 3 ── 输出数据集（[U-2] 改为动态渲染，支持任意数量的 ADaM 数据集）
    nav_panel(
      title = tagList(bs_icon("table"), " 输出数据集"),
      value = "tab_output",
      # 标签页内容由 server.R output$output_dataset_tabs 动态生成
      uiOutput("output_dataset_tabs")
    )
  )
)
