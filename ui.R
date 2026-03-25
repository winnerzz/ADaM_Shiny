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
    .sidebar-block {
      margin-bottom: 1rem;
    }
    .sidebar-section-card {
      background: #0d1117; border: 1px solid #21262d; border-radius: 6px;
      padding: 0.55rem 0.75rem; margin-top: 0.45rem;
    }
    .sidebar-section-title {
      display: flex; align-items: center; justify-content: space-between; gap: 0.5rem;
      margin-bottom: 0.25rem;
    }
    .sidebar-section-title .title-left {
      display: flex; align-items: center; gap: 0.45rem;
      font-size: 0.72rem; font-weight: 700; letter-spacing: 0.06em;
      text-transform: uppercase; color: #e6edf3;
    }
    .sidebar-section-body {
      color: #8b949e; font-size: 0.74rem; line-height: 1.45;
    }
    .summary-banner {
      background: linear-gradient(135deg, rgba(45,212,191,0.10), rgba(14,165,233,0.08));
      border: 1px solid rgba(45,212,191,0.25);
      border-radius: 8px;
      padding: 0.9rem 1rem;
      margin-bottom: 0.9rem;
    }
    .summary-banner.warn {
      background: rgba(210,153,34,0.08);
      border-color: rgba(210,153,34,0.28);
    }
    .summary-banner.error {
      background: rgba(248,81,73,0.08);
      border-color: rgba(248,81,73,0.30);
    }
    .summary-banner-title {
      display: flex; align-items: center; gap: 0.5rem;
      color: #e6edf3; font-weight: 700; font-size: 0.86rem;
      margin-bottom: 0.35rem;
    }
    .summary-banner-text {
      color: #8b949e; font-size: 0.78rem; line-height: 1.55;
    }
    .context-strip {
      background: #0d1117; border: 1px solid #21262d; border-radius: 6px;
      padding: 0.6rem 0.8rem; margin-bottom: 0.8rem;
    }
    .context-strip-title {
      color: #e6edf3; font-size: 0.76rem; font-weight: 700;
      letter-spacing: 0.05em; text-transform: uppercase; margin-bottom: 0.35rem;
    }
    .context-strip-body {
      color: #8b949e; font-size: 0.75rem; line-height: 1.5;
    }
    .workflow-overview-card {
      background: linear-gradient(180deg, rgba(45,212,191,0.08), rgba(13,17,23,0.96));
      border: 1px solid rgba(45,212,191,0.18);
      border-radius: 8px;
      padding: 0.8rem 0.85rem;
      margin-bottom: 1rem;
    }
    .workflow-overview-title {
      color: #e6edf3; font-size: 0.75rem; font-weight: 700;
      letter-spacing: 0.08em; text-transform: uppercase; margin-bottom: 0.6rem;
    }
    .workflow-step-row {
      display: flex; align-items: flex-start; gap: 0.55rem;
      padding: 0.4rem 0;
      border-top: 1px solid rgba(255,255,255,0.04);
    }
    .workflow-step-row:first-of-type { border-top: none; padding-top: 0; }
    .workflow-step-dot {
      width: 8px; height: 8px; border-radius: 50%; margin-top: 0.32rem; flex-shrink: 0;
      background: #30363d;
    }
    .workflow-step-dot.active { background: #2dd4bf; box-shadow: 0 0 0 4px rgba(45,212,191,0.12); }
    .workflow-step-dot.done { background: #3fb950; }
    .workflow-step-dot.warn { background: #d29922; }
    .workflow-step-copy { min-width: 0; }
    .workflow-step-name { color: #e6edf3; font-size: 0.76rem; font-weight: 600; margin-bottom: 0.14rem; }
    .workflow-step-desc { color: #8b949e; font-size: 0.72rem; line-height: 1.45; }
    .workflow-details {
      background: #0d1117; border: 1px solid #21262d; border-radius: 8px;
      margin-bottom: 0.8rem; overflow: hidden;
    }
    .workflow-details > summary {
      list-style: none; cursor: pointer; padding: 0.7rem 0.8rem;
      display: flex; align-items: center; justify-content: space-between; gap: 0.7rem;
      color: #e6edf3; font-size: 0.78rem; font-weight: 700;
      letter-spacing: 0.04em;
    }
    .workflow-details > summary::-webkit-details-marker { display: none; }
    .workflow-details > summary .summary-meta {
      color: #8b949e; font-size: 0.68rem; font-weight: 500; letter-spacing: normal;
    }
    .workflow-details-body {
      padding: 0 0.8rem 0.8rem 0.8rem;
      border-top: 1px solid #21262d;
    }
    .hero-panel {
      background: linear-gradient(135deg, rgba(45,212,191,0.14), rgba(14,165,233,0.08) 45%, rgba(13,17,23,0.96));
      border: 1px solid rgba(45,212,191,0.18);
      border-radius: 10px;
      padding: 1.1rem 1.2rem;
    }
    .hero-kicker {
      color: #2dd4bf; font-size: 0.7rem; font-weight: 700; letter-spacing: 0.12em;
      text-transform: uppercase; margin-bottom: 0.45rem;
    }
    .hero-title {
      color: #e6edf3; font-family: 'Syne', sans-serif; font-size: 1.35rem;
      line-height: 1.08; margin-bottom: 0.45rem;
    }
    .hero-text {
      color: #9fb0c2; font-size: 0.84rem; line-height: 1.6; max-width: 860px;
    }
    .hero-meta {
      display: flex; flex-wrap: wrap; gap: 0.45rem; margin-top: 0.8rem;
    }
    .hero-pill {
      display: inline-flex; align-items: center;
      background: rgba(255,255,255,0.05); border: 1px solid rgba(255,255,255,0.08);
      border-radius: 999px; padding: 0.16rem 0.55rem;
      color: #c9d1d9; font-size: 0.7rem; font-family: 'JetBrains Mono', monospace;
    }
    .metric-grid {
      display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.75rem;
    }
    .metric-card {
      background: #0d1117; border: 1px solid #21262d; border-radius: 8px;
      padding: 0.75rem 0.85rem;
    }
    .metric-label {
      color: #8b949e; font-size: 0.68rem; letter-spacing: 0.08em; text-transform: uppercase;
      margin-bottom: 0.35rem;
    }
    .metric-value {
      color: #e6edf3; font-size: 1.1rem; font-weight: 700; line-height: 1.1; margin-bottom: 0.22rem;
    }
    .metric-note {
      color: #8b949e; font-size: 0.73rem; line-height: 1.5;
    }
    .workspace-card {
      background: #0d1117; border: 1px solid #21262d; border-radius: 8px;
      padding: 0.95rem 1rem;
    }
    .workspace-kicker {
      color: #8b949e; font-size: 0.68rem; font-weight: 700; letter-spacing: 0.1em;
      text-transform: uppercase; margin-bottom: 0.35rem;
    }
    .workspace-title {
      color: #e6edf3; font-size: 1rem; font-weight: 700; margin-bottom: 0.38rem;
    }
    .workspace-text {
      color: #9fb0c2; font-size: 0.78rem; line-height: 1.6;
    }
    .workspace-actions {
      display: flex; flex-wrap: wrap; gap: 0.55rem; margin-top: 0.85rem;
    }
    .btn-ghost-workflow {
      background: transparent; border: 1px solid #30363d; border-radius: 6px;
      color: #c9d1d9; font-size: 0.76rem; font-weight: 600; padding: 0.45rem 0.8rem;
    }
    .btn-ghost-workflow:hover { border-color: #2dd4bf; color: #2dd4bf; }
    .digest-list { display: flex; flex-direction: column; gap: 0.65rem; }
    .digest-item {
      background: #0d1117; border: 1px solid #21262d; border-radius: 8px;
      padding: 0.72rem 0.85rem;
    }
    .digest-item-head {
      display: flex; align-items: center; justify-content: space-between; gap: 0.7rem;
      margin-bottom: 0.28rem;
    }
    .digest-item-title {
      color: #e6edf3; font-size: 0.8rem; font-weight: 700;
    }
    .digest-item-text {
      color: #8b949e; font-size: 0.75rem; line-height: 1.5;
    }
    .detail-section-title {
      color: #8b949e; font-size: 0.7rem; letter-spacing: 0.08em; text-transform: uppercase;
      margin-bottom: 0.45rem;
    }
    .domain-upload-meta {
      margin: 0 0 0.3rem 0;
      font-size: 0.73rem;
      color: #8b949e;
      line-height: 1.45;
    }
    .input-next-step-card {
      background: linear-gradient(135deg, rgba(45,212,191,0.08), rgba(14,165,233,0.05));
      border: 1px solid rgba(45,212,191,0.22);
      border-radius: 8px;
      padding: 0.9rem 1rem;
    }
    .input-next-step-title {
      color: #e6edf3;
      font-size: 0.86rem;
      font-weight: 700;
      margin-bottom: 0.35rem;
    }
    .input-next-step-text {
      color: #8b949e;
      font-size: 0.77rem;
      line-height: 1.55;
    }
    .input-next-step-actions {
      display: flex;
      flex-wrap: wrap;
      gap: 0.6rem;
      margin-top: 0.8rem;
    }
    .input-prep-grid {
      display: grid;
      grid-template-columns: minmax(0, 1.08fr) minmax(0, 0.92fr);
      gap: 1rem;
      align-items: stretch;
    }
    .input-prep-main-card,
    .input-prep-side-card {
      height: 100%;
    }
    .input-prep-main-card .card-body,
    .input-prep-side-card .card-body {
      display: flex;
      flex-direction: column;
      gap: 0.9rem;
    }
    .input-section-block {
      background: rgba(255,255,255,0.02);
      border: 1px solid #21262d;
      border-radius: 8px;
      padding: 0.85rem 0.9rem;
    }
    .input-section-title {
      color: #e6edf3;
      font-size: 0.76rem;
      font-weight: 700;
      letter-spacing: 0.05em;
      text-transform: uppercase;
      margin-bottom: 0.42rem;
    }
    .input-section-meta {
      color: #8b949e;
      font-size: 0.73rem;
      line-height: 1.5;
      margin-bottom: 0.7rem;
    }
    .sdtm-upload-region {
      max-height: 430px;
      overflow-y: auto;
      padding-right: 0.2rem;
    }
    .sdtm-upload-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 0.75rem;
    }
    .sdtm-upload-card {
      background: rgba(13,17,23,0.82);
      border: 1px solid #21262d;
      border-radius: 8px;
      padding: 0.7rem 0.75rem;
      min-width: 0;
    }
    .sdtm-upload-card .form-group {
      margin-bottom: 0;
    }
    .sdtm-upload-card .form-control[type='file'] {
      margin-top: 0.35rem;
    }
    .input-side-stack {
      display: flex;
      flex-direction: column;
      gap: 0.9rem;
      height: 100%;
    }
    .input-side-stack .input-next-step-card {
      margin-top: auto;
    }
    .validation-mini-card {
      background: var(--bs-secondary-bg);
      border: 1px solid var(--bs-border-color);
      border-left-width: 4px;
      border-radius: 6px;
      padding: 0.55rem 0.75rem;
      font-size: 0.76rem;
    }
    .validation-mini-card .dataset-name {
      color: var(--bs-body-color);
      font-weight: 700;
    }
    .validation-mini-card .dataset-meta {
      margin-top: 0.25rem;
      color: #8b949e;
    }
    .run-status-note {
      font-size: 0.78rem;
      color: #6e7681;
      align-self: center;
      display: flex;
      align-items: center;
      gap: 0.45rem;
      line-height: 1.5;
    }
    .detail-accordion {
      background: #0d1117; border: 1px solid #21262d; border-radius: 8px;
      margin-bottom: 0.85rem; overflow: hidden;
    }
    .detail-accordion > summary {
      list-style: none; cursor: pointer;
      display: flex; align-items: center; justify-content: space-between; gap: 0.8rem;
      padding: 0.8rem 0.95rem;
      color: #e6edf3; font-size: 0.8rem; font-weight: 700;
    }
    .detail-accordion > summary::-webkit-details-marker { display: none; }
    .detail-accordion-head {
      display: flex; align-items: center; gap: 0.5rem;
    }
    .detail-accordion-meta {
      color: #8b949e; font-size: 0.7rem; font-weight: 500;
    }
    .detail-accordion-body {
      border-top: 1px solid #21262d;
      padding: 0.95rem;
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
    .badge-pass-custom {
      background: rgba(63,185,80,0.15); color: #3fb950;
      border: 1px solid rgba(63,185,80,0.35);
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
    .llm-tuning-section {
      margin-top: 0.45rem; padding-top: 0.55rem; border-top: 1px solid #21262d;
    }
    .llm-status-card {
      background: #0d1117; border: 1px solid #21262d; border-radius: 6px;
      padding: 0.65rem 0.8rem; margin-top: 0.55rem;
    }
    .llm-status-title {
      display: flex; align-items: center; gap: 0.45rem;
      color: #e6edf3; font-size: 0.76rem; font-weight: 600;
      margin-bottom: 0.48rem; letter-spacing: 0.04em;
    }
    .llm-chip-row {
      display: flex; flex-wrap: wrap; gap: 0.35rem; margin-bottom: 0.45rem;
    }
    .llm-chip {
      display: inline-flex; align-items: center;
      background: rgba(255,255,255,0.04); border: 1px solid #30363d;
      border-radius: 999px; padding: 0.12rem 0.48rem;
      color: #8b949e; font-size: 0.68rem; font-family: 'JetBrains Mono',monospace;
    }
    .llm-chip-primary {
      background: rgba(45,212,191,0.12); color: #2dd4bf;
      border-color: rgba(45,212,191,0.35);
    }
    .llm-status-meta {
      color: #8b949e; font-size: 0.72rem; line-height: 1.5;
      display: flex; flex-wrap: wrap; gap: 0.35rem;
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
    .api-config-section .form-select,
    .api-config-section .selectize-input,
    .api-config-section .selectize-dropdown {
      background: #0d1117 !important;
      border: 1px solid #30363d !important;
      color: #c9d1d9 !important;
      border-radius: 5px !important;
      font-size: 0.8rem !important;
      padding: 0.35rem 0.6rem !important;
    }
    .api-config-section .selectize-input input,
    .api-config-section .selectize-input > div,
    .api-config-section .selectize-input .item,
    .api-config-section .selectize-input .active,
    .api-config-section .selectize-control.single .selectize-input:after,
    .api-config-section .selectize-input::placeholder,
    .api-config-section .form-control::placeholder,
    .api-config-section .selectize-dropdown .option,
    .api-config-section .selectize-dropdown .optgroup-header,
    .api-config-section select option {
      color: #c9d1d9 !important;
      background: #0d1117 !important;
    }
    .api-config-section input,
    .api-config-section textarea,
    .api-config-section select {
      color: #c9d1d9 !important;
      -webkit-text-fill-color: #c9d1d9 !important;
      caret-color: #c9d1d9 !important;
    }
    .api-config-section .selectize-dropdown-content {
      background: #0d1117 !important;
      color: #c9d1d9 !important;
    }
    .api-config-section .form-control:focus,
    .api-config-section .form-select:focus,
    .api-config-section .selectize-input.focus {
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
    .password-field-shell {
      margin-bottom: 0.6rem;
    }
    .password-field-label {
      display: block;
      font-size: 0.7rem;
      color: #8b949e;
      margin-bottom: 0.2rem;
    }
    .password-field-wrap {
      position: relative;
    }
    .password-field-wrap .form-control {
      padding-right: 2.5rem !important;
    }
    .password-toggle-btn {
      position: absolute;
      top: 50%;
      right: 0.45rem;
      transform: translateY(-50%);
      border: none;
      background: transparent;
      color: #8b949e;
      width: 1.8rem;
      height: 1.8rem;
      border-radius: 999px;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      cursor: pointer;
      transition: color 0.2s, background 0.2s;
    }
    .password-toggle-btn:hover {
      color: #2dd4bf;
      background: rgba(45,212,191,0.08);
    }
    .password-toggle-btn .icon-hide { display: none; }
    .password-toggle-btn.is-visible .icon-show { display: none; }
    .password-toggle-btn.is-visible .icon-hide { display: inline-flex; }

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
    .top-action-btn {
      background: transparent; border: 1px solid #30363d; color: #c9d1d9;
      border-radius: 6px; padding: 0.28rem 0.6rem; cursor: pointer;
      transition: border-color 0.2s, color 0.2s, background 0.2s;
      font-size: 0.74rem; font-weight: 600; letter-spacing: 0.04em;
      display: inline-flex; align-items: center; gap: 0.38rem;
    }
    .top-action-btn:hover { border-color: #2dd4bf; color: #2dd4bf; background: rgba(45,212,191,0.06); }
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
    html.light-theme .top-action-btn { border-color: #d0d7de; color: #57606a; background: #ffffff; }
    html.light-theme .top-action-btn:hover { border-color: #0d9488; color: #0d9488; background: rgba(13,148,136,0.05); }

    /* 侧边栏 */
    html.light-theme .bslib-sidebar-layout > .sidebar {
      background: #f6f8fa !important; border-right-color: #d0d7de !important;
    }
    html.light-theme .form-control[type='file'] {
      background: #ffffff; border-color: #d0d7de; color: #57606a;
    }
    html.light-theme .form-control[type='file']:hover { border-color: #0d9488; color: #24292f; }
    html.light-theme .upload-label { color: #57606a; }
    html.light-theme .sidebar-section-card,
    html.light-theme .context-strip {
      background: #ffffff; border-color: #d0d7de;
    }
    html.light-theme .sidebar-section-title .title-left,
    html.light-theme .summary-banner-title,
    html.light-theme .context-strip-title { color: #24292f; }
    html.light-theme .sidebar-section-body,
    html.light-theme .context-strip-body,
    html.light-theme .summary-banner-text { color: #57606a; }
    html.light-theme .summary-banner {
      background: linear-gradient(135deg, rgba(13,148,136,0.08), rgba(14,165,233,0.06));
      border-color: rgba(13,148,136,0.25);
    }
    html.light-theme .summary-banner.warn {
      background: rgba(210,153,34,0.08); border-color: rgba(210,153,34,0.28);
    }
    html.light-theme .summary-banner.error {
      background: rgba(248,81,73,0.08); border-color: rgba(248,81,73,0.28);
    }

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
    html.light-theme .llm-tuning-section { border-top-color: #d0d7de; }
    html.light-theme .llm-status-card { background: #ffffff; border-color: #d0d7de; }
    html.light-theme .llm-status-title { color: #24292f; }
    html.light-theme .llm-chip {
      background: #f6f8fa; border-color: #d0d7de; color: #57606a;
    }
    html.light-theme .llm-chip-primary {
      background: rgba(13,148,136,0.08); border-color: rgba(13,148,136,0.25); color: #0d9488;
    }
    html.light-theme .llm-status-meta { color: #57606a; }

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
    html.light-theme .api-config-section .form-select,
    html.light-theme .api-config-section .selectize-input,
    html.light-theme .api-config-section .selectize-dropdown {
      background: #ffffff !important; border-color: #d0d7de !important; color: #24292f !important;
    }
    html.light-theme .api-config-section .selectize-input input,
    html.light-theme .api-config-section .selectize-input > div,
    html.light-theme .api-config-section .selectize-input .item,
    html.light-theme .api-config-section .selectize-input .active,
    html.light-theme .api-config-section .selectize-control.single .selectize-input:after,
    html.light-theme .api-config-section .selectize-input::placeholder,
    html.light-theme .api-config-section .form-control::placeholder,
    html.light-theme .api-config-section .selectize-dropdown .option,
    html.light-theme .api-config-section .selectize-dropdown .optgroup-header,
    html.light-theme .api-config-section select option {
      color: #24292f !important;
      background: #ffffff !important;
    }
    html.light-theme .api-config-section input,
    html.light-theme .api-config-section textarea,
    html.light-theme .api-config-section select {
      color: #24292f !important;
      -webkit-text-fill-color: #24292f !important;
      caret-color: #24292f !important;
    }
    html.light-theme .api-config-section .selectize-dropdown-content {
      background: #ffffff !important;
      color: #24292f !important;
    }
    html.light-theme .api-config-section .form-control:focus,
    html.light-theme .api-config-section .form-select:focus,
    html.light-theme .api-config-section .selectize-input.focus {
      border-color: #0d9488 !important; box-shadow: 0 0 0 2px rgba(13,148,136,0.15) !important;
    }
    html.light-theme .api-config-section label { color: #57606a !important; }
    html.light-theme .password-field-label { color: #57606a !important; }
    html.light-theme .password-toggle-btn { color: #57606a; }
    html.light-theme .password-toggle-btn:hover {
      color: #0d9488;
      background: rgba(13,148,136,0.08);
    }
    html.light-theme .failover-section { border-top-color: #d0d7de; }
    html.light-theme .workflow-overview-card,
    html.light-theme .hero-panel {
      background: linear-gradient(135deg, rgba(13,148,136,0.08), rgba(255,255,255,0.96));
      border-color: rgba(13,148,136,0.18);
    }
    html.light-theme .workflow-overview-title,
    html.light-theme .workflow-step-name,
    html.light-theme .workflow-details > summary,
    html.light-theme .hero-title,
    html.light-theme .metric-value,
    html.light-theme .workspace-title,
    html.light-theme .digest-item-title { color: #24292f; }
    html.light-theme .workflow-step-desc,
    html.light-theme .workflow-details > summary .summary-meta,
    html.light-theme .hero-text,
    html.light-theme .metric-note,
    html.light-theme .workspace-text,
    html.light-theme .digest-item-text,
    html.light-theme .detail-section-title,
    html.light-theme .domain-upload-meta,
    html.light-theme .input-next-step-text,
    html.light-theme .validation-mini-card .dataset-meta,
    html.light-theme .run-status-note { color: #57606a; }
    html.light-theme .workflow-details,
    html.light-theme .metric-card,
    html.light-theme .workspace-card,
    html.light-theme .digest-item,
    html.light-theme .detail-accordion { background: #ffffff; border-color: #d0d7de; }
    html.light-theme .workflow-details-body { border-top-color: #d0d7de; }
    html.light-theme .detail-accordion > summary { color: #24292f; }
    html.light-theme .detail-accordion-meta { color: #57606a; }
    html.light-theme .detail-accordion-body { border-top-color: #d0d7de; }
    html.light-theme .input-next-step-card {
      background: linear-gradient(135deg, rgba(13,148,136,0.08), rgba(255,255,255,0.96));
      border-color: rgba(13,148,136,0.2);
    }
    html.light-theme .input-next-step-title { color: #24292f; }
    html.light-theme .input-section-block,
    html.light-theme .sdtm-upload-card {
      background: #ffffff;
      border-color: #d0d7de;
    }
    html.light-theme .input-section-title { color: #24292f; }
    html.light-theme .input-section-meta { color: #57606a; }
    html.light-theme .hero-pill {
      background: #ffffff; border-color: #d0d7de; color: #57606a;
    }
    html.light-theme .btn-ghost-workflow { border-color: #d0d7de; color: #57606a; }
    html.light-theme .btn-ghost-workflow:hover { border-color: #0d9488; color: #0d9488; }

    @media (max-width: 991px) {
      .input-prep-grid { grid-template-columns: 1fr; }
      .sdtm-upload-grid { grid-template-columns: 1fr; }
      .sdtm-upload-region { max-height: none; overflow: visible; }
    }
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
    width = 320, open = TRUE,

    # ── 品牌栏 ───────────────────────────────────────────────────────────────
    tags$script(HTML("
      /* ── 主题切换 ── */
      function toggleAdamTheme() {
        var isLight = document.documentElement.classList.toggle('light-theme');
        Shiny.setInputValue('theme_is_light', isLight, {priority: 'event'});
      }

      function togglePasswordVisibility(targetId, buttonEl) {
        var input = document.getElementById(targetId);
        if (!input) return;
        var visible = input.type === 'text';
        input.type = visible ? 'password' : 'text';
        if (buttonEl) {
          if (visible) buttonEl.classList.remove('is-visible');
          else buttonEl.classList.add('is-visible');
        }
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
      actionButton("btn_open_ai_settings",
        label = tagList(bs_icon("sliders", size = "0.78rem"), " AI 设置"),
        class = "top-action-btn"),
      tags$button(
        class   = "theme-toggle-btn",
        title   = "切换明暗主题",
        onclick = "toggleAdamTheme()",
        tags$span(class="icon-dark-mode",  "☀"),   # 深色模式下显示（点击切换到浅色）
        tags$span(class="icon-light-mode", "🌙")   # 浅色模式下显示（点击切换回深色）
      )
    ),
    uiOutput("sidebar_workflow_overview"),
    uiOutput("llm_config_status"),
    uiOutput("next_action_hint")
  ),

  navset_tab(
    id = "main_tabs",
    nav_panel(
      title = tagList(bs_icon("inboxes"), " 输入准备"),
      value = "tab_input",
      layout_columns(col_widths = c(12), gap = "1rem",
        uiOutput("workflow_hero"),
        div(class = "input-prep-grid",
          card(
            class = "input-prep-main-card",
            card_header(tagList(bs_icon("database", size="0.75rem"), "  SDTM 输入范围与文件")),
            uiOutput("sdtm_section_status"),
            div(class = "input-section-block",
              div(class = "input-section-title", "域范围"),
              div(class = "input-section-meta", "核心域始终保留；可按分析需要启用其他域。上传区会仅展示当前启用域。"),
              uiOutput("sdtm_domain_selector")
            ),
            div(class = "input-section-block",
              div(class = "input-section-title", "CSV 上传"),
              div(class = "input-section-meta", "上传当前启用域对应的 SDTM CSV。域较多时，下方区域会保持在当前卡片内滚动，不再拉长整页布局。"),
              div(class = "sdtm-upload-region",
                uiOutput("sdtm_upload_panel")
              )
            )
          ),
          card(
            class = "input-prep-side-card",
            card_header(tagList(bs_icon("file-earmark-spreadsheet", size="0.75rem"), "  Analysis Specification 与后续动作")),
            div(class = "input-side-stack",
              div(class = "input-section-block",
                div(class = "input-section-title", "Analysis Specification"),
                div(class = "input-section-meta", "可同时选择多个 Spec CSV。系统会自动解析列结构，并在确认后进入生成准备状态。"),
                fileInput(
                  inputId     = "file_spec",
                  label       = NULL,
                  accept      = ".csv",
                  multiple    = TRUE,
                  placeholder = "ADaM Spec (.csv)"
                ),
                uiOutput("spec_parse_status")
              ),
              uiOutput("input_next_step")
            )
          )
        ),
        card(
          card_header(tagList(bs_icon("folder2-open", size="0.75rem"), "  已选文件")),
          uiOutput("uploaded_files_list"),
          div(style="height:0.85rem;"),
          uiOutput("uploaded_preview_gallery"),
          div(style="margin-top:0.8rem;text-align:right;",
            actionButton(
              inputId = "btn_clear_uploads",
              label   = tagList(bs_icon("trash3", size="0.72rem"), " 清空上传"),
              class   = "btn-outline-secondary"
            )
          )
        )
      )
    ),
    nav_panel(
      title = tagList(bs_icon("stars"), " 生成与审阅"),
      value = "tab_generate",
      layout_columns(col_widths = c(12), gap = "1rem",
        uiOutput("workflow_hero"),
        layout_columns(col_widths = c(8,4), gap = "1rem",
          card(
            card_header(tagList(bs_icon("compass", size="0.75rem"), "  当前任务")),
            uiOutput("current_step_workspace"),
            uiOutput("code_context_summary"),
            uiOutput("profile_context_summary"),
            uiOutput("plan_context_summary")
          ),
          card(
            card_header(tagList(bs_icon("sliders", size="0.75rem"), "  AI 与生成")),
            uiOutput("llm_section_status"),
            uiOutput("llm_config_status"),
            actionButton("btn_open_ai_settings_inline",
              label = tagList(bs_icon("sliders", size = "0.78rem"), " 打开 AI 设置"),
              class = "btn-outline-secondary"),
            div(style="height:0.65rem;"),
            actionButton("btn_generate",
              tagList(bs_icon("cpu",size="0.9rem"), " 生成 ADaM 与代码"),
              class = "btn-primary"),
            div(style="height:0.8rem;"),
            uiOutput("run_code_status"),
            div(style="height:0.5rem;"),
            uiOutput("next_action_hint")
          )
        ),
        card(
          card_header(tagList(bs_icon("file-earmark-code",size="0.75rem"),"  代码编辑器")),
          aceEditor("code_editor",value="# 请先在“输入准备”完成文件上传，然后点击生成...",
            mode="r",theme="tomorrow_night",height="460px",fontSize=13,
            showLineNumbers=TRUE,highlightActiveLine=TRUE,
            autoComplete="live",wordWrap=FALSE,readOnly=FALSE,debounce=500),
          div(style="display:flex;justify-content:space-between;align-items:center;margin-top:0.85rem;gap:0.75rem;flex-wrap:wrap;",
            textOutput("code_line_count",inline=TRUE) |>
              tagAppendAttributes(style="font-size:0.72rem;color:#6e7681;font-family:'JetBrains Mono',monospace;"),
            div(style="display:flex;gap:0.6rem;justify-content:flex-end;flex-wrap:wrap;",
              actionButton("btn_reset_code",tagList(bs_icon("arrow-counterclockwise")," 重置"),
                class="btn-sm btn-outline-secondary",style="font-size:0.75rem;padding:0.3rem 0.7rem;"),
              actionButton("btn_run_code",tagList(bs_icon("play-circle-fill",size="1rem")," 确认并运行代码"))
            )
          )
        ),
        tags$details(class="detail-accordion",
          tags$summary(
            div(class="detail-accordion-head", bs_icon("diagram-3", size="0.8rem"), "输入数据画像"),
            span(class="detail-accordion-meta", "启动生成并完成 SDTM 读取后显示")
          ),
          div(class="detail-accordion-body",
            DTOutput("tbl_sdtm_profile"),
            uiOutput("sdtm_profile_placeholder")
          )
        ),
        tags$details(class="detail-accordion",
          tags$summary(
            div(class="detail-accordion-head", bs_icon("bezier2", size="0.8rem"), "生成计划明细"),
            span(class="detail-accordion-meta", "查看变量级 derivation plan")
          ),
          div(class="detail-accordion-body",
            DTOutput("tbl_plan_variables"),
            uiOutput("plan_variables_placeholder")
          )
        ),
        tags$details(class="detail-accordion",
          tags$summary(
            div(class="detail-accordion-head", bs_icon("shield-exclamation", size="0.8rem"), "LLM 风险明细"),
            span(class="detail-accordion-meta", "仅在需要时展开查看")
          ),
          div(class="detail-accordion-body",
            div(style="max-width:160px;margin-left:auto;margin-bottom:0.75rem;",
              selectInput("filter_risk_level",NULL,
                choices=c("全部"="ALL","ERROR"="ERROR","WARNING"="WARNING","INFO"="INFO"),
                selected="ALL",width="100%")
            ),
            DTOutput("tbl_risk_logs"),
            uiOutput("risk_logs_placeholder")
          )
        ),
        tags$details(class="detail-accordion",
          tags$summary(
            div(class="detail-accordion-head", bs_icon("clipboard2-check", size="0.8rem"), "结构与语义校验"),
            span(class="detail-accordion-meta", "执行后查看结构、质量与 plan 对齐结果")
          ),
          div(class="detail-accordion-body",
            uiOutput("validation_overview"),
            div(style="max-width:160px;margin-left:auto;margin-bottom:0.75rem;margin-top:0.8rem;",
              selectInput("filter_validation_level", NULL,
                choices=c("全部"="ALL","ERROR"="ERROR","WARNING"="WARNING","INFO"="INFO"),
                selected="ALL", width="100%")
            ),
            DTOutput("tbl_validation"),
            uiOutput("validation_placeholder")
          )
        ),
        tags$details(class="detail-accordion",
          tags$summary(
            div(class="detail-accordion-head", bs_icon("terminal-fill", size="0.8rem"), "运行日志"),
            span(class="detail-accordion-meta", "查看完整执行与模型调用日志")
          ),
          div(class="detail-accordion-body",
            uiOutput("pipeline_steps"),
            div(style="height:0.7rem;"),
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
            div(style="margin-top:0.7rem;"),
            verbatimTextOutput("run_status")
          )
        )
      )
    ),
    nav_panel(
      title = tagList(bs_icon("table"), " 输出结果"),
      value = "tab_output",
      uiOutput("output_context_summary"),
      uiOutput("output_dataset_tabs")
    )
  )
)
