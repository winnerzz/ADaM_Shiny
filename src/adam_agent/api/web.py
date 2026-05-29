"""Static local web UI for the product-facing Phase 8 workflow."""

from __future__ import annotations


INDEX_HTML = r"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>ADaM Agent Studio</title>
  <style>
    :root {
      color-scheme: light;
      --bg: #f5f7fa;
      --panel: #ffffff;
      --line: #d8dee8;
      --text: #172033;
      --muted: #637083;
      --soft: #edf3f6;
      --accent: #0f766e;
      --accent-dark: #115e59;
      --ok: #067647;
      --warn: #9a5b00;
      --danger: #b42318;
      --code: #101827;
    }
    * { box-sizing: border-box; }
    body {
      margin: 0;
      min-height: 100vh;
      font-family: Arial, Helvetica, sans-serif;
      color: var(--text);
      background: var(--bg);
    }
    header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 18px;
      padding: 16px 22px;
      border-bottom: 1px solid var(--line);
      background: var(--panel);
    }
    h1 { margin: 0 0 4px; font-size: 22px; }
    h2 { margin: 0; font-size: 17px; }
    h3 { margin: 0 0 8px; font-size: 14px; }
    .subtitle, .muted, .status-line {
      color: var(--muted);
      font-size: 13px;
      line-height: 1.45;
    }
    .header-status {
      display: grid;
      gap: 7px;
      min-width: 340px;
      max-width: 460px;
    }
    .status-card {
      padding: 10px 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .status-row {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      margin-bottom: 4px;
    }
    .status-label {
      color: var(--muted);
      font-size: 11px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .status-detail {
      color: var(--text);
      font-size: 13px;
      line-height: 1.35;
      overflow-wrap: anywhere;
    }
    main {
      display: grid;
      grid-template-columns: 280px minmax(700px, 1fr);
      gap: 14px;
      padding: 14px;
      min-height: calc(100vh - 74px);
    }
    aside, section, .card {
      background: var(--panel);
      border: 1px solid var(--line);
      border-radius: 8px;
    }
    aside {
      padding: 12px;
      height: fit-content;
      position: sticky;
      top: 14px;
    }
    section { margin-bottom: 14px; }
    .section-head {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 12px;
      padding: 14px 16px;
      border-bottom: 1px solid var(--line);
    }
    .section-body { padding: 16px; }
    .step {
      display: grid;
      grid-template-columns: 28px 1fr;
      gap: 9px;
      padding: 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
      margin-bottom: 8px;
    }
    .step.active { border-color: rgba(15, 118, 110, 0.45); background: #edf8f6; }
    .step.done .step-number { background: var(--ok); }
    .step-number {
      display: grid;
      place-items: center;
      width: 28px;
      height: 28px;
      border-radius: 999px;
      color: #fff;
      background: var(--accent);
      font-size: 12px;
      font-weight: 700;
    }
    .step strong { display: block; margin-bottom: 2px; font-size: 13px; }
    .step p { margin: 0; color: var(--muted); font-size: 12px; line-height: 1.4; }
    .grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
    .grid3 { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
    .grid5 { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 12px; }
    .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 10px; margin-bottom: 12px; }
    .card { padding: 13px; min-width: 0; }
    .metric {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .metric-value { display: block; margin-bottom: 3px; font-size: 22px; font-weight: 800; color: var(--text); }
    .metric-label { color: var(--muted); font-size: 12px; }
    .choice-card {
      cursor: pointer;
      min-height: 140px;
    }
    .choice-card:hover { border-color: rgba(15, 118, 110, 0.5); background: #fbfffe; }
    .choice-card.active { border-color: var(--accent); background: #eef8f6; }
    .drop-card {
      border: 1px dashed #b9c4d3;
      background: #fbfdff;
    }
    .drop-card input { margin-top: 8px; }
    label {
      display: block;
      margin-bottom: 6px;
      color: var(--muted);
      font-size: 12px;
      font-weight: 700;
    }
    input, textarea {
      width: 100%;
      border: 1px solid var(--line);
      border-radius: 6px;
      padding: 8px 9px;
      background: #fff;
      color: var(--text);
      font-size: 13px;
    }
    input { height: 36px; }
    select {
      width: 100%;
      height: 36px;
      border: 1px solid var(--line);
      border-radius: 6px;
      padding: 7px 9px;
      background: #fff;
      color: var(--text);
      font-size: 13px;
    }
    input[type=checkbox] {
      width: auto;
      height: auto;
      margin-right: 6px;
    }
    input[type=file] { height: auto; padding: 7px; }
    textarea { min-height: 74px; resize: vertical; }
    .field { margin-bottom: 12px; }
    .button-row { display: flex; flex-wrap: wrap; gap: 8px; margin-top: 10px; }
    button {
      min-height: 36px;
      border: 1px solid var(--accent-dark);
      border-radius: 6px;
      padding: 0 12px;
      color: #fff;
      background: var(--accent);
      font-weight: 700;
      cursor: pointer;
    }
    button:hover { background: var(--accent-dark); }
    button.secondary {
      color: var(--text);
      background: #fff;
      border-color: var(--line);
    }
    button.secondary:hover { background: #edf3f8; }
    button:disabled { opacity: 0.55; cursor: not-allowed; }
    .target-button {
      color: var(--text);
      background: #fff;
      border-color: var(--line);
    }
    .target-button.active { color: #fff; background: var(--accent); border-color: var(--accent-dark); }
    .operation-banner {
      margin-bottom: 12px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .operation-banner.busy {
      border-color: #a7d8cf;
      background: #eef8f6;
    }
    .operation-banner.fail {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .operation-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 9px;
    }
    .operation-title {
      display: block;
      margin-bottom: 2px;
      font-size: 14px;
      font-weight: 800;
    }
    .progress-track {
      height: 9px;
      overflow: hidden;
      border-radius: 999px;
      background: #e4e9f0;
    }
    .progress-bar {
      height: 100%;
      width: 0%;
      border-radius: 999px;
      background: var(--accent);
      transition: width 0.25s ease;
    }
    .operation-banner.busy .progress-bar {
      width: 68%;
      animation: progressPulse 1.2s ease-in-out infinite;
    }
    .operation-banner.done .progress-bar { width: 100%; background: var(--ok); }
    .operation-banner.fail .progress-bar { width: 100%; background: var(--danger); }
    @keyframes progressPulse {
      0% { opacity: 0.55; }
      50% { opacity: 1; }
      100% { opacity: 0.55; }
    }
    .graph-canvas {
      min-height: 180px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .graph-row {
      display: flex;
      align-items: center;
      flex-wrap: wrap;
      gap: 8px;
      margin-bottom: 10px;
    }
    .graph-node {
      min-width: 84px;
      padding: 9px 10px;
      border: 1px solid #bdc8d7;
      border-radius: 8px;
      background: #fff;
      font-size: 13px;
      font-weight: 800;
      text-align: center;
    }
    .graph-node.source { border-color: #9fb7d6; background: #eef5ff; color: #24456f; }
    .graph-node.target { border-color: rgba(15, 118, 110, 0.45); background: #eef8f6; color: var(--accent-dark); }
    .graph-node.blocked { border-color: #e3b0aa; background: #fff2f0; color: var(--danger); }
    .graph-arrow { color: var(--muted); font-weight: 800; }
    .dependency-card {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
      margin-bottom: 10px;
    }
    .dependency-card.active {
      border-color: rgba(15, 118, 110, 0.35);
      background: #fbfffe;
    }
    .dependency-card.blocked {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .dependency-title {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      margin-bottom: 8px;
      font-weight: 800;
    }
    .dependency-steps {
      display: grid;
      gap: 7px;
      margin: 0;
      padding: 0;
      list-style: none;
    }
    .dependency-steps li {
      display: grid;
      grid-template-columns: 25px 1fr;
      gap: 8px;
      align-items: start;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.4;
    }
    .step-dot {
      display: grid;
      place-items: center;
      width: 22px;
      height: 22px;
      border-radius: 999px;
      color: #fff;
      background: var(--accent);
      font-size: 11px;
      font-weight: 800;
    }
    .dependency-note {
      margin-top: 9px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .dataset-board { display: grid; gap: 8px; }
    .dataset-card {
      padding: 11px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
      cursor: pointer;
    }
    .dataset-card:hover { border-color: rgba(15, 118, 110, 0.35); background: #fbfffe; }
    .dataset-card.active { border-color: rgba(15, 118, 110, 0.45); background: #fbfffe; }
    .dataset-card.blocked { border-color: #efc4be; background: #fff8f7; }
    .dataset-top {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 7px;
    }
    .dataset-name { font-size: 15px; font-weight: 800; }
    .stage-strip { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 5px; }
    .stage {
      min-height: 26px;
      display: grid;
      place-items: center;
      border-radius: 6px;
      border: 1px solid var(--line);
      color: var(--muted);
      background: #f7f9fb;
      font-size: 11px;
      font-weight: 700;
      text-align: center;
    }
    .stage.done { color: var(--ok); background: #e8f6ee; border-color: #b8dfc9; }
    .stage.active { color: var(--accent-dark); background: #e6f5f2; border-color: #a7d8cf; }
    .stage.blocked { color: var(--danger); background: #fde9e7; border-color: #e8b2ac; }
    .timeline {
      display: grid;
      gap: 8px;
      max-height: 260px;
      overflow: auto;
      padding-right: 4px;
    }
    .timeline-item {
      display: grid;
      grid-template-columns: 86px 1fr;
      gap: 10px;
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .timeline-time { color: var(--muted); font-size: 12px; }
    .timeline-title { margin-bottom: 2px; font-weight: 800; font-size: 13px; }
    .pill {
      display: inline-flex;
      align-items: center;
      border-radius: 999px;
      min-height: 23px;
      padding: 2px 8px;
      color: var(--ok);
      background: #e8f6ee;
      font-size: 12px;
      font-weight: 700;
      white-space: nowrap;
    }
    .pill.warn { color: var(--warn); background: #fff4df; }
    .pill.fail { color: var(--danger); background: #fde9e7; }
    .note {
      margin: 0 0 12px;
      padding: 10px 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.45;
    }
    .note.strong {
      color: var(--text);
      border-color: rgba(15, 118, 110, 0.24);
      background: #eef8f6;
    }
    .note.warn {
      border-color: #f0d19b;
      background: #fff8ea;
      color: #6d4200;
    }
    .file-list { display: grid; gap: 8px; }
    .file-item {
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
    }
    .file-title {
      display: flex;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 4px;
      font-size: 13px;
      font-weight: 700;
    }
    .file-meta {
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
      overflow-wrap: anywhere;
    }
    .mini-pre {
      margin-top: 7px;
      max-height: 120px;
      overflow: auto;
      padding: 8px;
      border-radius: 6px;
      color: #d8e2f0;
      background: var(--code);
      font-size: 11px;
      line-height: 1.4;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
    }
    .tabs { display: flex; flex-wrap: wrap; gap: 7px; margin: 0 0 12px; }
    .tab {
      min-height: 33px;
      padding: 0 10px;
      color: var(--text);
      background: #fff;
      border: 1px solid var(--line);
      border-radius: 6px;
    }
    .tab.active { color: #fff; background: var(--accent); border-color: var(--accent-dark); }
    table { width: 100%; border-collapse: collapse; font-size: 13px; }
    th, td {
      border-bottom: 1px solid var(--line);
      padding: 8px 6px;
      text-align: left;
      vertical-align: top;
      overflow-wrap: anywhere;
    }
    th { color: var(--muted); font-size: 12px; white-space: nowrap; }
    pre {
      margin: 0;
      min-height: 260px;
      max-height: 560px;
      overflow: auto;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      border-radius: 6px;
      padding: 12px;
      color: #d8e2f0;
      background: var(--code);
      font-size: 12px;
      line-height: 1.45;
    }
    .result-tabs { display: flex; flex-wrap: wrap; gap: 7px; margin: 12px 0; }
    .result-tab {
      min-height: 32px;
      padding: 0 10px;
      color: var(--text);
      background: #fff;
      border: 1px solid var(--line);
      border-radius: 6px;
    }
    .result-tab.active { color: #fff; background: var(--accent); border-color: var(--accent-dark); }
    .table-wrap { overflow: auto; border: 1px solid var(--line); border-radius: 8px; }
    .table-toolbar {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      margin: 10px 0;
    }
    .download-list { display: grid; gap: 8px; }
    .download-item {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
      padding: 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    ul.clean { margin: 0; padding-left: 18px; color: var(--muted); font-size: 13px; line-height: 1.5; }
    details { margin-top: 12px; }
    summary { cursor: pointer; font-weight: 700; font-size: 13px; }
    .hidden { display: none !important; }
    @media (max-width: 1120px) {
      main { grid-template-columns: 1fr; }
      aside { position: static; }
      .grid3, .grid5, .metric-grid { grid-template-columns: 1fr; }
    }
    @media (max-width: 760px) {
      header { align-items: flex-start; flex-direction: column; }
      .header-status { min-width: 0; width: 100%; max-width: none; }
      .grid2 { grid-template-columns: 1fr; }
    }
  </style>
</head>
<body>
  <header>
    <div>
      <h1>ADaM Agent Studio</h1>
      <div class="subtitle">Upload study evidence, generate reviewable R code, then run it locally after approval.</div>
    </div>
    <div class="header-status">
      <div class="status-card">
        <div class="status-row">
          <span class="status-label">System</span>
          <span class="pill warn" id="health">Checking API...</span>
        </div>
        <div class="status-detail" id="globalStatusDetail">Waiting for the local API health check.</div>
      </div>
    </div>
  </header>

  <main>
    <aside>
      <div class="step active" data-step="1"><div class="step-number">1</div><div><strong>Start</strong><p>Use the demo or upload your own files.</p></div></div>
      <div class="step" data-step="2"><div class="step-number">2</div><div><strong>Inputs</strong><p>Check recognized SDTM, specs, and references.</p></div></div>
      <div class="step" data-step="3"><div class="step-number">3</div><div><strong>Target</strong><p>Choose which ADaM dataset to generate.</p></div></div>
      <div class="step" data-step="4"><div class="step-number">4</div><div><strong>Code</strong><p>Generate R without running it.</p></div></div>
      <div class="step" data-step="5"><div class="step-number">5</div><div><strong>Review & Run</strong><p>Approve code, then execute in R sandbox.</p></div></div>
      <div class="step" data-step="6"><div class="step-number">6</div><div><strong>Results</strong><p>Inspect generated ADaM and warnings.</p></div></div>
    </aside>

    <div>
      <section>
        <div class="section-head">
          <h2>Study Dashboard</h2>
          <span class="pill warn" id="graphStatus">waiting</span>
        </div>
        <div class="section-body">
          <div class="operation-banner" id="operationBanner">
            <div class="operation-head">
              <div>
                <span class="operation-title" id="operationTitle">Ready for study setup</span>
                <div class="muted" id="operationDetail">No long-running operation is active.</div>
              </div>
              <span class="pill" id="operationStatus">idle</span>
            </div>
            <div class="progress-track"><div class="progress-bar" id="operationProgress"></div></div>
          </div>
          <div class="metric-grid">
            <div class="metric"><span class="metric-value" id="metricInputs">0</span><span class="metric-label">input files</span></div>
            <div class="metric"><span class="metric-value" id="metricTargets">0</span><span class="metric-label">ADaM targets</span></div>
            <div class="metric"><span class="metric-value" id="metricRunnable">0</span><span class="metric-label">runnable now</span></div>
            <div class="metric"><span class="metric-value" id="metricBlocked">0</span><span class="metric-label">blocked</span></div>
          </div>
          <div class="grid2">
            <div>
              <h3>Dependency Map</h3>
              <div id="dependencyGraph" class="graph-canvas"><div class="muted">Load inputs to build the study graph.</div></div>
            </div>
            <div>
              <h3>Dataset Execution Cards</h3>
              <div id="datasetBoard" class="dataset-board"><div class="muted">No dataset selected yet.</div></div>
            </div>
          </div>
        </div>
      </section>

      <section>
        <div class="section-head">
          <h2>Start A Study</h2>
          <span class="pill warn" id="workspaceStatus">not started</span>
        </div>
        <div class="section-body">
          <div class="grid2">
            <div class="card choice-card" id="demoChoice">
              <h3>Try With Shiny Demo Data</h3>
              <p class="muted">Loads AE, DM, EX as SDTM; ads_adsl_full and ads_adae_full as specs; ADSL and ADAE as reference outputs.</p>
              <div class="button-row"><button id="createDemoButton">Load Demo</button></div>
            </div>
            <div class="card choice-card" id="uploadChoice">
              <h3>Use My Study Files</h3>
              <p class="muted">Creates a local working folder automatically. You only choose file roles; technical paths stay hidden.</p>
              <div class="button-row"><button class="secondary" id="startUploadButton">Start Upload</button></div>
            </div>
          </div>
          <p class="note" id="workspaceMessage">Choose one path to begin. The app will organize files into the backend structure automatically.</p>

          <div id="uploadPanel" class="hidden">
            <div class="grid5">
              <div class="card drop-card">
                <h3>SDTM Source Data</h3>
                <p class="muted">Examples: dm.csv, ae.csv, ex.csv.</p>
                <input id="uploadSdtm" type="file" multiple>
                <button data-upload-role="sdtm">Upload SDTM</button>
                <div class="file-meta" id="uploadStatusSdtm"></div>
              </div>
              <div class="card drop-card">
                <h3>ADaM Specs</h3>
                <p class="muted">Examples: ads_adsl_full.csv, ads_adae_full.csv.</p>
                <input id="uploadSpec" type="file" multiple>
                <button data-upload-role="spec">Upload Specs</button>
                <div class="file-meta" id="uploadStatusSpec"></div>
              </div>
              <div class="card drop-card">
                <h3>Reference ADaM</h3>
            <p class="muted">Existing ADaM for compare/output-shape evidence only.</p>
                <input id="uploadReference" type="file" multiple>
                <button data-upload-role="reference">Upload Reference ADaM</button>
                <div class="file-meta" id="uploadStatusReference"></div>
              </div>
              <div class="card drop-card">
                <h3>Define</h3>
                <p class="muted">define.xml or related metadata evidence.</p>
                <input id="uploadDefine" type="file" multiple>
                <button data-upload-role="define">Upload Define</button>
                <div class="file-meta" id="uploadStatusDefine"></div>
              </div>
              <div class="card drop-card">
                <h3>Legacy Code</h3>
                <p class="muted">SAS/R programs used as lineage evidence.</p>
                <input id="uploadLegacy" type="file" multiple>
                <button data-upload-role="legacy">Upload Legacy Code</button>
                <div class="file-meta" id="uploadStatusLegacy"></div>
              </div>
            </div>
            <p class="note warn" style="margin-top:12px;">Reference ADaM is used for compare/output-shape evidence only. It is not derivation authority and does not override user specs.</p>
          </div>
        </div>
      </section>

      <section>
        <div class="section-head">
          <h2>Recognized Inputs</h2>
          <span class="muted" id="inputSummaryLine">Nothing scanned yet.</span>
        </div>
        <div class="section-body">
          <div class="grid5">
            <div class="card"><h3>SDTM</h3><div id="sdtmFiles" class="file-list"><div class="muted">No files yet.</div></div></div>
            <div class="card"><h3>Specs</h3><div id="specFiles" class="file-list"><div class="muted">No files yet.</div></div></div>
            <div class="card"><h3>Reference ADaM</h3><div id="referenceFiles" class="file-list"><div class="muted">No files yet.</div></div></div>
            <div class="card"><h3>Define</h3><div id="defineFiles" class="file-list"><div class="muted">No files yet.</div></div></div>
            <div class="card"><h3>Legacy Code</h3><div id="legacyFiles" class="file-list"><div class="muted">No files yet.</div></div></div>
          </div>
          <div id="inputWarnings" class="note" style="margin-top: 12px;">No warnings yet.</div>
        </div>
      </section>

      <section>
        <div class="section-head">
          <h2>Choose Output</h2>
          <span id="planStatus" class="pill warn">waiting</span>
        </div>
        <div class="section-body">
          <p class="note">Pick the ADaM dataset you want to generate. The app only adds upstream ADaM dependencies when your spec, define, or legacy code provides evidence.</p>
          <div class="button-row" id="targetButtons"></div>
          <div class="grid2" style="margin-top:12px;">
            <div class="field">
              <label for="manualTarget">Add another ADaM target</label>
              <input id="manualTarget" placeholder="Example: ADLB, ADCM, ADSL">
            </div>
            <div class="field">
              <label>&nbsp;</label>
              <button class="secondary" id="addTargetButton">Add Target</button>
            </div>
          </div>
          <div id="planView" class="note">Load inputs first, then choose a target.</div>
          <div class="button-row">
            <button class="secondary" id="finalizeInputsButton" disabled>Finalize Inputs / Draft Spec</button>
            <button class="secondary" id="approveDraftSpecButton" disabled>Approve Draft Spec</button>
          </div>
          <div id="draftSpecPane" class="note">Finalize inputs after upload. If no approved spec is present, the app will generate a draft spec for review.</div>
        </div>
      </section>

      <section>
        <div class="section-head">
          <h2>Generate, Review, Run</h2>
          <span id="codeStatus" class="pill warn">not generated</span>
        </div>
        <div class="section-body">
          <div class="button-row">
            <button id="generateCodeButton" disabled>Generate R Code</button>
            <button id="approveButton" disabled>Approve And Run Locally</button>
          </div>
          <p class="note">Generation creates R code only. Running happens after approval, using the local R sandbox.</p>
          <div class="tabs">
            <button class="tab active" data-view="summary">Summary</button>
            <button class="tab" data-view="code">R Code</button>
            <button class="tab" data-view="risk">Assumptions & Risks</button>
            <button class="tab" data-view="output">Generated ADaM</button>
            <button class="tab" data-view="timeline">Audit Timeline</button>
          </div>
          <div id="reviewPane"><p class="note">No code generated yet.</p></div>
          <details>
            <summary>Advanced settings and audit files</summary>
            <div class="grid3" style="margin-top:12px;">
              <div class="field">
                <label for="studyDir">Study folder</label>
                <input id="studyDir">
              </div>
              <div class="field">
                <label for="runId">Run id</label>
                <input id="runId">
              </div>
              <div class="field">
                <label for="configPath">LLM config</label>
                <input id="configPath" value="studies\\_template\\configs\\mock_downstream.json">
              </div>
              <div class="field">
                <label for="modelMode">Model mode</label>
                <select id="modelMode">
                  <option value="mock">Mock / offline</option>
                  <option value="real">Real LLM API</option>
                </select>
              </div>
              <div class="field">
                <label for="llmProvider">Provider</label>
                <select id="llmProvider">
                  <option value="openai-compatible">OpenAI-compatible</option>
                  <option value="openai">OpenAI</option>
                  <option value="anthropic">Anthropic / Claude</option>
                  <option value="deepseek">DeepSeek</option>
                  <option value="qwen">Qwen</option>
                </select>
              </div>
              <div class="field">
                <label for="llmModel">Model</label>
                <input id="llmModel" value="gpt-5.5">
              </div>
              <div class="field">
                <label for="llmBaseUrl">Base URL</label>
                <input id="llmBaseUrl" placeholder="Optional, for a relay or compatible endpoint">
              </div>
              <div class="field">
                <label for="llmApiKey">API key</label>
                <input id="llmApiKey" type="password" placeholder="Used for this browser request only">
              </div>
              <div class="field">
                <label>&nbsp;</label>
                <label><input id="llmAllowExternal" type="checkbox"> allow external API for demo data</label>
                <button class="secondary" id="testLlmButton" type="button">Test Connection</button>
              </div>
              <div class="field">
                <label for="rscriptPath">Rscript path</label>
                <input id="rscriptPath" value="C:\\Dev\\R-4.5.2\\bin\\Rscript.exe">
              </div>
              <div class="field">
                <label for="reviewer">Reviewer</label>
                <input id="reviewer" value="local_user">
              </div>
              <div class="field">
                <label for="reviewNotes">Review notes</label>
                <input id="reviewNotes" value="Approved for local sandbox execution.">
              </div>
            </div>
            <div id="llmStatus" class="note">Mock mode is active. No external LLM call will be made unless Real LLM API is selected.</div>
            <div id="advancedPane" class="note">Audit artifacts appear after a run.</div>
          </details>
        </div>
      </section>
    </div>
  </main>

  <script>
    const state = {
      studyId: null,
      inputSummary: null,
      plan: null,
      graphState: null,
      generated: null,
      review: null,
      execution: null,
      generatedByDataset: {},
      reviewByDataset: {},
      executionByDataset: {},
      draftSpecByDataset: {},
      draftSpecReviewByDataset: {},
      finalizedInputsByDataset: {},
      runReview: null,
      selectedTarget: null,
      targetCandidates: [],
      events: [],
      selectedView: 'summary',
      selectedResultView: 'generated',
      tablePages: {},
      compareResults: {}
    };
    const uploadInputs = {
      sdtm: 'uploadSdtm',
      spec: 'uploadSpec',
      reference: 'uploadReference',
      define: 'uploadDefine',
      legacy: 'uploadLegacy'
    };
    const uploadStatus = {
      sdtm: 'uploadStatusSdtm',
      spec: 'uploadStatusSpec',
      reference: 'uploadStatusReference',
      define: 'uploadStatusDefine',
      legacy: 'uploadStatusLegacy'
    };
    const byId = (id) => document.getElementById(id);

    function defaultRunId() {
      return `run_${new Date().toISOString().replace(/[-:T.Z]/g, '').slice(0, 14)}`;
    }
    byId('runId').value = defaultRunId();

    async function api(path, options = {}) {
      const response = await fetch(path, options);
      const payload = await response.json().catch(() => ({}));
      if (!response.ok) {
        throw new Error(payload.detail || `Request failed: ${response.status}`);
      }
      return payload;
    }

    function escapeHtml(value) {
      return String(value ?? '').replace(/[&<>"']/g, (char) => ({
        '&': '&amp;',
        '<': '&lt;',
        '>': '&gt;',
        '"': '&quot;',
        "'": '&#39;'
      }[char]));
    }

    function setPill(id, status) {
      const node = byId(id);
      node.textContent = status;
      node.className = 'pill';
      if (['failed', 'blocked', 'error', 'not started', 'unavailable'].includes(status)) node.classList.add('fail');
      if (['waiting', 'not generated', 'running', 'review', 'warning', 'stale', 'draft review', 'checking'].includes(status)) node.classList.add('warn');
    }

    function setOperation(status, title, detail) {
      const banner = byId('operationBanner');
      const statusNode = byId('operationStatus');
      byId('operationTitle').textContent = title;
      byId('operationDetail').textContent = detail;
      statusNode.textContent = status;
      statusNode.className = 'pill';
      banner.className = 'operation-banner';
      if (status === 'running') {
        banner.classList.add('busy');
        statusNode.classList.add('warn');
      } else if (status === 'failed') {
        banner.classList.add('fail');
        statusNode.classList.add('fail');
      } else if (status === 'done') {
        banner.classList.add('done');
      } else if (status === 'waiting') {
        statusNode.classList.add('warn');
      }
      byId('globalStatusDetail').textContent = detail;
    }

    function beginOperation(title, detail) {
      setOperation('running', title, detail);
    }

    function completeOperation(title, detail) {
      setOperation('done', title, detail);
    }

    function failOperation(title, error) {
      setOperation('failed', title, String(error));
    }

    function setStep(index) {
      for (const node of document.querySelectorAll('.step')) {
        const step = Number(node.dataset.step);
        node.classList.toggle('active', step === index);
        node.classList.toggle('done', step < index);
      }
    }

    function addEvent(title, detail) {
      state.events.unshift({
        time: new Date().toLocaleTimeString([], {hour: '2-digit', minute: '2-digit', second: '2-digit'}),
        title,
        detail
      });
      state.events = state.events.slice(0, 30);
      renderTimeline();
    }

    function studyDir() {
      return byId('studyDir').value.trim();
    }

    function runId() {
      const value = byId('runId').value.trim();
      if (value) return value;
      const next = defaultRunId();
      byId('runId').value = next;
      return next;
    }

    function selectedTargets() {
      return state.selectedTarget ? [state.selectedTarget] : [];
    }

    function llmProviderOverride() {
      if (byId('modelMode').value !== 'real') return null;
      const provider = byId('llmProvider').value;
      const model = byId('llmModel').value.trim();
      const baseUrl = byId('llmBaseUrl').value.trim();
      const apiKey = byId('llmApiKey').value.trim();
      const approvedBy = byId('reviewer').value.trim() || 'local_user';
      if (!model) {
        throw new Error('Model is required for real LLM mode.');
      }
      if (!apiKey) {
        throw new Error('API key is required for real LLM mode.');
      }
      return {
        provider,
        model,
        base_url: baseUrl || null,
        api_key: apiKey,
        timeout_seconds: 300,
        max_tokens: 4096,
        allow_custom_base_url: Boolean(baseUrl),
        custom_base_url_approved_by: baseUrl ? approvedBy : null
      };
    }

    function llmExposureOverride() {
      if (byId('modelMode').value !== 'real') return null;
      const approvedBy = byId('reviewer').value.trim() || 'local_user';
      return {
        mode: 'demo_rich_context',
        data_classification: 'processed_demo',
        external_api_allowed: byId('llmAllowExternal').checked,
        approved_by: approvedBy,
        approval_note: 'Approved in local UI for processed demo data.',
        sample_rows_per_dataset: 3,
        include_reference_rows: true
      };
    }

    function llmOverridePayload() {
      if (byId('modelMode').value !== 'real') return {};
      if (!byId('llmAllowExternal').checked) {
        throw new Error('Real LLM mode requires explicit external API approval for demo data.');
      }
      return {
        llm_provider_override: llmProviderOverride(),
        llm_exposure_override: llmExposureOverride()
      };
    }

    function updateLlmModeControls() {
      const realMode = byId('modelMode').value === 'real';
      for (const id of ['llmProvider', 'llmModel', 'llmBaseUrl', 'llmApiKey', 'llmAllowExternal', 'testLlmButton']) {
        byId(id).disabled = !realMode;
      }
      byId('llmStatus').className = realMode ? 'note warn' : 'note';
      byId('llmStatus').textContent = realMode
        ? 'Real LLM mode is selected. Test the connection before generating code; the API key is used only for this browser request.'
        : 'Mock mode is active. No external LLM call will be made unless Real LLM API is selected.';
    }

    async function testLlmConnection() {
      if (byId('modelMode').value !== 'real') {
        byId('llmStatus').className = 'note';
        byId('llmStatus').textContent = 'Mock mode is active. There is no external connection to test.';
        return;
      }
      beginOperation('Testing LLM connection', 'Sending a short provider health request. No study data is included.');
      byId('llmStatus').className = 'note warn';
      byId('llmStatus').textContent = 'Testing provider connection...';
      try {
        if (!byId('llmAllowExternal').checked) {
          throw new Error('Check external API approval before testing the provider.');
        }
        const payload = await api('/llm/test-connection', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            llm_provider: llmProviderOverride(),
            llm_exposure: llmExposureOverride()
          })
        });
        const relayNote = payload.external_relay ? ' Custom or relay endpoint approved.' : '';
        byId('llmStatus').className = 'note strong';
        byId('llmStatus').textContent = `Connection OK: ${payload.provider} / ${payload.model}.${relayNote} No study data was sent.`;
        completeOperation('LLM connection OK', `${payload.provider} responded. No study data was sent.`);
        addEvent('LLM connection tested', `${payload.provider} responded successfully.`);
      } catch (error) {
        byId('llmStatus').className = 'note warn';
        byId('llmStatus').textContent = String(error);
        failOperation('LLM connection failed', error);
      }
    }

    async function checkHealth() {
      try {
        const payload = await api('/health');
        setPill('health', payload.status === 'ok' ? 'API ready' : 'unavailable');
        byId('globalStatusDetail').textContent = payload.status === 'ok'
          ? 'Local API is running. Choose demo data or upload study files.'
          : 'Local API responded but is not ready.';
      } catch {
        setPill('health', 'unavailable');
        failOperation('API unavailable', 'The browser cannot reach the local FastAPI service.');
      }
    }

    async function startUploadWorkspace() {
      beginOperation('Creating workspace', 'Preparing the local study folders for uploaded files.');
      byId('uploadPanel').classList.remove('hidden');
      byId('uploadChoice').classList.add('active');
      byId('demoChoice').classList.remove('active');
      byId('workspaceMessage').textContent = 'Creating a local workspace...';
      try {
        const payload = await api('/product-workspace', {method: 'POST'});
        applyWorkspacePayload(payload);
        state.inputSummary = payload.input_summary;
        renderInputSummary(payload.input_summary);
        addEvent('Workspace created', 'A local study workspace is ready for uploads.');
        setPill('workspaceStatus', 'ready');
        byId('workspaceMessage').textContent = 'Workspace ready. Upload SDTM and spec files by role.';
        completeOperation('Workspace ready', 'Upload SDTM, specs, reference ADaM, define, or legacy code by role.');
        setStep(2);
      } catch (error) {
        setPill('workspaceStatus', 'error');
        byId('workspaceMessage').textContent = String(error);
        failOperation('Workspace creation failed', error);
      }
    }

    async function createDemoStudy() {
      beginOperation('Loading demo study', 'Copying demo SDTM, spec, and reference ADaM files into a local workspace.');
      byId('demoChoice').classList.add('active');
      byId('uploadChoice').classList.remove('active');
      byId('uploadPanel').classList.add('hidden');
      byId('workspaceMessage').textContent = 'Loading demo data...';
      try {
        const payload = await api('/demo-study', {method: 'POST'});
        applyWorkspacePayload(payload);
        await scanInputs();
        autoSelectFirstTarget(inferTargets(state.inputSummary));
        addEvent('Demo loaded', 'Shiny demo inputs were copied into the study workspace.');
        setPill('workspaceStatus', 'ready');
        byId('workspaceMessage').textContent = 'Demo loaded. Files are already organized by role.';
        completeOperation('Demo study ready', 'Demo inputs are organized. Choose a target and review the dependency plan.');
        setStep(3);
      } catch (error) {
        setPill('workspaceStatus', 'error');
        byId('workspaceMessage').textContent = String(error);
        failOperation('Demo load failed', error);
      }
    }

    function applyWorkspacePayload(payload) {
      state.studyId = payload.study_id || null;
      byId('studyDir').value = payload.study_dir || '';
      byId('runId').value = payload.run_id || defaultRunId();
      byId('configPath').value = payload.config_path || byId('configPath').value;
      if (payload.rscript_path) byId('rscriptPath').value = payload.rscript_path;
      state.selectedTarget = payload.target_datasets?.length ? payload.target_datasets[0] : null;
      resetRunState();
    }

    async function uploadRole(role) {
      if (!studyDir()) {
        await startUploadWorkspace();
      }
      const input = byId(uploadInputs[role]);
      if (!input.files.length) {
        byId('workspaceMessage').textContent = `Choose at least one ${role} file first.`;
        return;
      }
      beginOperation(`Uploading ${role}`, 'Saving files, rescanning study inputs, and invalidating stale plans if needed.');
      byId('workspaceMessage').textContent = `Uploading ${role} file(s)...`;
      const form = new FormData();
      for (const file of input.files) form.append('files', file);
      try {
        byId(uploadStatus[role]).textContent = 'Uploading...';
        const payload = await api(`/studies/files?study_dir=${encodeURIComponent(studyDir())}&role=${encodeURIComponent(role)}${state.studyId ? `&study_id=${encodeURIComponent(state.studyId)}` : ''}`, {
          method: 'POST',
          body: form
        });
        state.inputSummary = payload.input_summary;
        invalidateUiStateAfterInputChange(payload);
        renderInputSummary(payload.input_summary);
        addEvent(`${role} uploaded`, uploadDiffMessage(payload));
        byId(uploadStatus[role]).textContent = `${payload.saved_files.length} file(s) uploaded.`;
        byId('workspaceMessage').textContent = `Uploaded ${payload.saved_files.length} file(s). ${uploadDiffMessage(payload)}`;
        completeOperation(`${role} upload complete`, uploadDiffMessage(payload));
        setStep(2);
      } catch (error) {
        byId(uploadStatus[role]).textContent = 'Upload failed.';
        byId('workspaceMessage').textContent = String(error);
        failOperation(`${role} upload failed`, error);
      }
    }

    function invalidateUiStateAfterInputChange(payload) {
      if (!payload?.input_diff?.changed) return;
      state.plan = null;
      state.graphState = null;
      state.generated = null;
      state.review = null;
      state.execution = null;
      state.generatedByDataset = {};
      state.reviewByDataset = {};
      state.executionByDataset = {};
      state.draftSpecByDataset = {};
      state.draftSpecReviewByDataset = {};
      state.finalizedInputsByDataset = {};
      state.runReview = null;
      state.tablePages = {};
      state.compareResults = {};
      setPill('planStatus', 'stale');
      setPill('codeStatus', 'stale');
      byId('planView').innerHTML = '<p class="note warn">Inputs changed. Dependency plan, draft specs, generated code, and reviews must be refreshed before generation continues.</p>';
      byId('draftSpecPane').innerHTML = '<p class="note warn">Inputs changed. Finalize inputs again before approving or generating code.</p>';
      byId('reviewPane').innerHTML = '<p class="note warn">Inputs changed. Previous generated-code state was cleared from the UI.</p>';
    }

    function uploadDiffMessage(payload) {
      const diff = payload?.input_diff || {};
      const parts = [];
      if (diff.added?.length) parts.push(`${diff.added.length} added`);
      if (diff.changed_files?.length) parts.push(`${diff.changed_files.length} changed`);
      if (diff.removed?.length) parts.push(`${diff.removed.length} removed`);
      const touched = payload?.touched_runs?.length ? ` ${payload.touched_runs.length} existing run(s) marked stale.` : '';
      return `${payload.saved_files?.length || 0} file(s) added and inputs rescanned.${parts.length ? ` Input diff: ${parts.join(', ')}.` : ''}${touched}`;
    }

    async function scanInputs() {
      const payload = await api(`/study-inputs?study_dir=${encodeURIComponent(studyDir())}`);
      state.inputSummary = payload;
      renderInputSummary(payload);
      addEvent('Inputs scanned', 'The app refreshed study evidence and target candidates.');
      return payload;
    }

    function renderInputSummary(summary) {
      renderFiles('sdtmFiles', summary?.sdtm || []);
      renderFiles('specFiles', summary?.specs || []);
      renderFiles('referenceFiles', summary?.reference_adam || []);
      renderFiles('defineFiles', summary?.define || []);
      renderFiles('legacyFiles', summary?.legacy_code || []);
      const sdtm = summary?.sdtm?.length || 0;
      const specs = summary?.specs?.length || 0;
      const refs = summary?.reference_adam?.length || 0;
      const define = summary?.define?.length || 0;
      const legacy = summary?.legacy_code?.length || 0;
      byId('inputSummaryLine').textContent = `${sdtm} SDTM, ${specs} spec, ${refs} reference, ${define} define, ${legacy} legacy file(s) recognized.`;
      const warnings = [...(summary?.warnings || []), ...((summary?.invalid_files || []).map((item) => `${item.path}: ${item.reason}`))];
      byId('inputWarnings').textContent = warnings.length ? warnings.join(' ') : 'No input warnings.';
      renderTargetButtons(inferTargets(summary));
      renderGraphAwareDashboard();
      if (runId()) {
        loadReviewSummary(runId());
      }
    }

    function renderFiles(containerId, files) {
      const node = byId(containerId);
      if (!files || !files.length) {
        node.innerHTML = '<div class="muted">No files found.</div>';
        return;
      }
      node.innerHTML = files.map((file) => `
        <div class="file-item">
          <div class="file-title">
            <span>${escapeHtml(file.dataset || file.file_name)}</span>
            <span class="pill ${file.status === 'ok' ? '' : 'warn'}">${escapeHtml(file.status)}</span>
          </div>
          <div class="file-meta">${escapeHtml(file.file_name)} | ${escapeHtml(file.format)} | ${file.row_count ?? file.line_count ?? '-'} ${file.preview_type === 'code' || file.preview_type === 'text' ? 'lines' : 'rows'}</div>
          <div class="file-meta">${escapeHtml(fileSummary(file))}</div>
          ${file.text_preview ? `<div class="mini-pre">${escapeHtml(file.text_preview)}</div>` : ''}
        </div>
      `).join('');
    }

    function fileSummary(file) {
      if (file.preview_type === 'code' || file.preview_type === 'text') {
        const targets = (file.detected_targets || []).join(', ');
        const deps = (file.detected_dependencies || []).join(', ');
        return [targets ? `ADaM tokens: ${targets}` : '', deps ? `Dependency hints: ${deps}` : '', file.note || ''].filter(Boolean).join(' | ');
      }
      const columns = (file.columns || []).slice(0, 10).join(', ');
      return columns || file.note || 'No preview details.';
    }

    function inferTargets(summary) {
      const candidates = new Set();
      for (const spec of summary?.specs || []) {
        if (spec.dataset && spec.dataset.startsWith('AD')) candidates.add(spec.dataset);
        for (const token of inferAdTokens(`${spec.file_name} ${spec.dataset || ''} ${(spec.columns || []).join(' ')}`)) candidates.add(token);
      }
      for (const ref of summary?.reference_adam || []) {
        if (ref.dataset && ref.dataset.startsWith('AD')) candidates.add(ref.dataset);
      }
      for (const legacy of summary?.legacy_code || []) {
        for (const token of inferAdTokens(`${legacy.file_name} ${legacy.dataset || ''}`)) candidates.add(token);
      }
      const merged = new Set([...(state.targetCandidates || []), ...candidates]);
      if (!merged.size) merged.add('ADAE');
      state.targetCandidates = Array.from(merged).sort();
      return state.targetCandidates;
    }

    function inferAdTokens(text) {
      const normalized = String(text || '').toUpperCase();
      const tokens = new Set();
      for (const match of normalized.matchAll(/\bAD[A-Z0-9]{1,6}\b/g)) {
        tokens.add(match[0]);
      }
      for (const match of normalized.matchAll(/ADS[_-]?(AD[A-Z0-9]{1,6})/g)) {
        tokens.add(match[1]);
      }
      return Array.from(tokens);
    }

    function autoSelectFirstTarget(targets) {
      const available = targets.length ? targets : inferTargets(state.inputSummary);
      state.selectedTarget = available.includes('ADAE') ? 'ADAE' : available[0];
      renderTargetButtons(available);
      if (state.selectedTarget) preparePlan();
    }

    function renderTargetButtons(targets) {
      const node = byId('targetButtons');
      if (!targets.length) {
        node.innerHTML = '<span class="muted">No ADaM targets inferred yet.</span>';
        return;
      }
      if (!state.selectedTarget || !targets.includes(state.selectedTarget)) {
        state.selectedTarget = targets.includes('ADAE') ? 'ADAE' : targets[0];
      }
      node.innerHTML = targets.map((target) => `
        <button class="target-button ${target === state.selectedTarget ? 'active' : ''}" data-target="${escapeHtml(target)}">${escapeHtml(target)}</button>
      `).join('');
      for (const button of node.querySelectorAll('[data-target]')) {
        button.addEventListener('click', () => {
          state.selectedTarget = button.dataset.target;
          renderTargetButtons(targets);
          resetActiveDatasetView();
          preparePlan();
        });
      }
      byId('generateCodeButton').disabled = !state.selectedTarget;
      byId('finalizeInputsButton').disabled = !state.selectedTarget;
      renderDraftSpecPane();
      renderGraphAwareDashboard();
    }

    function addManualTarget() {
      const value = byId('manualTarget').value.trim().toUpperCase();
      if (!value) return;
      if (!/^AD[A-Z0-9]{1,6}$/.test(value)) {
        byId('planView').innerHTML = '<p class="note warn">Target should look like an ADaM dataset name, for example ADSL, ADAE, ADLB, or ADCM.</p>';
        return;
      }
      const next = new Set(state.targetCandidates || []);
      next.add(value);
      state.targetCandidates = Array.from(next).sort();
      state.selectedTarget = value;
      byId('manualTarget').value = '';
      resetActiveDatasetView();
      renderTargetButtons(state.targetCandidates);
      addEvent('Target added', `${value} was added manually for planning.`);
      preparePlan();
    }

    function resetRunState() {
      state.plan = null;
      state.graphState = null;
      state.generated = null;
      state.review = null;
      state.execution = null;
      state.generatedByDataset = {};
      state.reviewByDataset = {};
      state.executionByDataset = {};
      state.draftSpecByDataset = {};
      state.draftSpecReviewByDataset = {};
      state.runReview = null;
      state.targetCandidates = state.selectedTarget ? [state.selectedTarget] : [];
      state.tablePages = {};
      state.compareResults = {};
      resetActiveDatasetView();
    }

    function resetActiveDatasetView() {
      syncActiveDatasetState();
      state.selectedResultView = 'generated';
      setPill('codeStatus', codeStatusForActiveDataset());
      byId('approveButton').disabled = !canApproveGeneratedCode(state.selectedTarget);
      byId('finalizeInputsButton').disabled = !state.selectedTarget;
      renderDraftSpecPane();
      renderPane();
      renderGraphAwareDashboard();
    }

    function syncActiveDatasetState() {
      state.generated = generatedFor(state.selectedTarget);
      state.review = reviewFor(state.selectedTarget);
      state.execution = executionFor(state.selectedTarget);
    }

    async function refreshGraphState() {
      if (!studyDir() || !runId()) return null;
      try {
        const graph = await api(`/runs/${encodeURIComponent(runId())}/graph-state?study_dir=${encodeURIComponent(studyDir())}`);
        state.graphState = graph;
        applyGraphState(graph);
        return graph;
      } catch {
        return null;
      }
    }

    function applyGraphState(graph) {
      const graphTargets = graph?.target_datasets || [];
      if (graphTargets.length) {
        state.targetCandidates = Array.from(new Set([...(state.targetCandidates || []), ...graphTargets])).sort();
      }
      for (const [dataset, datasetState] of Object.entries(graph?.datasets || {})) {
        const target = dataset.toUpperCase();
        const spec = datasetState.spec_state || {};
        if (spec.status === 'draft_generated' && spec.draft_spec_path) {
          state.draftSpecByDataset[target] = {
            dataset: target,
            status: 'draft',
            spec_path: spec.draft_spec_path,
            variables: spec.variables || [],
            warnings: spec.warnings || []
          };
        }
        if (spec.status === 'approved') {
          state.draftSpecReviewByDataset[target] = {
            dataset: target,
            approved: true,
            approved_spec_path: spec.approved_spec_path
          };
          state.finalizedInputsByDataset[target] = {
            ...(state.finalizedInputsByDataset[target] || {}),
            dataset: target,
            status: 'approved_draft_spec_ready',
            approved_draft_spec_available: true,
            approved_spec_path: spec.approved_spec_path
          };
        }
        if (spec.status === 'input_spec_ready') {
          state.finalizedInputsByDataset[target] = {
            ...(state.finalizedInputsByDataset[target] || {}),
            dataset: target,
            status: 'input_spec_ready',
            input_spec_available: true,
            input_spec_path: spec.input_spec_path
          };
        }
        const code = datasetState.code_state || {};
        if (code.code_path && code.status) {
          const existingGenerated = state.generatedByDataset[target] || {};
          state.generatedByDataset[target] = {
            ...existingGenerated,
            study_id: graph.study_id,
            run_id: graph.run_id,
            dataset: target,
            status: code.status,
            code_path: code.code_path,
            static_check_path: code.static_check_path || null,
            draft_spec_path: code.spec_source === 'approved_draft_spec' ? code.spec_path : null,
            generated_code: existingGenerated.generated_code || '',
            assumptions: existingGenerated.assumptions || [],
            risk_points: existingGenerated.risk_points || [],
            used_inputs: existingGenerated.used_inputs || [],
            expected_outputs: existingGenerated.expected_outputs || []
          };
        }
        if (code.status === 'approved') {
          state.reviewByDataset[target] = {
            dataset: target,
            decision: code.decision || 'approve',
            approved: true,
            review_path: code.review_path || null
          };
        }
        const execution = datasetState.execution_state || {};
        if (execution.status) {
          state.executionByDataset[target] = {
            dataset: target,
            status: execution.status,
            validation_status: execution.validation_status || datasetState.validation_summary?.status || null,
            output_path: execution.output_path || null,
            validation_report_path: execution.validation_report_path || null,
            diagnostics_path: execution.diagnostics_path || null,
            terminal_failure: Boolean(execution.terminal_failure)
          };
        }
        if (datasetState.compare_summary?.status) {
          state.compareResults[target] = datasetState.compare_summary;
        }
      }
      syncActiveDatasetState();
    }

    function generatedFor(dataset) {
      return dataset ? state.generatedByDataset[dataset] || null : null;
    }

    function canApproveGeneratedCode(dataset) {
      const generated = generatedFor(dataset);
      return Boolean(generated && generated.status !== 'stale' && generated.generated_code);
    }

    function reviewFor(dataset) {
      return dataset ? state.reviewByDataset[dataset] || null : null;
    }

    function executionFor(dataset) {
      return dataset ? state.executionByDataset[dataset] || null : null;
    }

    function draftSpecFor(dataset) {
      return dataset ? state.draftSpecByDataset[dataset] || null : null;
    }

    function draftSpecReviewFor(dataset) {
      return dataset ? state.draftSpecReviewByDataset[dataset] || null : null;
    }

    function finalizedInputsFor(dataset) {
      return dataset ? state.finalizedInputsByDataset[dataset] || null : null;
    }

    function targetHasInputSpec(target) {
      const specs = state.inputSummary?.specs || [];
      const normalized = String(target || '').toUpperCase();
      return specs.some((item) => String(item.dataset || '').toUpperCase() === normalized || String(item.file_name || '').toUpperCase().includes(normalized));
    }

    function targetSpecGateSatisfied(target) {
      const finalized = finalizedInputsFor(target);
      return Boolean(
        finalized?.input_spec_available ||
        finalized?.approved_draft_spec_available ||
        targetHasInputSpec(target) ||
        draftSpecReviewFor(target)?.approved
      );
    }

    function datasetReviewFor(dataset) {
      return (state.runReview?.dataset_reviews || []).find((item) => item.dataset === dataset) || null;
    }

    function codeStatusForActiveDataset() {
      if (!state.selectedTarget) return 'not generated';
      const execution = executionFor(state.selectedTarget);
      if (execution) return execution.status;
      const generated = generatedFor(state.selectedTarget);
      if (generated?.status === 'stale') return 'stale';
      if (generated?.generated_code) return 'review';
      if (generated) return 'reload code';
      const persisted = datasetReviewFor(state.selectedTarget);
      if (persisted?.output_preview) return persisted.status || 'completed';
      if (persisted?.generated_code) return 'review';
      return 'not generated';
    }

    async function preparePlan() {
      if (!studyDir() || !state.selectedTarget) return;
      beginOperation('Preparing dependency plan', `Checking whether ${state.selectedTarget} needs upstream ADaM datasets.`);
      const payload = {
        study_dir: studyDir(),
        study_id: state.studyId,
        run_id: runId(),
        target_datasets: selectedTargets(),
        approved_dependency_datasets: []
      };
      try {
        const plan = await api('/runs/prepare', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(payload)
        });
        state.plan = plan;
        await refreshGraphState();
        addEvent('Dependency plan prepared', `${state.selectedTarget} status: ${plan.dependency_review_status}.`);
        setPill('planStatus', plan.dependency_review_status || 'planned');
        renderPlan(plan);
        renderDraftSpecPane();
        renderGraphAwareDashboard();
        completeOperation('Dependency plan ready', dependencyPlanSummary(plan));
        setStep(4);
      } catch (error) {
        setPill('planStatus', 'failed');
        byId('planView').textContent = String(error);
        failOperation('Dependency planning failed', error);
      }
    }

    function renderPlan(plan) {
      const blocks = (plan.blocked_datasets || []).map((item) => `<li>${escapeHtml(item.dataset)} needs ${escapeHtml(item.blocked_by)}: ${humanDependencyReason(item.reason)}</li>`).join('');
      const decisions = (plan.dependency_decisions || []).map((item) => `<li>${escapeHtml(item.dataset)}: ${dependencyDecisionText(item)}</li>`).join('');
      byId('planView').innerHTML = `
        <p><strong>Selected target:</strong> ${escapeHtml(state.selectedTarget || '')}</p>
        <p><strong>Runnable now:</strong> ${escapeHtml((plan.runnable_datasets || []).join(', ') || 'None')}</p>
        ${blocks ? `<p class="note warn">Dependency action needed before generation:</p><ul class="clean">${blocks}</ul>` : '<p class="note strong">No blocking dependency action is required.</p>'}
        <ul class="clean">${decisions || '<li>No explicit dependency was detected for this target.</li>'}</ul>
      `;
    }

    function dependencyPlanSummary(plan) {
      const blocked = plan?.blocked_datasets || [];
      const runnable = plan?.runnable_datasets || [];
      if (blocked.length) {
        return `${blocked.length} dataset(s) need action before generation. Review the dependency plan below.`;
      }
      if (runnable.length) {
        return `${runnable.join(', ')} can proceed. Continue with Finalize Inputs / Draft Spec.`;
      }
      return 'No runnable target yet. Review input evidence and target selection.';
    }

    function humanDependencyReason(reason) {
      const value = String(reason || '');
      if (value === 'dependency_user_action_required') return 'missing upstream ADaM; provide it or approve system generation later';
      if (value === 'unsupported_dataset') return 'not an ADaM target supported by this prototype';
      if (value === 'blocked_by_dependency') return 'another required dataset is not ready';
      return escapeHtml(value || 'review required');
    }

    function dependencyDecisionText(decision) {
      const dependencies = decision.dependencies || [];
      if (!dependencies.length) {
        if (decision.source === 'no_dependency_evidence') {
          return 'no upstream ADaM was detected; human review should confirm this is correct.';
        }
        if (decision.source === 'input_spec_no_adam_dependency') {
          return 'user spec did not show an upstream ADaM dependency.';
        }
        return 'no upstream ADaM dependency currently detected.';
      }
      return `uses upstream ADaM ${dependencies.join(', ')}. Source: ${String(decision.source || 'evidence').replaceAll('_', ' ')}.`;
    }

    async function finalizeInputsForDraftSpec() {
      if (!state.selectedTarget) return;
      if (!state.plan) await preparePlan();
      beginOperation('Finalizing inputs', `Checking whether ${state.selectedTarget} has an approved spec or needs a draft spec.`);
      byId('draftSpecPane').innerHTML = '<p class="note warn">Finalizing uploaded inputs...</p>';
      setPill('codeStatus', 'running');
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(state.selectedTarget)}/finalize-inputs`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            study_id: state.studyId,
            config_path: byId('configPath').value.trim() || null,
            rscript_path: byId('rscriptPath').value.trim() || null,
            ...llmOverridePayload()
          })
        });
        if (payload.draft_spec) {
          state.draftSpecByDataset[payload.dataset] = payload.draft_spec;
        }
        state.finalizedInputsByDataset[payload.dataset] = payload;
        if (payload.approved_draft_spec_available) {
          state.draftSpecReviewByDataset[payload.dataset] = {
            dataset: payload.dataset,
            approved: true,
            approved_spec_path: payload.approved_spec_path
          };
        }
        setPill('codeStatus', payload.next_action === 'review_draft_spec' ? 'draft review' : 'not generated');
        byId('approveDraftSpecButton').disabled = payload.next_action !== 'review_draft_spec';
        addEvent('Inputs finalized', payload.message);
        completeOperation('Inputs finalized', payload.message);
        renderDraftSpecPane();
      } catch (error) {
        setPill('codeStatus', 'failed');
        byId('draftSpecPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Finalize inputs failed', error);
      }
    }

    async function approveDraftSpec() {
      const draft = draftSpecFor(state.selectedTarget);
      if (!draft) return;
      beginOperation('Approving draft spec', `Recording approval for ${draft.dataset} draft spec in this run.`);
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(draft.dataset)}/draft-spec-review`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            reviewer: byId('reviewer').value.trim() || 'local_user',
            decision: 'approve',
            notes: byId('reviewNotes').value.trim() || 'Approved for code generation in this run.'
          })
        });
        state.draftSpecReviewByDataset[payload.dataset] = payload;
        setPill('codeStatus', 'not generated');
        addEvent('Draft spec approved', `${payload.dataset} draft spec can now be used for R code generation.`);
        completeOperation('Draft spec approved', `${payload.dataset} can now use the approved draft spec for code generation.`);
        renderDraftSpecPane();
      } catch (error) {
        byId('draftSpecPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Draft spec approval failed', error);
      }
    }

    function renderDraftSpecPane() {
      const node = byId('draftSpecPane');
      if (!node) return;
      if (!state.selectedTarget) {
        node.innerHTML = '<p class="note">Choose an output dataset before finalizing inputs.</p>';
        byId('approveDraftSpecButton').disabled = true;
        return;
      }
      const finalized = finalizedInputsFor(state.selectedTarget);
      if (finalized?.input_spec_available || targetHasInputSpec(state.selectedTarget)) {
        const specPath = finalized?.input_spec_path ? ` Artifact: ${escapeHtml(finalized.input_spec_path)}` : '';
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} has an uploaded input spec. The code generator will use that spec directly.${specPath}</p>`;
        byId('approveDraftSpecButton').disabled = true;
        return;
      }
      if (finalized?.approved_draft_spec_available) {
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} already has a user-approved draft spec for this run. The code generator can use it now. Artifact: ${escapeHtml(finalized.approved_spec_path || '')}</p>`;
        byId('approveDraftSpecButton').disabled = true;
        return;
      }
      const draft = draftSpecFor(state.selectedTarget);
      const review = draftSpecReviewFor(state.selectedTarget);
      if (!draft) {
        node.innerHTML = `<p class="note warn">No input spec found for ${escapeHtml(state.selectedTarget)}. Generate a draft spec from uploaded SDTM/reference/define/legacy evidence, review it, then approve it before generating R code.</p>`;
        byId('approveDraftSpecButton').disabled = true;
        return;
      }
      const rows = (draft.variables || []).slice(0, 20).map((item) => `
        <tr>
          <td>${escapeHtml(item.variable || '')}</td>
          <td>${escapeHtml(item.type || '')}</td>
          <td>${escapeHtml((item.source_domains || []).join(', '))}</td>
          <td>${escapeHtml(item.derivation || '')}</td>
          <td>${escapeHtml(item.risk_level || '')}</td>
        </tr>
      `).join('');
      node.innerHTML = `
        <p class="note ${review?.approved ? 'strong' : 'warn'}">
          Draft spec for ${escapeHtml(draft.dataset)} ${review?.approved ? 'approved for this run' : 'requires review before code generation'}.
          Artifact: ${escapeHtml(draft.spec_path)}
        </p>
        <div class="table-wrap"><table><thead><tr><th>Variable</th><th>Type</th><th>Source</th><th>Derivation</th><th>Risk</th></tr></thead><tbody>${rows || '<tr><td class="muted" colspan="5">No variables returned.</td></tr>'}</tbody></table></div>
        <div style="margin-top:10px;"><h3>Draft warnings</h3><ul class="clean">${listItems(draft.warnings, 'None reported.')}</ul></div>
      `;
      byId('approveDraftSpecButton').disabled = Boolean(review?.approved);
    }

    function renderGraphAwareDashboard() {
      const summary = state.inputSummary;
      const fileCount =
        (summary?.sdtm?.length || 0) +
        (summary?.specs?.length || 0) +
        (summary?.reference_adam?.length || 0) +
        (summary?.define?.length || 0) +
        (summary?.legacy_code?.length || 0);
      const targets = state.targetCandidates || [];
      const runnable = state.plan?.runnable_datasets || [];
      const blocked = state.plan?.blocked_datasets || [];
      byId('metricInputs').textContent = String(fileCount);
      byId('metricTargets').textContent = String(targets.length);
      byId('metricRunnable').textContent = String(runnable.length);
      byId('metricBlocked').textContent = String(blocked.length);
      setPill('graphStatus', blocked.length ? 'blocked' : targets.length ? 'ready' : 'waiting');
      renderDependencyGraph(targets, runnable, blocked);
      renderDatasetBoard(targets, runnable, blocked);
    }

    function renderDependencyGraph(targets, runnable, blocked) {
      const node = byId('dependencyGraph');
      const sdtm = (state.inputSummary?.sdtm || []).map((item) => item.dataset).filter(Boolean);
      if (!targets.length && !sdtm.length) {
        node.innerHTML = '<div class="muted">Load inputs to build the study graph.</div>';
        return;
      }
      const blockedNames = new Set((blocked || []).map((item) => item.dataset));
      const orderedTargets = [
        ...(state.selectedTarget ? [state.selectedTarget] : []),
        ...targets.filter((target) => target !== state.selectedTarget)
      ];
      const rows = orderedTargets.map((target) => {
        const dependencies = dependenciesForTarget(target);
        const status = datasetStatus(target, runnable, blocked);
        const isBlocked = blockedNames.has(target);
        const depItems = dependencies.length
          ? dependencies.map((dependency, index) => dependencyStepHtml(dependency, index, runnable, targets)).join('')
          : `<li><span class="step-dot">2</span><span>No upstream ADaM dependency is currently detected. This is an evidence-based planning result, not a clinical guarantee.</span></li>`;
        const decision = dependencyDecisionFor(target);
        return `
          <div class="dependency-card ${target === state.selectedTarget ? 'active' : ''} ${isBlocked ? 'blocked' : ''}">
            <div class="dependency-title">
              <span>${escapeHtml(target)} generation plan</span>
              <span class="pill ${isBlocked || status === 'failed' ? 'fail' : status === 'ready' || status === 'completed' || status === 'reference' ? '' : 'warn'}">${escapeHtml(status)}</span>
            </div>
            <ul class="dependency-steps">
              <li><span class="step-dot">1</span><span>Use uploaded SDTM evidence${sdtm.length ? `: ${escapeHtml(sdtm.slice(0, 8).join(', '))}${sdtm.length > 8 ? ', ...' : ''}` : '. No SDTM source has been recognized yet.'}</span></li>
              ${depItems}
              <li><span class="step-dot">3</span><span>${nextActionText(target, status, isBlocked)}</span></li>
            </ul>
            <div class="dependency-note">${escapeHtml(decision?.reason || 'Prepare a dependency plan to explain why this target is ready or blocked.')}</div>
          </div>
        `;
      });
      node.innerHTML = rows.join('') || '<div class="muted">No dependency graph yet.</div>';
    }

    function dependencyStepHtml(dependency, index, runnable, targets) {
      const available = dependencyAvailable(dependency, runnable, targets);
      const evidence = hasDatasetEvidence(dependency) ? 'provided in Reference ADaM' : (runnable || []).includes(dependency) ? 'planned/runnable in this run' : (targets || []).includes(dependency) ? 'selected as a target' : 'missing';
      return `<li><span class="step-dot">${index + 2}</span><span>Requires upstream ADaM <strong>${escapeHtml(dependency)}</strong>: ${available ? 'available' : 'needs user action'} (${escapeHtml(evidence)}).</span></li>`;
    }

    function dependencyDecisionFor(target) {
      return (state.plan?.dependency_decisions || []).find((item) => item.dataset === target) || null;
    }

    function nextActionText(target, status, isBlocked) {
      if (isBlocked) {
        const block = (state.plan?.blocked_datasets || []).find((item) => item.dataset === target);
        return `Action required before generation: ${block ? `${block.dataset} needs ${block.blocked_by}` : 'resolve blocked dependencies'}.`;
      }
      if (!state.plan) return 'Next: prepare the dependency plan for this target.';
      if (!targetSpecGateSatisfied(target)) return 'Next: click Finalize Inputs / Draft Spec, then approve the draft spec if no uploaded spec exists.';
      if (!generatedFor(target)) return 'Next: click Generate R Code. This will not run R yet.';
      if (generatedFor(target)?.status === 'stale') return 'Inputs changed after code generation. Regenerate R code before review or execution.';
      if (!generatedFor(target)?.generated_code) return 'Generated-code state exists, but the code text is not loaded in this browser. Reload the run review before approving.';
      if (!reviewFor(target) && !executionFor(target)) return 'Next: review the generated R code, then approve local execution.';
      if (executionFor(target)?.status === 'completed') return 'Next: inspect the generated ADaM table, compare result, and downloads.';
      if (executionFor(target)?.status === 'terminal_failure') return 'Execution failed. Review diagnostics before retrying.';
      if (status === 'reference') return 'This dataset is available as reference evidence. Select another output target if you want to generate code.';
      return 'Next: continue with the active review step shown below.';
    }

    function renderDatasetBoard(targets, runnable, blocked) {
      const node = byId('datasetBoard');
      if (!targets.length) {
        node.innerHTML = '<div class="muted">No dataset selected yet.</div>';
        return;
      }
      const blockedNames = new Set((blocked || []).map((item) => item.dataset));
      node.innerHTML = targets.map((target) => {
        const status = datasetStatus(target, runnable, blocked);
        const isActive = target === state.selectedTarget;
        const generated = generatedFor(target);
        const review = reviewFor(target);
        const execution = executionFor(target);
        const persisted = datasetReviewFor(target);
        const isGenerated = Boolean(generated || persisted?.generated_code);
        const isCompleted = execution?.status === 'completed' || Boolean(persisted?.output_preview);
        const hasReview = Boolean(review || persisted?.generated_code);
        const statusClass = status === 'blocked' || status === 'failed' ? 'fail' : ['ready', 'completed', 'reference'].includes(status) ? '' : 'warn';
        return `
          <div class="dataset-card ${isActive ? 'active' : ''} ${blockedNames.has(target) ? 'blocked' : ''}" data-card-target="${escapeHtml(target)}">
            <div class="dataset-top">
              <span class="dataset-name">${escapeHtml(target)}</span>
              <span class="pill ${statusClass}">${escapeHtml(status)}</span>
            </div>
            <div class="stage-strip">
              <div class="stage done">inputs</div>
              <div class="stage ${state.plan ? (blockedNames.has(target) ? 'blocked' : 'done') : 'active'}">plan</div>
              <div class="stage ${isGenerated ? 'done' : target === state.selectedTarget ? 'active' : ''}">code</div>
              <div class="stage ${hasReview ? 'done' : isGenerated ? 'active' : ''}">review</div>
              <div class="stage ${isCompleted ? 'done' : execution ? 'blocked' : ''}">run</div>
            </div>
          </div>
        `;
      }).join('');
      for (const card of node.querySelectorAll('[data-card-target]')) {
        card.addEventListener('click', () => {
          state.selectedTarget = card.dataset.cardTarget;
          renderTargetButtons(state.targetCandidates || []);
          resetActiveDatasetView();
          preparePlan();
        });
      }
    }

    function hasDatasetEvidence(dataset) {
      return Boolean((state.inputSummary?.reference_adam || []).find((item) => item.dataset === dataset));
    }

    function dependenciesForTarget(target) {
      const decision = (state.plan?.dependency_decisions || []).find((item) => item.dataset === target);
      return decision?.dependencies || [];
    }

    function dependencyAvailable(dependency, runnable, targets) {
      return hasDatasetEvidence(dependency) || (runnable || []).includes(dependency) || (targets || []).includes(dependency);
    }

    function datasetStatus(target, runnable, blocked) {
      if ((blocked || []).find((item) => item.dataset === target)) return 'blocked';
      const execution = executionFor(target);
      const persisted = datasetReviewFor(target);
      if (execution) return execution.status;
      if (persisted?.output_preview) return persisted.status || 'completed';
      if (generatedFor(target)?.status === 'stale') return 'stale';
      if (generatedFor(target)?.generated_code || persisted?.generated_code) return 'needs review';
      if (generatedFor(target)) return 'reload code';
      if (hasDatasetEvidence(target)) return 'reference';
      if ((runnable || []).includes(target)) return 'ready';
      if (state.plan) return 'waiting';
      return 'candidate';
    }

    async function generateCode() {
      if (!state.selectedTarget) return;
      if (!state.plan) await preparePlan();
      if (!targetSpecGateSatisfied(state.selectedTarget)) {
        byId('reviewPane').innerHTML = '<p class="note warn">No approved input spec is available. Click Finalize Inputs / Draft Spec, review the draft spec, then approve it before generating R code.</p>';
        byId('draftSpecPane').scrollIntoView({behavior: 'smooth', block: 'center'});
        return;
      }
      beginOperation('Generating R code', `Calling the selected LLM/code generator for ${state.selectedTarget}. This may take a few minutes.`);
      setPill('codeStatus', 'running');
      try {
        const overrides = llmOverridePayload();
        const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(state.selectedTarget)}/generate-code`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            study_id: state.studyId,
            config_path: byId('configPath').value.trim() || null,
            rscript_path: byId('rscriptPath').value.trim() || null,
            ...overrides
          })
        });
        state.generated = payload;
        state.generatedByDataset[payload.dataset] = payload;
        state.review = reviewFor(payload.dataset);
        state.execution = executionFor(payload.dataset);
        state.selectedView = 'summary';
        setActiveTab();
        setPill('codeStatus', 'review');
        byId('approveButton').disabled = false;
        addEvent('R code generated', `${payload.dataset} code is ready for review.`);
        completeOperation('R code generated', `${payload.dataset} code is ready for review. R has not been executed yet.`);
        renderGraphAwareDashboard();
        setStep(5);
        renderPane();
      } catch (error) {
        setPill('codeStatus', 'failed');
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('R code generation failed', error);
      }
    }

    async function approveAndRun() {
      const generated = generatedFor(state.selectedTarget);
      if (!generated) return;
      state.generated = generated;
      beginOperation('Running approved R code', `Approving ${generated.dataset} code, then executing it with local Rscript.`);
      setPill('codeStatus', 'running');
      try {
        state.review = await api(`/runs/${encodeURIComponent(generated.run_id)}/datasets/${encodeURIComponent(generated.dataset)}/code-review`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            reviewer: byId('reviewer').value.trim() || 'local_user',
            decision: 'approve',
            notes: byId('reviewNotes').value.trim()
          })
        });
        state.reviewByDataset[generated.dataset] = state.review;
        state.execution = await api(`/runs/${encodeURIComponent(generated.run_id)}/datasets/${encodeURIComponent(generated.dataset)}/execute-approved-code`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            study_id: state.studyId,
            rscript_path: byId('rscriptPath').value.trim() || null
          })
        });
        state.executionByDataset[generated.dataset] = state.execution;
        await loadReviewSummary(generated.run_id);
        addEvent('Sandbox completed', `${generated.dataset} finished with status ${state.execution.status}.`);
        setPill('codeStatus', state.execution.status);
        if (state.execution.status === 'completed') {
          completeOperation('R sandbox completed', `${generated.dataset} output passed structural validation and is ready for review.`);
        } else {
          failOperation('R sandbox finished with failure', `${generated.dataset} status: ${state.execution.status}. Check diagnostics.`);
        }
        state.selectedView = 'output';
        setActiveTab();
        setStep(6);
        renderPane();
        renderGraphAwareDashboard();
      } catch (error) {
        setPill('codeStatus', 'failed');
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Approve and run failed', error);
      }
    }

    async function loadReviewSummary(id) {
      try {
        state.runReview = await api(`/runs/${encodeURIComponent(id)}/review-summary?study_dir=${encodeURIComponent(studyDir())}`);
        for (const review of state.runReview?.dataset_reviews || []) {
          if (review?.compare_summary) {
            state.compareResults[review.dataset] = review.compare_summary;
          }
        }
        syncActiveDatasetState();
        setPill('codeStatus', codeStatusForActiveDataset());
        byId('approveButton').disabled = !canApproveGeneratedCode(state.selectedTarget);
        renderAdvanced();
        renderGraphAwareDashboard();
        renderPane();
      } catch {
        state.runReview = null;
        renderAdvanced();
        renderGraphAwareDashboard();
      }
    }

    function selectedDatasetReview() {
      return datasetReviewFor(state.selectedTarget);
    }

    function renderPane() {
      const pane = byId('reviewPane');
      syncActiveDatasetState();
      const generated = generatedFor(state.selectedTarget);
      const datasetReview = selectedDatasetReview();
      if (state.selectedView === 'summary') {
        if (!generated) {
          pane.innerHTML = '<p class="note">Generate code after choosing a target. Nothing has been sent to R yet.</p>';
          return;
        }
        pane.innerHTML = `
          <p class="note strong">R code is ready for ${escapeHtml(generated.dataset)}. Review the assumptions, then approve to run locally.</p>
          ${draftSpecNotice(generated)}
          <div class="grid2">
            <div class="card"><h3>What will happen</h3><ul class="clean">${listItems(generated.expected_outputs, 'No output declared.')}</ul></div>
            <div class="card"><h3>Inputs used</h3><ul class="clean">${listItems(generated.used_inputs, 'No inputs declared.')}</ul></div>
          </div>
        `;
        return;
      }
      if (state.selectedView === 'code') {
        pane.innerHTML = generated ? `<pre>${escapeHtml(generated.generated_code)}</pre>` : '<p class="note">No R code generated yet.</p>';
        return;
      }
      if (state.selectedView === 'risk') {
        pane.innerHTML = generated ? `
          ${draftSpecNotice(generated)}
          <div class="grid2">
            <div><h3>Assumptions</h3><ul class="clean">${listItems(generated.assumptions, 'None reported.')}</ul></div>
            <div><h3>Risk Points</h3><ul class="clean">${listItems(generated.risk_points, 'None reported.')}</ul></div>
          </div>
          <div class="card" style="margin-top:12px;"><h3>Generation Warnings</h3><ul class="clean">${listItems(generated.warnings, 'None reported.')}</ul></div>
        ` : '<p class="note">No generated package yet.</p>';
        return;
      }
      if (state.selectedView === 'timeline') {
        pane.innerHTML = `<div class="timeline" id="timelinePane">${timelineHtml()}</div>`;
        return;
      }
      pane.innerHTML = resultWorkspace(datasetReview);
      attachResultHandlers(datasetReview);
    }

    function draftSpecNotice(generated) {
      if (!generated?.draft_spec_path) return '';
      return `
        <p class="note warn">
          Draft spec generated for ${escapeHtml(generated.dataset)} because no approved input spec was supplied.
          This draft is evidence for review, not an approved production rule.
          Artifact: ${escapeHtml(generated.draft_spec_path)}
        </p>
      `;
    }

    function resultWorkspace(review) {
      if (!review) return '<p class="note">No generated ADaM output found yet.</p>';
      return `
        <p class="note strong">Result review for ${escapeHtml(review.dataset)}. Use the tabs below to inspect the generated table, compare it with reference ADaM, and download artifacts.</p>
        <div class="result-tabs">
          <button class="result-tab ${state.selectedResultView === 'generated' ? 'active' : ''}" data-result-view="generated">Generated Table</button>
          <button class="result-tab ${state.selectedResultView === 'reference' ? 'active' : ''}" data-result-view="reference">Reference Table</button>
          <button class="result-tab ${state.selectedResultView === 'compare' ? 'active' : ''}" data-result-view="compare">Compare</button>
          <button class="result-tab ${state.selectedResultView === 'downloads' ? 'active' : ''}" data-result-view="downloads">Downloads</button>
        </div>
        <div id="resultPane">${resultPaneHtml(review)}</div>
      `;
    }

    function resultPaneHtml(review) {
      if (state.selectedResultView === 'compare') return comparePane(review);
      if (state.selectedResultView === 'downloads') return downloadsPane(review);
      return outputPreview(review, state.selectedResultView === 'reference' ? 'reference' : 'generated');
    }

    function outputPreview(review, kind) {
      const preview = review?.output_preview;
      const selectedPreview = kind === 'reference' ? review?.reference_preview : preview;
      if (!selectedPreview) return `<p class="note">${kind === 'reference' ? 'No reference ADaM found for this dataset.' : 'No generated ADaM output found yet.'}</p>`;
      const key = tableKey(review.dataset, kind);
      const page = state.tablePages[key] || null;
      const columns = page?.columns || selectedPreview.columns || [];
      const rows = page?.rows || selectedPreview.sample_rows || [];
      const head = columns.map((column) => `<th>${escapeHtml(column)}</th>`).join('');
      const body = rows.map((row) => `<tr>${columns.map((column) => `<td>${escapeHtml(row[column] || '')}</td>`).join('')}</tr>`).join('');
      const rowCount = page?.row_count ?? selectedPreview.row_count ?? '-';
      const currentPage = page?.page || 1;
      const totalPages = page?.total_pages || 0;
      return `
        <p class="note strong">${kind === 'reference' ? 'Reference ADaM' : 'Generated ADaM'}: ${escapeHtml(selectedPreview.file_name)} | ${rowCount} rows | ${columns.length} columns.</p>
        <div class="table-toolbar">
          <span class="muted">${totalPages ? `Page ${currentPage} of ${totalPages}` : 'Showing preview rows.'}</span>
          <div class="button-row" style="margin:0;">
            <button class="secondary" data-table-action="prev" data-kind="${kind}" ${currentPage <= 1 ? 'disabled' : ''}>Previous</button>
            <button class="secondary" data-table-action="next" data-kind="${kind}" ${totalPages && currentPage >= totalPages ? 'disabled' : ''}>Next</button>
            <button class="secondary" data-table-action="load" data-kind="${kind}">Load Full Page</button>
          </div>
        </div>
        <div class="table-wrap"><table><thead><tr>${head}</tr></thead><tbody>${body || '<tr><td class="muted">No preview rows.</td></tr>'}</tbody></table></div>
      `;
    }

    function comparePane(review) {
      const compare = state.compareResults[review.dataset] || review.compare_summary;
      if (!compare) return '<p class="note">Compare has not been run yet.</p>';
      const mismatchRows = (compare.mismatch_samples || []).map((item) => `
        <tr><td>${escapeHtml(item.key)}</td><td>${escapeHtml(item.column)}</td><td>${escapeHtml(item.generated)}</td><td>${escapeHtml(item.reference)}</td></tr>
      `).join('');
      return `
        <p class="note ${compare.status === 'match' ? 'strong' : 'warn'}">Compare status: ${escapeHtml(compare.status)}. ${escapeHtml(compare.note || '')}</p>
        <div class="grid3">
          <div class="metric"><span class="metric-value">${compare.row_count_generated ?? '-'}</span><span class="metric-label">generated rows</span></div>
          <div class="metric"><span class="metric-value">${compare.row_count_reference ?? '-'}</span><span class="metric-label">reference rows</span></div>
          <div class="metric"><span class="metric-value">${compare.mismatch_count ?? 0}</span><span class="metric-label">cell mismatches</span></div>
        </div>
        <div class="grid2" style="margin-top:12px;">
          <div class="card"><h3>Column differences</h3><ul class="clean">
            <li>Generated only: ${escapeHtml((compare.generated_only_columns || []).join(', ') || 'none')}</li>
            <li>Reference only: ${escapeHtml((compare.reference_only_columns || []).join(', ') || 'none')}</li>
            <li>Keys used: ${escapeHtml((compare.key_columns || []).join(', ') || 'row order')}</li>
          </ul></div>
          <div class="card"><h3>Row key differences</h3><ul class="clean">
            <li>Generated only keys: ${escapeHtml((compare.generated_only_keys || []).slice(0, 8).join(', ') || 'none')}</li>
            <li>Reference only keys: ${escapeHtml((compare.reference_only_keys || []).slice(0, 8).join(', ') || 'none')}</li>
          </ul></div>
        </div>
        <div class="button-row"><button class="secondary" id="refreshCompareButton">Run Compare Again</button></div>
        <h3 style="margin-top:12px;">Mismatch Samples</h3>
        <div class="table-wrap"><table><thead><tr><th>Key</th><th>Column</th><th>Generated</th><th>Reference</th></tr></thead><tbody>${mismatchRows || '<tr><td class="muted" colspan="4">No mismatch samples.</td></tr>'}</tbody></table></div>
      `;
    }

    function downloadsPane(review) {
      const items = review.downloads || [];
      if (!items.length) return '<p class="note">No downloadable artifacts are registered yet.</p>';
      return `<div class="download-list">${items.map((item) => `
        <div class="download-item">
          <div>
            <strong>${escapeHtml(item.label)}</strong>
            <div class="file-meta">${escapeHtml(item.file_name || item.note || 'Not available yet.')}</div>
          </div>
          <button class="secondary" data-download-kind="${escapeHtml(item.kind)}" ${item.available ? '' : 'disabled'}>Download</button>
        </div>
      `).join('')}</div>`;
    }

    function attachResultHandlers(review) {
      if (!review) return;
      for (const button of document.querySelectorAll('[data-result-view]')) {
        button.addEventListener('click', () => {
          state.selectedResultView = button.dataset.resultView;
          renderPane();
        });
      }
      for (const button of document.querySelectorAll('[data-table-action]')) {
        button.addEventListener('click', () => handleTableAction(review, button.dataset.kind, button.dataset.tableAction));
      }
      for (const button of document.querySelectorAll('[data-download-kind]')) {
        button.addEventListener('click', () => downloadArtifact(review, button.dataset.downloadKind));
      }
      const compareButton = byId('refreshCompareButton');
      if (compareButton) compareButton.addEventListener('click', () => refreshCompare(review));
    }

    async function handleTableAction(review, kind, action) {
      const key = tableKey(review.dataset, kind);
      const current = state.tablePages[key];
      let page = current?.page || 1;
      if (action === 'next') page += 1;
      if (action === 'prev') page = Math.max(1, page - 1);
      if (action === 'load') page = 1;
      const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(review.dataset)}/table?study_dir=${encodeURIComponent(studyDir())}&kind=${encodeURIComponent(kind)}&page=${page}&page_size=25`);
      state.tablePages[key] = payload;
      renderPane();
    }

    async function refreshCompare(review) {
      const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(review.dataset)}/compare?study_dir=${encodeURIComponent(studyDir())}`);
      state.compareResults[review.dataset] = payload;
      renderPane();
    }

    function downloadArtifact(review, kind) {
      const url = `/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(review.dataset)}/download?study_dir=${encodeURIComponent(studyDir())}&kind=${encodeURIComponent(kind)}`;
      window.location.href = url;
    }

    function tableKey(dataset, kind) {
      return `${dataset}:${kind}`;
    }

    function renderAdvanced() {
      const artifacts = state.runReview?.advanced_artifacts || {};
      const rows = Object.entries(artifacts).map(([key, value]) => `<tr><td>${escapeHtml(key)}</td><td>${escapeHtml(value)}</td></tr>`).join('');
      byId('advancedPane').innerHTML = rows
        ? `<table><thead><tr><th>Artifact</th><th>Path</th></tr></thead><tbody>${rows}</tbody></table>`
        : 'Audit artifacts appear after a run.';
    }

    function renderTimeline() {
      if (state.selectedView === 'timeline') {
        byId('reviewPane').innerHTML = `<div class="timeline" id="timelinePane">${timelineHtml()}</div>`;
      }
    }

    function timelineHtml() {
      if (!state.events.length) {
        return '<div class="note">No graph events yet.</div>';
      }
      return state.events.map((event) => `
        <div class="timeline-item">
          <div class="timeline-time">${escapeHtml(event.time)}</div>
          <div>
            <div class="timeline-title">${escapeHtml(event.title)}</div>
            <div class="muted">${escapeHtml(event.detail)}</div>
          </div>
        </div>
      `).join('');
    }

    function listItems(items, fallback) {
      if (!items || !items.length) return `<li>${escapeHtml(fallback)}</li>`;
      return items.map((item) => `<li>${escapeHtml(item)}</li>`).join('');
    }

    function setActiveTab() {
      for (const button of document.querySelectorAll('[data-view]')) {
        button.classList.toggle('active', button.dataset.view === state.selectedView);
      }
    }

    byId('createDemoButton').addEventListener('click', createDemoStudy);
    byId('startUploadButton').addEventListener('click', startUploadWorkspace);
    byId('finalizeInputsButton').addEventListener('click', finalizeInputsForDraftSpec);
    byId('approveDraftSpecButton').addEventListener('click', approveDraftSpec);
    byId('generateCodeButton').addEventListener('click', generateCode);
    byId('approveButton').addEventListener('click', approveAndRun);
    byId('addTargetButton').addEventListener('click', addManualTarget);
    byId('modelMode').addEventListener('change', updateLlmModeControls);
    byId('testLlmButton').addEventListener('click', testLlmConnection);
    for (const button of document.querySelectorAll('[data-upload-role]')) {
      button.addEventListener('click', () => uploadRole(button.dataset.uploadRole));
    }
    for (const button of document.querySelectorAll('[data-view]')) {
      button.addEventListener('click', () => {
        state.selectedView = button.dataset.view;
        setActiveTab();
        renderPane();
      });
    }
    updateLlmModeControls();
    checkHealth();
  </script>
</body>
</html>
"""
