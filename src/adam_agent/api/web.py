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
    .status-meta-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 7px;
      margin-top: 9px;
    }
    .status-chip {
      min-height: 48px;
      padding: 7px 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
    }
    .status-chip span {
      display: block;
      margin-bottom: 3px;
      color: var(--muted);
      font-size: 10px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .status-chip strong {
      display: block;
      color: var(--text);
      font-size: 12px;
      line-height: 1.3;
      overflow-wrap: anywhere;
    }
    .header-progress-track {
      height: 7px;
      overflow: hidden;
      margin-top: 9px;
      border-radius: 999px;
      background: #e4e9f0;
    }
    .header-progress-bar {
      width: 0%;
      height: 100%;
      border-radius: 999px;
      background: var(--accent);
      transition: width 0.25s ease;
    }
    .header-progress-bar.running {
      width: 68%;
      animation: progressPulse 1.2s ease-in-out infinite;
    }
    .header-progress-bar.done { width: 100%; background: var(--ok); }
    .header-progress-bar.failed { width: 100%; background: var(--danger); }
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
    .action-hints {
      display: grid;
      gap: 6px;
      margin-top: 10px;
    }
    .action-hint {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 10px;
      padding: 8px 10px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      font-size: 12px;
    }
    .action-hint strong {
      display: block;
      color: var(--text);
      font-size: 12px;
    }
    .action-hint span { color: var(--muted); }
    .action-hint.ready { border-color: #b8dfc9; background: #f2fbf5; }
    .action-hint.blocked { border-color: #e8b2ac; background: #fff8f7; }
    .action-hint.waiting { background: #fbfdff; }
    .target-option {
      display: inline-flex;
      align-items: center;
      gap: 7px;
      min-height: 38px;
      padding: 6px 8px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
    }
    .target-option.planned { border-color: rgba(15, 118, 110, 0.45); background: #eef8f6; }
    .target-option.active { box-shadow: inset 0 0 0 2px rgba(15, 118, 110, 0.16); }
    .target-check {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      margin: 0;
      color: var(--text);
      font-size: 13px;
      font-weight: 800;
      cursor: pointer;
    }
    .target-name { min-width: 44px; }
    .target-hint {
      color: var(--muted);
      font-size: 11px;
      font-weight: 700;
    }
    .target-view {
      min-height: 28px;
      padding: 0 8px;
      font-size: 12px;
    }
    .target-view.active { color: #fff; background: var(--accent); border-color: var(--accent-dark); }
    .target-selection-summary {
      margin-top: 8px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
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
    .study-progress-panel {
      margin-bottom: 12px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .study-progress-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 10px;
    }
    .study-progress-title {
      display: block;
      margin: 2px 0 3px;
      font-size: 15px;
      font-weight: 800;
    }
    .study-progress-steps {
      display: grid;
      grid-template-columns: repeat(5, minmax(0, 1fr));
      gap: 7px;
    }
    .progress-step {
      min-height: 46px;
      display: grid;
      align-content: center;
      gap: 2px;
      padding: 7px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      color: var(--muted);
      font-size: 11px;
      font-weight: 800;
      text-align: center;
    }
    .progress-step.done { color: var(--ok); background: #e8f6ee; border-color: #b8dfc9; }
    .progress-step.active { color: var(--accent-dark); background: #e6f5f2; border-color: #a7d8cf; }
    .progress-step.blocked { color: var(--danger); background: #fde9e7; border-color: #e8b2ac; }
    .progress-step span { display: block; color: inherit; font-size: 10px; font-weight: 700; }
    .review-queue-panel {
      margin: 0 0 12px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fffdf7;
    }
    .review-queue-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 9px;
    }
    .review-queue-title {
      display: block;
      margin: 2px 0 3px;
      font-size: 15px;
      font-weight: 800;
    }
    .review-queue-list {
      display: grid;
      gap: 7px;
    }
    .review-queue-item {
      display: grid;
      grid-template-columns: minmax(92px, 0.45fr) 1fr;
      gap: 10px;
      padding: 9px;
      border: 1px solid #ead7a6;
      border-radius: 7px;
      background: #fffaf0;
      font-size: 12px;
    }
    .review-queue-item.fail { border-color: #efc4be; background: #fff8f7; }
    .review-queue-target {
      color: var(--text);
      font-size: 13px;
      font-weight: 800;
    }
    .review-queue-action {
      color: var(--text);
      font-weight: 800;
    }
    .review-queue-detail {
      margin-top: 2px;
      color: var(--muted);
      line-height: 1.35;
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
    .dependency-summary {
      display: grid;
      grid-template-columns: repeat(3, minmax(0, 1fr));
      gap: 8px;
      margin-bottom: 10px;
    }
    .dependency-summary-item {
      min-height: 58px;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
    }
    .dependency-summary-item strong {
      display: block;
      margin-bottom: 3px;
      color: var(--text);
      font-size: 12px;
    }
    .dependency-flow {
      display: grid;
      gap: 7px;
      margin-bottom: 9px;
    }
    .dependency-flow-row {
      display: grid;
      grid-template-columns: 86px 1fr;
      gap: 9px;
      align-items: start;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.4;
    }
    .dependency-flow-label {
      color: var(--text);
      font-size: 12px;
      font-weight: 800;
    }
    .dependency-action {
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
      color: var(--text);
      font-size: 13px;
      line-height: 1.4;
    }
    .dependency-action.ready { border-color: #b8dfc9; background: #f2fbf5; }
    .dependency-action.waiting { border-color: #f0d19b; background: #fff8ea; }
    .dependency-action.blocked { border-color: #efc4be; background: #fff8f7; color: var(--danger); }
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
    .dataset-context {
      margin: -2px 0 8px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
    }
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
    .agent-audit-panel {
      margin-top: 12px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .agent-audit-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 9px;
    }
    .agent-audit-title { font-size: 14px; font-weight: 800; }
    .agent-audit-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 7px;
      margin-bottom: 9px;
    }
    .agent-audit-node {
      min-height: 44px;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      font-size: 12px;
    }
    .agent-audit-node strong {
      display: block;
      margin-bottom: 2px;
      color: var(--text);
      font-size: 12px;
    }
    .agent-audit-node span { color: var(--muted); }
    .agent-audit-node.warn { border-color: #f0d19b; background: #fff8ea; }
    .agent-audit-node.fail { border-color: #efc4be; background: #fff8f7; }
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
      .grid3, .grid5, .metric-grid, .study-progress-steps, .agent-audit-grid, .dependency-summary { grid-template-columns: 1fr; }
    }
    @media (max-width: 760px) {
      header { align-items: flex-start; flex-direction: column; }
      .header-status { min-width: 0; width: 100%; max-width: none; }
      .grid2 { grid-template-columns: 1fr; }
      .review-queue-item { grid-template-columns: 1fr; }
      .status-meta-grid { grid-template-columns: 1fr; }
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
          <span class="status-label">Current Status</span>
          <span class="pill warn" id="health">Checking API...</span>
        </div>
        <div class="status-detail" id="globalStatusDetail">Waiting for the local API health check.</div>
        <div class="status-meta-grid" id="headerStatusGrid">
          <div class="status-chip"><span>Now</span><strong id="headerOperation">Idle</strong></div>
          <div class="status-chip"><span>Study</span><strong id="headerStudy">Not loaded</strong></div>
          <div class="status-chip"><span>Target</span><strong id="headerTarget">None</strong></div>
          <div class="status-chip"><span>Next</span><strong id="headerNextAction">Setup</strong></div>
        </div>
        <div class="header-progress-track"><div class="header-progress-bar" id="headerOperationProgress"></div></div>
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
          <div class="study-progress-panel" id="studyProgressPanel">
            <div class="study-progress-head">
              <div>
                <span class="status-label">Study Progress</span>
                <span class="study-progress-title" id="studyProgressTitle">No study loaded</span>
                <div class="muted" id="studyProgressDetail">Load demo data or upload study files to start.</div>
              </div>
              <span class="pill warn" id="studyNextAction">setup</span>
            </div>
            <div class="study-progress-steps" id="studyProgressSteps"></div>
          </div>
          <div class="review-queue-panel" id="humanReviewQueuePanel">
            <div class="review-queue-head">
              <div>
                <span class="status-label">Human Review Queue</span>
                <span class="review-queue-title" id="humanReviewQueueTitle">No open review gate</span>
                <div class="muted" id="humanReviewQueueDetail">Graph review gates will appear here when the workflow needs a human decision.</div>
              </div>
              <span class="pill" id="humanReviewQueueStatus">clear</span>
            </div>
            <div class="review-queue-list" id="humanReviewQueueList"></div>
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
          <div class="agent-audit-panel" id="agentAuditPanel">
            <div class="agent-audit-head">
              <div>
                <div class="status-label">Agent Audit</div>
                <div class="agent-audit-title" id="agentAuditTitle">No graph decisions yet</div>
                <div class="muted" id="agentAuditDetail">Agent decisions appear after dependency planning or dataset actions.</div>
              </div>
              <span class="pill warn" id="agentAuditStatus">waiting</span>
            </div>
            <div class="agent-audit-grid" id="agentAuditGrid"></div>
            <div class="note" id="agentAuditRiskNote">No agent risk flags yet.</div>
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
          <p class="note">Select one or more ADaM datasets to plan together. The active dataset is the one shown in the review/code panels below; generation and execution still happen one dataset at a time.</p>
          <div class="button-row" id="targetButtons"></div>
          <div id="targetSelectionSummary" class="target-selection-summary">No planning target selected yet.</div>
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
          <div id="specActionHints" class="action-hints"></div>
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
          <div id="generationActionHints" class="action-hints"></div>
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
      runProgress: null,
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
      selectedTargetsForPlan: [],
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
      if (['failed', 'blocked', 'error', 'not started', 'unavailable', 'diagnose'].includes(status)) node.classList.add('fail');
      if ([
        'waiting',
        'not generated',
        'running',
        'review',
        'warning',
        'stale',
        'draft review',
        'checking',
        'setup',
        'plan',
        'dependency',
        'choose target',
        'spec review',
        'generate code',
        'regenerate',
        'reload review',
        'code review',
        'review_dependency_plan',
        'replan_dependencies',
        'review_draft_spec',
        'review_code',
        'review_terminal_failure',
        'finalize_inputs',
        'reconfirm_inputs',
        'generate_code',
        'execute_approved_code',
        'resolve_dependency',
        'prepare_dependency_plan'
      ].includes(status)) node.classList.add('warn');
    }

    function setOperation(status, title, detail) {
      const banner = byId('operationBanner');
      const statusNode = byId('operationStatus');
      byId('operationTitle').textContent = title;
      byId('operationDetail').textContent = detail;
      byId('headerOperation').textContent = title;
      statusNode.textContent = status;
      statusNode.className = 'pill';
      banner.className = 'operation-banner';
      const headerProgress = byId('headerOperationProgress');
      headerProgress.className = 'header-progress-bar';
      if (status === 'running') {
        banner.classList.add('busy');
        statusNode.classList.add('warn');
        headerProgress.classList.add('running');
      } else if (status === 'failed') {
        banner.classList.add('fail');
        statusNode.classList.add('fail');
        headerProgress.classList.add('failed');
      } else if (status === 'done') {
        banner.classList.add('done');
        headerProgress.classList.add('done');
      } else if (status === 'waiting') {
        statusNode.classList.add('warn');
      }
      byId('globalStatusDetail').textContent = detail;
      updateHeaderStatusOverview();
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

    function recognizedInputCount() {
      return (
        (state.inputSummary?.sdtm?.length || 0) +
        (state.inputSummary?.specs?.length || 0) +
        (state.inputSummary?.reference_adam?.length || 0) +
        (state.inputSummary?.define?.length || 0) +
        (state.inputSummary?.legacy_code?.length || 0)
      );
    }

    function updateHeaderStatusOverview() {
      const progress = state.runProgress || {};
      const active = state.selectedTarget || '';
      const blocked = progress.blocked_datasets || state.plan?.blocked_datasets || [];
      const runnable = progress.runnable_datasets || state.plan?.runnable_datasets || [];
      const activeStatus = active ? datasetStatus(active, runnable, blocked) : '';
      const next = progress.next_action || byId('studyNextAction')?.textContent || studyNextActionPill(active, activeStatus, blocked, recognizedInputCount());
      byId('headerStudy').textContent = state.studyId || (studyDir() ? 'Local study' : 'Not loaded');
      byId('headerTarget').textContent = active || 'None';
      byId('headerNextAction').textContent = next || 'Setup';
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
      const selected = (state.selectedTargetsForPlan || [])
        .map((target) => String(target || '').toUpperCase())
        .filter(Boolean);
      return Array.from(new Set(selected));
    }

    function planSelectionSet() {
      return new Set(selectedTargets());
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
        updateHeaderStatusOverview();
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
        await refreshRunProgress();
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
        await refreshGraphReadModels();
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
      state.runProgress = null;
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
      const touchedCount = new Set([...(payload?.touched_runs || []), ...(payload?.touched_graph_runs || [])]).size;
      const touched = touchedCount ? ` ${touchedCount} existing run(s) marked stale.` : '';
      const skipped = payload?.skipped_graph_runs?.length ? ` ${payload.skipped_graph_runs.length} graph run(s) could not be refreshed; check audit files.` : '';
      return `${payload.saved_files?.length || 0} file(s) added and inputs rescanned.${parts.length ? ` Input diff: ${parts.join(', ')}.` : ''}${touched}${skipped}`;
    }

    async function scanInputs() {
      const payload = await api(`/study-inputs?study_dir=${encodeURIComponent(studyDir())}`);
      state.inputSummary = payload;
      renderInputSummary(payload);
      addEvent('Inputs scanned', 'The app refreshed study evidence and target candidates.');
      await refreshRunProgress();
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
      state.selectedTargetsForPlan = state.selectedTarget ? [state.selectedTarget] : [];
      renderTargetButtons(available);
      if (state.selectedTarget) preparePlan();
    }

    function renderTargetButtons(targets) {
      const node = byId('targetButtons');
      if (!targets.length) {
        node.innerHTML = '<span class="muted">No ADaM targets inferred yet.</span>';
        updateHeaderStatusOverview();
        return;
      }
      if (!state.selectedTarget || !targets.includes(state.selectedTarget)) {
        state.selectedTarget = targets.includes('ADAE') ? 'ADAE' : targets[0];
      }
      const allowed = new Set(targets);
      state.selectedTargetsForPlan = selectedTargets().filter((target) => allowed.has(target));
      if (!state.selectedTargetsForPlan.length && state.selectedTarget) {
        state.selectedTargetsForPlan = [state.selectedTarget];
      }
      const planned = planSelectionSet();
      node.innerHTML = targets.map((target) => `
        <span class="target-option ${planned.has(target) ? 'planned' : ''} ${target === state.selectedTarget ? 'active' : ''}">
          <label class="target-check">
            <input type="checkbox" data-target-toggle="${escapeHtml(target)}" ${planned.has(target) ? 'checked' : ''}>
            <span class="target-name">${escapeHtml(target)}</span>
          </label>
          <button class="secondary target-view ${target === state.selectedTarget ? 'active' : ''}" data-target-view="${escapeHtml(target)}">${target === state.selectedTarget ? 'Viewing' : 'View'}</button>
        </span>
      `).join('');
      renderTargetSelectionSummary();
      for (const checkbox of node.querySelectorAll('[data-target-toggle]')) {
        checkbox.addEventListener('change', () => {
          const target = checkbox.dataset.targetToggle;
          const next = planSelectionSet();
          if (checkbox.checked) {
            next.add(target);
          } else if (next.size > 1) {
            next.delete(target);
          } else {
            checkbox.checked = true;
          }
          state.selectedTargetsForPlan = Array.from(next).sort();
          renderTargetButtons(targets);
          resetActiveDatasetView();
          preparePlan();
        });
      }
      for (const button of node.querySelectorAll('[data-target-view]')) {
        button.addEventListener('click', () => {
          state.selectedTarget = button.dataset.targetView;
          renderTargetButtons(targets);
          resetActiveDatasetView();
        });
      }
      byId('generateCodeButton').disabled = !state.selectedTarget;
      byId('finalizeInputsButton').disabled = !state.selectedTarget;
      renderDraftSpecPane();
      renderGraphAwareDashboard();
    }

    function renderTargetSelectionSummary() {
      const planned = selectedTargets();
      const active = state.selectedTarget || '';
      byId('targetSelectionSummary').textContent = planned.length
        ? `Planned together: ${planned.join(', ')}. Active detail view: ${active || 'none'}. Code generation and R execution still run only for the active detail target.`
        : `No planning target selected. Active detail view: ${active || 'none'}.`;
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
      state.selectedTargetsForPlan = Array.from(new Set([...selectedTargets(), value])).sort();
      byId('manualTarget').value = '';
      resetActiveDatasetView();
      renderTargetButtons(state.targetCandidates);
      addEvent('Target added', `${value} was added manually for planning.`);
      preparePlan();
    }

    function resetRunState() {
      state.plan = null;
      state.graphState = null;
      state.runProgress = null;
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
      state.targetCandidates = state.selectedTarget ? [state.selectedTarget] : [];
      state.selectedTargetsForPlan = state.selectedTarget ? [state.selectedTarget] : [];
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
      renderActionAvailability();
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

    async function refreshRunProgress() {
      if (!studyDir() || !runId()) return null;
      try {
        const progress = await api(`/runs/${encodeURIComponent(runId())}/progress?study_dir=${encodeURIComponent(studyDir())}`);
        state.runProgress = progress;
        applyRunProgress(progress);
        return progress;
      } catch {
        state.runProgress = null;
        return null;
      }
    }

    async function refreshGraphReadModels() {
      const graph = await refreshGraphState();
      await refreshRunProgress();
      return graph;
    }

    function applyRunProgress(progress) {
      const progressTargets = (progress?.target_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      if (progressTargets.length) {
        state.targetCandidates = Array.from(new Set([...(state.targetCandidates || []), ...progressTargets])).sort();
      }
      if (progressTargets.length && (!state.selectedTarget || !state.targetCandidates.includes(state.selectedTarget))) {
        state.selectedTarget = progressTargets.includes('ADAE') ? 'ADAE' : progressTargets[0];
      }
    }

    function applyGraphState(graph) {
      const recoveredPlan = planFromGraphState(graph);
      if (recoveredPlan) {
        state.plan = recoveredPlan;
        setPill('planStatus', recoveredPlan.dependency_review_status || 'planned');
      }
      const graphTargets = graph?.target_datasets || [];
      if (graphTargets.length) {
        state.targetCandidates = Array.from(new Set([...(state.targetCandidates || []), ...graphTargets])).sort();
      }
      const requestedTargets = graph?.requested_datasets || [];
      if (requestedTargets.length) {
        state.selectedTargetsForPlan = requestedTargets.map((target) => String(target || '').toUpperCase()).filter(Boolean);
      }
      if (graphTargets.length && (!state.selectedTarget || !state.targetCandidates.includes(state.selectedTarget))) {
        state.selectedTarget = graphTargets.includes('ADAE') ? 'ADAE' : graphTargets[0];
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
      if (recoveredPlan) {
        renderPlan(recoveredPlan);
      }
    }

    function planFromGraphState(graph) {
      if (!graph) return null;
      const requested = (graph.requested_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const targets = (graph.target_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const runnable = (graph.runnable_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const blocked = graph.blocked_datasets || [];
      const decisions = graph.dependency_decisions || [];
      if (!requested.length && !targets.length && !runnable.length && !blocked.length && !decisions.length) return null;
      return {
        requested_datasets: requested,
        target_datasets: targets,
        runnable_datasets: runnable,
        blocked_datasets: blocked,
        dependency_review_status: graph.dependency_review_status || (blocked.length ? 'blocked' : 'accepted'),
        dependency_decisions: decisions,
        dependency_plan: graph.dependency_plan || {},
        dependency_resolution: graph.dependency_resolution || []
      };
    }

    function generatedFor(dataset) {
      return dataset ? state.generatedByDataset[dataset] || null : null;
    }

    function canApproveGeneratedCode(dataset) {
      const generated = generatedFor(dataset);
      return Boolean(generated && generated.status !== 'stale' && generated.generated_code);
    }

    function activeDependencyBlock() {
      if (!state.selectedTarget) return null;
      return (state.plan?.blocked_datasets || []).find((item) => item.dataset === state.selectedTarget) || null;
    }

    function actionAvailability() {
      const target = state.selectedTarget;
      const blocked = activeDependencyBlock();
      const progress = datasetProgressFor(target);
      const progressBlocked = Boolean(progress?.blocked);
      const progressBlockReason = progress?.blocked_reason || '';
      const generated = generatedFor(target);
      const execution = executionFor(target);
      const draft = draftSpecFor(target);
      const draftReview = draftSpecReviewFor(target);
      const finalized = finalizedInputsFor(target);
      const hasSpecGate = targetSpecGateSatisfied(target);
      const finalizeReady = Boolean(target && !blocked && !progressBlocked);
      const draftApprovalReady = Boolean(target && draft && !draftReview?.approved && !finalized?.input_spec_available && !targetHasInputSpec(target));
      const generateReady = Boolean(target && !blocked && !progressBlocked && hasSpecGate);
      const approveReady = Boolean(canApproveGeneratedCode(target) && !progressBlocked);
      return {
        finalize: {
          ready: finalizeReady,
          label: 'Finalize Inputs / Draft Spec',
          reason: !target
            ? 'Choose an ADaM output first.'
            : !state.plan
              ? 'Clicking will prepare the dependency plan first, then finalize inputs if the target is runnable.'
              : progressBlocked
                ? progressBlockReason
                : blocked
                ? `${target} is blocked by ${blocked.blocked_by}. Resolve or approve the dependency plan first.`
                : hasSpecGate
                  ? `${target} already has an input spec or approved draft spec; finalizing again is optional.`
                  : `Ready to check whether ${target} has an input spec or needs a draft spec.`
        },
        approveDraft: {
          ready: draftApprovalReady,
          label: 'Approve Draft Spec',
          reason: !target
            ? 'Choose an ADaM output first.'
            : finalized?.input_spec_available || targetHasInputSpec(target)
              ? `${target} has an uploaded input spec, so no draft-spec approval is needed.`
              : draftReview?.approved || finalized?.approved_draft_spec_available
                ? `${target} draft spec is already approved for this run.`
                : draft
                  ? `Review the generated draft spec for ${target}; approve it before code generation.`
                  : 'Finalize inputs first. If no uploaded spec exists, the app will create a draft spec for review.'
        },
        generate: {
          ready: generateReady,
          label: 'Generate R Code',
          reason: !target
            ? 'Choose an ADaM output first.'
            : !state.plan
              ? 'Clicking will prepare the dependency plan first, then generate only if the target is runnable.'
              : progressBlocked
                ? progressBlockReason
                : blocked
                ? `${target} is blocked by ${blocked.blocked_by}; generation is paused until dependency review is resolved.`
                : !hasSpecGate
                  ? 'Confirm the uploaded input spec or review/approve the generated draft spec first.'
                  : generated?.status === 'stale'
                    ? 'Inputs changed after code generation; regenerate R code before review.'
                    : generated
                      ? `${target} already has generated code. Regenerate only if the current code is stale or rejected.`
                      : `Ready to call the selected code generator for ${target}.`
        },
        approveRun: {
          ready: approveReady,
          label: 'Approve And Run Locally',
          pill: execution?.status === 'completed' ? 'rerun' : execution?.status === 'terminal_failure' || execution?.status === 'failed' ? 'diagnose' : null,
          reason: !target
            ? 'Choose an ADaM output first.'
            : progressBlocked
              ? progressBlockReason
            : execution?.status === 'completed'
              ? `${target} already completed local execution. Approval remains available only if you intentionally rerun the same generated code.`
              : execution?.status === 'terminal_failure' || execution?.status === 'failed'
                ? `${target} execution failed. Review diagnostics before retrying or regenerating code.`
                : !generated
                  ? 'Generate R code first.'
                  : generated.status === 'stale'
                    ? 'Generated code is stale because inputs changed; regenerate before approval.'
                    : !generated.generated_code
                      ? 'Generated-code metadata exists, but the code text is not loaded in this browser. Reload the run review before approving.'
                      : `Ready for human code approval and local R execution for ${target}.`
        }
      };
    }

    function renderActionAvailability() {
      const availability = actionAvailability();
      setButtonAvailability('finalizeInputsButton', availability.finalize);
      setButtonAvailability('approveDraftSpecButton', availability.approveDraft);
      setButtonAvailability('generateCodeButton', availability.generate);
      setButtonAvailability('approveButton', availability.approveRun);
      renderActionHints('specActionHints', [availability.finalize, availability.approveDraft]);
      renderActionHints('generationActionHints', [availability.generate, availability.approveRun]);
    }

    function setButtonAvailability(id, item) {
      const button = byId(id);
      if (!button) return;
      button.title = item.reason;
      button.setAttribute('aria-disabled-reason', item.reason);
      button.dataset.actionReady = String(Boolean(item.ready));
      button.disabled = !item.ready;
    }

    function renderActionHints(id, items) {
      const node = byId(id);
      if (!node) return;
      node.innerHTML = items.map((item) => {
        const stateClass = item.ready ? 'ready' : 'blocked';
        const pill = item.pill || (item.ready ? 'ready' : 'waiting');
        return `
          <div class="action-hint ${stateClass}">
            <div><strong>${escapeHtml(item.label)}</strong><span>${escapeHtml(item.reason)}</span></div>
            <span class="pill ${item.ready ? '' : 'warn'}">${pill}</span>
          </div>
        `;
      }).join('');
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
      if (!studyDir()) return;
      const targets = selectedTargets();
      if (!targets.length) {
        byId('planView').innerHTML = '<p class="note warn">Select at least one ADaM dataset for planning.</p>';
        return;
      }
      beginOperation('Preparing dependency plan', `Checking dependencies for ${targets.join(', ')}.`);
      const payload = {
        study_dir: studyDir(),
        study_id: state.studyId,
        run_id: runId(),
        target_datasets: targets,
        approved_dependency_datasets: []
      };
      try {
        const plan = await api('/runs/prepare', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(payload)
        });
        state.plan = plan;
        await refreshGraphReadModels();
        addEvent('Dependency plan prepared', `${plannedTargetsForDisplay(plan, targets).join(', ')} status: ${plan.dependency_review_status}.`);
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
        <p><strong>Planned targets:</strong> ${escapeHtml(plannedTargetsForDisplay(plan).join(', ') || 'None')}</p>
        <p><strong>Active detail target:</strong> ${escapeHtml(state.selectedTarget || '')}</p>
        <p><strong>Runnable now:</strong> ${escapeHtml((plan.runnable_datasets || []).join(', ') || 'None')}</p>
        ${blocks ? `<p class="note warn">Dependency action needed before generation:</p><ul class="clean">${blocks}</ul>` : '<p class="note strong">No blocking dependency action is required.</p>'}
        <ul class="clean">${decisions || '<li>No explicit dependency was detected for this target.</li>'}</ul>
      `;
    }

    function plannedTargetsForDisplay(plan, fallbackTargets = null) {
      const requested = (plan?.requested_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      if (requested.length) return Array.from(new Set(requested));
      return fallbackTargets || selectedTargets();
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
      if (value === 'dependency_not_runtime_evidence') return 'upstream output is review-only and cannot be used as runtime input';
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
      const availability = actionAvailability().finalize;
      if (!availability.ready) {
        byId('draftSpecPane').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
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
        await refreshGraphReadModels();
        setPill('codeStatus', payload.next_action === 'review_draft_spec' ? 'draft review' : 'not generated');
        byId('approveDraftSpecButton').disabled = payload.next_action !== 'review_draft_spec';
        addEvent('Inputs finalized', payload.message);
        completeOperation('Inputs finalized', payload.message);
        renderDraftSpecPane();
        renderActionAvailability();
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
        await refreshGraphReadModels();
        setPill('codeStatus', 'not generated');
        addEvent('Draft spec approved', `${payload.dataset} draft spec can now be used for R code generation.`);
        completeOperation('Draft spec approved', `${payload.dataset} can now use the approved draft spec for code generation.`);
        renderDraftSpecPane();
        renderActionAvailability();
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
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} has an uploaded input spec. The code generator will use that spec directly. ${artifactRecordedNote('Input spec artifact')}</p>`;
        byId('approveDraftSpecButton').disabled = true;
        return;
      }
      if (finalized?.approved_draft_spec_available) {
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} already has a user-approved draft spec for this run. The code generator can use it now. ${artifactRecordedNote('Approved draft-spec artifact')}</p>`;
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
          ${artifactRecordedNote('Draft-spec artifact')}
        </p>
        <div class="table-wrap"><table><thead><tr><th>Variable</th><th>Type</th><th>Source</th><th>Derivation</th><th>Risk</th></tr></thead><tbody>${rows || '<tr><td class="muted" colspan="5">No variables returned.</td></tr>'}</tbody></table></div>
        <div style="margin-top:10px;"><h3>Draft warnings</h3><ul class="clean">${listItems(draft.warnings, 'None reported.')}</ul></div>
      `;
      byId('approveDraftSpecButton').disabled = Boolean(review?.approved);
    }

    function artifactRecordedNote(label) {
      return `${label || 'Artifact'} recorded. Technical path is available under Advanced settings and audit files.`;
    }

    function renderGraphAwareDashboard() {
      const summary = state.inputSummary;
      const progress = state.runProgress;
      const fileCount =
        (summary?.sdtm?.length || 0) +
        (summary?.specs?.length || 0) +
        (summary?.reference_adam?.length || 0) +
        (summary?.define?.length || 0) +
        (summary?.legacy_code?.length || 0);
      const targets = progress?.target_datasets?.length ? progress.target_datasets : state.targetCandidates || [];
      const runnable = progress?.runnable_datasets || state.plan?.runnable_datasets || [];
      const blocked = progress?.blocked_datasets || state.plan?.blocked_datasets || [];
      byId('metricInputs').textContent = String(fileCount);
      byId('metricTargets').textContent = String(targets.length);
      byId('metricRunnable').textContent = String(runnable.length);
      byId('metricBlocked').textContent = String(blocked.length);
      setPill('graphStatus', studyStatusPill(progress, blocked, targets));
      renderStudyProgress(targets, runnable, blocked);
      renderHumanReviewQueue();
      renderDependencyGraph(targets, runnable, blocked);
      renderDatasetBoard(targets, runnable, blocked);
      renderAgentAuditPanel();
      renderActionAvailability();
    }

    function renderStudyProgress(targets, runnable, blocked) {
      const summary = studyProgressSummary(targets, runnable, blocked);
      byId('studyProgressTitle').textContent = summary.title;
      byId('studyProgressDetail').textContent = summary.detail;
      setPill('studyNextAction', summary.action);
      updateHeaderStatusOverview();
      byId('studyProgressSteps').innerHTML = summary.steps.map((step) => `
        <div class="progress-step ${step.state}">
          ${escapeHtml(step.label)}
          <span>${escapeHtml(step.detail)}</span>
        </div>
      `).join('');
    }

    function studyProgressSummary(targets, runnable, blocked) {
      const progress = state.runProgress;
      const inputCount =
        (state.inputSummary?.sdtm?.length || 0) +
        (state.inputSummary?.specs?.length || 0) +
        (state.inputSummary?.reference_adam?.length || 0) +
        (state.inputSummary?.define?.length || 0) +
        (state.inputSummary?.legacy_code?.length || 0);
      const active = state.selectedTarget || '';
      const activeProgress = datasetProgressFor(active);
      const activeStatus = active ? (activeProgress?.status || datasetStatus(active, runnable, blocked)) : 'not selected';
      const activeNext = active
        ? (activeProgress?.blocked_reason || activeProgress?.action_label || nextActionText(active, activeStatus, Boolean((blocked || []).find((item) => item.dataset === active))))
        : 'Load or upload study evidence.';
      const interrupt = graphInterruptLabel();
      const localSteps = [
        {label: 'Inputs', detail: inputCount ? `${inputCount} file(s)` : 'not loaded', state: inputCount ? 'done' : 'active'},
        {label: 'Plan', detail: state.plan ? (blocked?.length ? 'needs action' : 'ready') : 'not prepared', state: state.plan ? (blocked?.length ? 'blocked' : 'done') : inputCount ? 'active' : ''},
        {label: 'Spec', detail: active ? specGateLabel(active) : 'choose target', state: active && targetSpecGateSatisfied(active) ? 'done' : active ? 'active' : ''},
        {label: 'Code Review', detail: active ? codeReviewLabel(active) : 'waiting', state: codeReviewStepState(active)},
        {label: 'Run', detail: active ? runStepLabel(active) : 'waiting', state: runStepState(active)}
      ];
      const steps = progress ? progressStepsFromReadModel(progress, activeProgress, inputCount) : localSteps;
      return {
        title: active
          ? `${active} is ${activeStatus}`
          : inputCount
            ? 'Inputs recognized'
            : 'No study loaded',
        detail: [
          progress?.status ? `Graph status: ${progress.status}.` : state.graphState?.status ? `Graph status: ${state.graphState.status}.` : '',
          progress?.output_quality_rollup ? studyQualityText(progress.output_quality_rollup) : '',
          progress?.plan_stale ? 'Dependency plan is stale after input changes.' : '',
          progress?.current_interrupt ? `Open gate: ${readableInterruptName(progress.current_interrupt.name)}.` : interrupt ? `Open gate: ${interrupt}.` : '',
          activeNext
        ].filter(Boolean).join(' '),
        action: progress?.next_action || studyNextActionPill(active, activeStatus, blocked, inputCount),
        steps
      };
    }

    function studyStatusPill(progress, blocked, targets) {
      if (progress?.output_quality_rollup?.completion_quality === 'review_only_complete') return 'review only';
      if (progress?.output_quality_rollup?.completion_quality === 'mixed_output_quality_complete') return 'mixed output';
      return progress?.next_action || (blocked.length ? 'blocked' : targets.length ? 'ready' : 'waiting');
    }

    function studyQualityText(rollup) {
      const quality = rollup?.completion_quality || '';
      if (quality === 'review_only_complete') {
        return `Output quality: ${rollup.review_only_outputs || 0} review-only/demo output(s); none can satisfy downstream runtime dependencies.`;
      }
      if (quality === 'mixed_output_quality_complete') {
        return `Output quality: ${rollup.real_runtime_outputs || 0} real runtime output(s), ${rollup.review_only_outputs || 0} review-only/demo output(s).`;
      }
      if (quality === 'real_runtime_complete') {
        return `Output quality: ${rollup.real_runtime_outputs || 0} real runtime output(s).`;
      }
      if (quality === 'terminal_failure_present') {
        return 'Output quality: at least one target ended in terminal failure.';
      }
      return '';
    }

    function datasetProgressFor(target) {
      const normalized = String(target || '').toUpperCase();
      if (!normalized) return null;
      return (state.runProgress?.datasets || []).find((item) => String(item.dataset || '').toUpperCase() === normalized) || null;
    }

    function progressStepsFromReadModel(progress, activeProgress, inputCount) {
      const planBlocked = progress.plan_stale || ['blocked', 'warning', 'review_required', 'stale'].includes(progress.dependency_review_status);
      const specStatus = activeProgress?.spec_status || '';
      const codeStatus = activeProgress?.code_status || '';
      const executionStatus = activeProgress?.execution_status || '';
      return [
        {label: 'Inputs', detail: inputCount ? `${inputCount} file(s)` : 'not loaded', state: inputCount ? 'done' : 'active'},
        {label: 'Plan', detail: progress.plan_stale ? 'replan needed' : planBlocked ? 'review needed' : 'ready', state: progress.plan_stale || planBlocked ? 'blocked' : 'done'},
        {label: 'Spec', detail: specStatus || 'not finalized', state: ['input_spec_ready', 'approved'].includes(specStatus) ? 'done' : specStatus === 'draft_generated' ? 'active' : ''},
        {label: 'Code Review', detail: codeStatus || 'not generated', state: codeStatus === 'approved' ? 'done' : codeStatus === 'generated' ? 'active' : codeStatus === 'stale' ? 'blocked' : ''},
        {label: 'Run', detail: executionStatus || 'waiting', state: executionStatus === 'completed' ? 'done' : ['terminal_failure', 'failed', 'stale'].includes(executionStatus) ? 'blocked' : executionStatus ? 'active' : ''}
      ];
    }

    function renderHumanReviewQueue() {
      const items = humanReviewQueueItems();
      byId('humanReviewQueueTitle').textContent = items.length
        ? `${items.length} review gate(s) open`
        : 'No open review gate';
      byId('humanReviewQueueDetail').textContent = items.length
        ? 'Review gates are read from graph state and dataset state.'
        : 'The graph has no open human decision gate for the active run.';
      setPill('humanReviewQueueStatus', items.length ? 'review' : 'clear');
      byId('humanReviewQueueList').innerHTML = items.length
        ? items.map((item) => reviewQueueItemHtml(item)).join('')
        : '<div class="muted">No dependency, draft-spec, code-review, or terminal-failure gate is open.</div>';
    }

    function humanReviewQueueItems() {
      const progress = state.runProgress || {};
      const graph = state.graphState || {};
      const items = [];
      const seen = new Set();
      addReviewQueueItem(items, seen, progress.current_interrupt, {
        scope: 'study',
        status: progress.status,
        reason: progress.action_label || ''
      });
      for (const datasetProgress of progress.datasets || []) {
        addReviewQueueItem(items, seen, datasetProgress.current_interrupt, {
          scope: 'dataset',
          dataset: datasetProgress.dataset,
          status: datasetProgress.status,
          reason: datasetProgress.action_label || datasetProgress.blocked_reason || ''
        });
        if (!datasetProgress.current_interrupt && ['review_code', 'review_draft_spec', 'review_terminal_failure', 'resolve_dependency'].includes(datasetProgress.next_action)) {
          addReviewQueueItem(items, seen, null, {
            scope: 'dataset',
            dataset: datasetProgress.dataset,
            status: datasetProgress.status,
            source: 'progress',
            interruptName: progressInterruptName(datasetProgress.next_action),
            reason: datasetProgress.action_label || datasetProgress.blocked_reason || ''
          });
        }
      }
      addReviewQueueItem(items, seen, graph.current_interrupt, {
        scope: 'study',
        status: graph.status,
        reason: graph.dependency_review_status ? `Dependency review status: ${graph.dependency_review_status}.` : ''
      });
      for (const [dataset, datasetState] of Object.entries(graph.datasets || {})) {
        const safeDatasetState = datasetState || {};
        addReviewQueueItem(items, seen, safeDatasetState.current_interrupt, {
          scope: 'dataset',
          dataset,
          status: safeDatasetState.status,
          reason: safeDatasetState.current_interrupt?.reason || ''
        });
        if (!safeDatasetState.current_interrupt && ['needs_review', 'terminal_failure'].includes(String(safeDatasetState.status || '').toLowerCase())) {
          addReviewQueueItem(items, seen, null, {
            scope: 'dataset',
            dataset,
            status: safeDatasetState.status,
            source: 'status',
            reason: 'Dataset status indicates a review step, but no open interrupt payload is present.'
          });
        }
      }
      return items;
    }

    function addReviewQueueItem(items, seen, interrupt, context) {
      const item = normalizeReviewQueueItem(interrupt, context);
      if (!item) return;
      const key = `${item.dataset || 'study'}:${item.name}:${item.source || 'interrupt'}`;
      if (seen.has(key)) return;
      seen.add(key);
      items.push(item);
    }

    function normalizeReviewQueueItem(interrupt, context) {
      const dataset = String(context.dataset || '').toUpperCase();
      if (interrupt) {
        if (typeof interrupt === 'string') {
          return {
            scope: context.scope || 'study',
            dataset,
            name: interrupt,
            status: 'open',
            source: 'interrupt',
            reason: context.reason || ''
          };
        }
        const name = interrupt.name || interrupt.interrupt || '';
        const interruptStatus = String(interrupt.status || 'open').toLowerCase();
        if (!name || interruptStatus !== 'open') return null;
        return {
          scope: context.scope || (interrupt.dataset ? 'dataset' : 'study'),
          dataset: String(interrupt.dataset || dataset || '').toUpperCase(),
          name,
          status: interruptStatus,
          source: 'interrupt',
          reason: interrupt.reason || context.reason || ''
        };
      }
      if (context.interruptName) {
        return {
          scope: context.scope || 'dataset',
          dataset,
          name: context.interruptName,
          status: context.status || 'open',
          source: context.source || 'progress',
          reason: context.reason || ''
        };
      }
      const status = String(context.status || '').toLowerCase();
      if (status === 'needs_review' || status === 'terminal_failure') {
        return {
          scope: context.scope || 'dataset',
          dataset,
          name: status === 'terminal_failure' ? 'terminal_failure' : 'review_required',
          status,
          source: context.source || 'status',
          reason: context.reason || ''
        };
      }
      return null;
    }

    function progressInterruptName(nextAction) {
      const names = {
        review_dependency_plan: 'dependency_review',
        review_draft_spec: 'draft_spec_review',
        review_code: 'code_review',
        review_terminal_failure: 'terminal_failure',
        resolve_dependency: 'dependency_user_action_required'
      };
      return names[nextAction] || 'review_required';
    }

    function reviewQueueItemHtml(item) {
      const isFailure = item.name === 'terminal_failure' || item.status === 'terminal_failure';
      const target = item.dataset || 'Study';
      return `
        <div class="review-queue-item ${isFailure ? 'fail' : ''}">
          <div>
            <div class="review-queue-target">${escapeHtml(target)}</div>
            <span class="pill ${isFailure ? 'fail' : 'warn'}">${escapeHtml(readableInterruptName(item.name))}</span>
          </div>
          <div>
            <div class="review-queue-action">${escapeHtml(reviewQueueActionText(item))}</div>
            <div class="review-queue-detail">${escapeHtml(reviewQueueDetailText(item))}</div>
          </div>
        </div>
      `;
    }

    function readableInterruptName(name) {
      const labels = {
        dependency_review: 'Dependency review',
        draft_spec_review: 'Draft spec review',
        code_review: 'Code review',
        terminal_failure: 'Terminal failure',
        review_required: 'Review required'
      };
      return labels[name] || titleFromToken(name || 'review');
    }

    function reviewQueueActionText(item) {
      const labels = {
        dependency_review: 'Review dependency plan before product steps continue.',
        draft_spec_review: 'Review the generated draft spec before code generation.',
        code_review: 'Review generated R code before local execution.',
        terminal_failure: 'Review diagnostics and choose repair, retry, or skip.',
        review_required: 'Open the dataset and continue the active review step.'
      };
      return labels[item.name] || 'Review this graph gate before continuing.';
    }

    function reviewQueueDetailText(item) {
      const source = item.source === 'status'
        ? `Status marker: ${item.status}.`
        : item.source === 'progress'
          ? `Graph progress action: ${item.status}.`
        : 'Open graph interrupt.';
      return [source, item.reason || 'No additional reason was recorded.'].join(' ');
    }

    function graphInterruptLabel() {
      const interrupt = state.graphState?.current_interrupt;
      if (!interrupt) return '';
      if (typeof interrupt === 'string') return interrupt;
      const name = interrupt.name || interrupt.interrupt || '';
      const dataset = interrupt.dataset || '';
      return [dataset, name].filter(Boolean).join(' / ');
    }

    function studyNextActionPill(active, activeStatus, blocked, inputCount) {
      if (!inputCount) return 'setup';
      if (!state.plan) return 'plan';
      if ((blocked || []).length) return 'dependency';
      if (!active) return 'choose target';
      if (!targetSpecGateSatisfied(active)) return 'spec review';
      if (!generatedFor(active)) return 'generate code';
      if (generatedFor(active)?.status === 'stale') return 'regenerate';
      if (!generatedFor(active)?.generated_code) return 'reload review';
      if (!reviewFor(active) && !executionFor(active)) return 'code review';
      if (executionFor(active)?.status === 'completed') return 'inspect output';
      if (executionFor(active)?.status === 'terminal_failure') return 'diagnose';
      return activeStatus || 'continue';
    }

    function specGateLabel(target) {
      if (finalizedInputsFor(target)?.input_spec_available || targetHasInputSpec(target)) return 'input spec';
      if (finalizedInputsFor(target)?.approved_draft_spec_available || draftSpecReviewFor(target)?.approved) return 'draft approved';
      if (draftSpecFor(target)) return 'draft review';
      return 'not finalized';
    }

    function codeReviewLabel(target) {
      const generated = generatedFor(target);
      if (!generated) return 'not generated';
      if (generated.status === 'stale') return 'stale';
      if (!generated.generated_code) return 'reload code';
      if (reviewFor(target)) return 'approved';
      return 'needs review';
    }

    function codeReviewStepState(target) {
      if (!target) return '';
      const generated = generatedFor(target);
      if (!generated) return targetSpecGateSatisfied(target) ? 'active' : '';
      if (generated.status === 'stale') return 'blocked';
      if (!generated.generated_code) return 'active';
      return reviewFor(target) ? 'done' : 'active';
    }

    function runStepLabel(target) {
      const execution = executionFor(target);
      if (!execution) return reviewFor(target) ? 'ready' : 'waiting';
      if (execution.status === 'completed') return 'completed';
      if (execution.status === 'terminal_failure') return 'failed';
      return execution.status || 'running';
    }

    function runStepState(target) {
      if (!target) return '';
      const execution = executionFor(target);
      if (!execution) return reviewFor(target) ? 'active' : '';
      if (execution.status === 'completed') return 'done';
      if (execution.status === 'terminal_failure' || execution.status === 'failed') return 'blocked';
      return 'active';
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
        const decision = dependencyDecisionFor(target);
        const dependencyRows = dependencies.length
          ? dependencies.map((dependency) => dependencyFlowRowHtml(dependency, runnable, targets)).join('')
          : `<div class="dependency-flow-row"><div class="dependency-flow-label">ADaM deps</div><div>No upstream ADaM dependency is currently detected. This is an evidence-based planning result, not a clinical guarantee.</div></div>`;
        return `
          <div class="dependency-card ${target === state.selectedTarget ? 'active' : ''} ${isBlocked ? 'blocked' : ''}">
            <div class="dependency-title">
              <span>${escapeHtml(target)} generation plan</span>
              <span class="pill ${isBlocked || status === 'failed' ? 'fail' : status === 'ready' || status === 'completed' || status === 'reference' ? '' : 'warn'}">${escapeHtml(status)}</span>
            </div>
            <div class="dependency-summary">
              <div class="dependency-summary-item"><strong>Source evidence</strong>${escapeHtml(dependencySourceEvidenceText(sdtm))}</div>
              <div class="dependency-summary-item"><strong>Dependency decision</strong>${escapeHtml(dependencyDecisionSummary(target, decision, dependencies))}</div>
              <div class="dependency-summary-item"><strong>Runtime meaning</strong>${escapeHtml(dependencyRuntimeSummary(target, status, isBlocked))}</div>
            </div>
            <div class="dependency-flow">${dependencyRows}</div>
            <div class="dependency-action ${isBlocked ? 'blocked' : state.plan ? 'ready' : 'waiting'}">${escapeHtml(nextActionText(target, status, isBlocked))}</div>
            <div class="dependency-note">${escapeHtml(decision?.reason || 'Prepare a dependency plan to explain why this target is ready or blocked.')}</div>
          </div>
        `;
      });
      node.innerHTML = rows.join('') || '<div class="muted">No dependency graph yet.</div>';
    }

    function dependencySourceEvidenceText(sdtm) {
      if (!sdtm.length) return 'No SDTM source has been recognized yet.';
      const shown = sdtm.slice(0, 8).join(', ');
      return `Recognized SDTM domains: ${shown}${sdtm.length > 8 ? ', ...' : ''}.`;
    }

    function dependencyDecisionSummary(target, decision, dependencies) {
      if (dependencies.length) return `${target} has upstream ADaM dependency: ${dependencies.join(', ')}.`;
      if (decision?.source === 'input_spec_no_adam_dependency') return `${target} input spec does not show an upstream ADaM dependency.`;
      if (decision?.source === 'no_dependency_evidence') return `${target} has no upstream ADaM dependency evidence in the current uploaded materials.`;
      return `${target} dependency plan has not recorded an upstream ADaM dependency.`;
    }

    function dependencyRuntimeSummary(target, status, isBlocked) {
      if (isBlocked || status === 'blocked') return `${target} cannot generate until the dependency gate is resolved.`;
      if (status === 'ready') return `${target} can move to spec/code review once required review gates are satisfied.`;
      if (status === 'completed') return `${target} has a completed runtime output for review.`;
      if (status === 'reference evidence') return 'Reference ADaM supports comparison/output-shape review only; it is not derivation authority.';
      return `${target} is tracked as ${status || 'candidate'} in the current study plan.`;
    }

    function dependencyFlowRowHtml(dependency, runnable, targets) {
      const runtimeAvailable = dependencyRuntimeAvailable(dependency, runnable, targets);
      const referenceEvidence = hasReferenceAdamEvidence(dependency);
      const evidence = dependencyEvidenceText(dependency, runnable, targets);
      const authorityNote = referenceEvidence
        ? ' Reference ADaM is comparison/output-shape evidence only; it is not derivation authority or a runtime dependency by itself.'
        : '';
      return `
        <div class="dependency-flow-row">
          <div class="dependency-flow-label">Needs ${escapeHtml(dependency)}</div>
          <div>${runtimeAvailable ? 'Runtime input is available or planned.' : 'User action is needed before this target can generate.'} Evidence: ${escapeHtml(evidence)}.${escapeHtml(authorityNote)}</div>
        </div>
      `;
    }

    function dependencyDecisionFor(target) {
      return (state.plan?.dependency_decisions || []).find((item) => item.dataset === target) || null;
    }

    function nextActionText(target, status, isBlocked) {
      const progress = datasetProgressFor(target);
      if (progress?.blocked_reason) return progress.blocked_reason;
      if (progress?.action_label) return progress.action_label;
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
      if (status === 'reference evidence') return 'Reference ADaM is available for compare/output-shape evidence only. It is not an approved derivation rule or runtime input by itself.';
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
        const progress = datasetProgressFor(target);
        const status = datasetStatus(target, runnable, blocked);
        const isActive = target === state.selectedTarget;
        const isPlanned = selectedTargets().includes(target);
        const generated = generatedFor(target);
        const review = reviewFor(target);
        const execution = executionFor(target);
        const persisted = datasetReviewFor(target);
        const isGenerated = Boolean(generated || persisted?.generated_code || progress?.code_status);
        const isCompleted = execution?.status === 'completed' || progress?.execution_status === 'completed' || Boolean(persisted?.output_preview);
        const hasReview = Boolean(review || persisted?.generated_code || progress?.code_status === 'approved');
        const statusClass = progress?.blocked || status === 'blocked' || status === 'failed' ? 'fail' : ['ready', 'completed', 'reference'].includes(status) ? '' : 'warn';
        return `
          <div class="dataset-card ${isActive ? 'active' : ''} ${blockedNames.has(target) || progress?.blocked ? 'blocked' : ''}" data-card-target="${escapeHtml(target)}">
            <div class="dataset-top">
              <span class="dataset-name">${escapeHtml(target)}</span>
              <span class="pill ${statusClass}">${escapeHtml(status)}</span>
            </div>
            <div class="dataset-context">${escapeHtml(datasetPlanningContext(target, isPlanned, isActive))}</div>
            <div class="stage-strip">
              <div class="stage done">inputs</div>
              <div class="stage ${state.plan ? (blockedNames.has(target) || progress?.blocked ? 'blocked' : 'done') : 'active'}">plan</div>
              <div class="stage ${isGenerated ? 'done' : target === state.selectedTarget && !progress?.blocked ? 'active' : ''}">code</div>
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
        });
      }
    }

    function datasetPlanningContext(target, isPlanned, isActive) {
      const parts = [];
      parts.push(isPlanned ? 'planned in this run' : 'view-only history/candidate');
      if (isActive) parts.push('active detail view');
      return `${target}: ${parts.join(' | ')}`;
    }

    function renderAgentAuditPanel() {
      const node = byId('agentAuditGrid');
      const decisions = activeAgentDecisions().slice(-8).reverse();
      const risks = activeRiskFlags();
      byId('agentAuditTitle').textContent = state.selectedTarget
        ? `${state.selectedTarget} agent decisions`
        : 'Study agent decisions';
      byId('agentAuditDetail').textContent = decisions.length
        ? `${decisions.length} recent bounded graph node decision(s) shown.`
        : 'No agent decisions are recorded for the active view yet.';
      setPill('agentAuditStatus', decisions.length ? 'audited' : 'waiting');
      node.innerHTML = decisions.length
        ? decisions.map((decision) => agentDecisionCard(decision)).join('')
        : '<div class="muted">Run dependency planning, draft spec, code generation, execution, or compare to populate agent audit.</div>';
      const readableRisks = risks.map(readableRiskFlag);
      byId('agentAuditRiskNote').innerHTML = readableRisks.length
        ? `<strong>Risk flags:</strong> ${escapeHtml(readableRisks.slice(0, 12).join(', '))}${readableRisks.length > 12 ? ' ...' : ''}`
        : 'No graph-level or active-dataset risk flags are recorded yet.';
    }

    function activeAgentDecisions() {
      const graph = state.graphState || {};
      const target = state.selectedTarget;
      const datasetState = target ? graph.datasets?.[target] : null;
      const decisions = datasetState?.agent_decisions?.length
        ? datasetState.agent_decisions
        : graph.agent_decisions || [];
      return Array.isArray(decisions) ? decisions : [];
    }

    function activeRiskFlags() {
      const graph = state.graphState || {};
      const target = state.selectedTarget;
      const datasetFlags = target ? graph.datasets?.[target]?.risk_flags || [] : [];
      return Array.from(new Set([...(graph.risk_flags || []), ...datasetFlags].map((flag) => String(flag || '').trim()).filter(Boolean)));
    }

    function agentDecisionCard(decision) {
      const agent = readableAgentName(decision.agent);
      const status = String(decision.status || '').toLowerCase();
      const klass = status.includes('fail') || status.includes('terminal') ? 'fail' : (decision.risk_flags || []).length || status.includes('warning') || status.includes('review') ? 'warn' : '';
      const detail = [readableDecisionName(decision.decision), readableNodeName(decision.node)].filter(Boolean).join(' | ');
      return `
        <div class="agent-audit-node ${klass}">
          <strong>${escapeHtml(agent)}</strong>
          <span>${escapeHtml(detail || 'Decision recorded')}</span>
        </div>
      `;
    }

    function readableAgentName(agent) {
      const labels = {
        evidence_agent: 'Evidence',
        dependency_agent: 'Dependency',
        spec_agent: 'Spec',
        code_agent: 'Code',
        static_review_agent: 'Static review',
        execution_agent: 'Execution',
        validation_agent: 'Validation',
        diagnosis_repair_agent: 'Diagnosis / repair',
        audit_agent: 'Audit'
      };
      return labels[agent] || String(agent || 'Agent');
    }

    function readableDecisionName(decision) {
      const labels = {
        dependency_plan_prepared: 'Dependency plan prepared',
        input_spec_ready: 'Input spec ready',
        approved_draft_spec_ready: 'Approved draft spec ready',
        draft_spec_generated: 'Draft spec generated',
        r_code_generated: 'R code generated',
        static_check_recorded: 'Static check recorded',
        r_execution_completed: 'R execution completed',
        r_execution_terminal_failure: 'R execution failed',
        reference_compare_recorded: 'Reference compare recorded',
        terminal_failure_triage_recorded: 'Terminal failure triaged',
        agent_audit_summary_written: 'Audit summary written'
      };
      return labels[decision] || titleFromToken(decision || 'decision recorded');
    }

    function readableNodeName(node) {
      const labels = {
        dependency_plan: 'Dependency planning',
        prepare_dataset_context: 'Evidence context',
        draft_spec_agent: 'Draft spec',
        generate_r_code_agent: 'Code generation',
        code_generation: 'Code generation',
        execute_approved_code: 'R execution',
        compare_reference_output: 'Reference compare',
        terminal_failure_review: 'Failure triage',
        write_agent_audit_summary: 'Audit summary'
      };
      return labels[node] || titleFromToken(node || '');
    }

    function readableRiskFlag(flag) {
      const labels = {
        static_check_limited_scope: 'Static check limited scope',
        reference_compare_limited_scope: 'Reference compare limited scope',
        terminal_failure_triage_limited_scope: 'Terminal failure triage limited scope',
        draft_spec_requires_human_review: 'Draft spec requires human review',
        missing_input_spec: 'Missing input spec',
        terminal_failure: 'Terminal failure',
        inputs_changed_after_planning: 'Inputs changed after planning',
        inputs_changed_after_dataset_progress: 'Inputs changed after dataset progress'
      };
      return labels[flag] || titleFromToken(flag || '');
    }

    function titleFromToken(value) {
      return String(value || '')
        .replace(/[_-]+/g, ' ')
        .replace(/\s+/g, ' ')
        .trim()
        .replace(/\b\w/g, (letter) => letter.toUpperCase());
    }

    function hasReferenceAdamEvidence(dataset) {
      return Boolean((state.inputSummary?.reference_adam || []).find((item) => item.dataset === dataset));
    }

    function dependenciesForTarget(target) {
      const decision = (state.plan?.dependency_decisions || []).find((item) => item.dataset === target);
      return decision?.dependencies || [];
    }

    function dependencyRuntimeAvailable(dependency, runnable, targets) {
      return (runnable || []).includes(dependency) || (targets || []).includes(dependency);
    }

    function dependencyEvidenceText(dependency, runnable, targets) {
      if ((runnable || []).includes(dependency)) return 'planned/runnable in this run';
      if ((targets || []).includes(dependency)) return 'selected as a target in this run';
      if (hasReferenceAdamEvidence(dependency)) return 'reference ADaM uploaded for compare/output-shape evidence only';
      return 'missing';
    }

    function datasetStatus(target, runnable, blocked) {
      const progress = datasetProgressFor(target);
      if (progress?.blocked) return 'blocked';
      if (progress?.output_quality?.quality_status === 'structural_stub') return 'demo output';
      if (progress?.output_quality?.quality_status === 'not_real_derivation') return 'review only';
      if (progress?.status) return progress.status;
      if ((blocked || []).find((item) => item.dataset === target)) return 'blocked';
      const execution = executionFor(target);
      const persisted = datasetReviewFor(target);
      if (execution) return execution.status;
      if (persisted?.output_preview) return persisted.status || 'completed';
      if (generatedFor(target)?.status === 'stale') return 'stale';
      if (generatedFor(target)?.generated_code || persisted?.generated_code) return 'needs review';
      if (generatedFor(target)) return 'reload code';
      if ((runnable || []).includes(target)) return 'ready';
      if (hasReferenceAdamEvidence(target)) return 'reference evidence';
      if (state.plan) return 'waiting';
      return 'candidate';
    }

    async function generateCode() {
      if (!state.selectedTarget) return;
      if (!state.plan) await preparePlan();
      const availability = actionAvailability().generate;
      if (!availability.ready) {
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
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
        await refreshGraphReadModels();
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
      const availability = actionAvailability().approveRun;
      if (!availability.ready) {
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
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
        await refreshGraphReadModels();
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
        await refreshGraphReadModels();
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
        await refreshGraphReadModels();
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
          ${artifactRecordedNote('Draft-spec artifact')}
        </p>
      `;
    }

    function resultWorkspace(review) {
      if (!review) return '<p class="note">No generated ADaM output found yet.</p>';
      return `
        <p class="note strong">Result review for ${escapeHtml(review.dataset)}. Use the tabs below to inspect the generated table, compare it with reference ADaM, and download artifacts.</p>
        ${outputQualityNotice(review.output_quality)}
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

    function outputQualityNotice(quality) {
      if (!quality) return '';
      const status = quality.quality_status || 'unknown';
      if (status === 'real_runtime_output' || status === 'not_completed') return '';
      const warnings = quality.warnings || [];
      const label = {
        structural_stub: 'Structural demo output',
        not_real_derivation: 'Mock/offline generation',
        terminal_failure: 'Terminal failure output'
      }[status] || titleFromToken(status);
      return `
        <div class="note warn">
          <strong>${escapeHtml(label)}.</strong>
          <ul class="clean">${listItems(warnings, 'This output is not runtime dependency evidence.')}</ul>
        </div>
      `;
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
      const review = state.runReview || {};
      const artifacts = state.runReview?.advanced_artifacts || {};
      const sourceRows = [];
      if (review.read_model_source) {
        sourceRows.push(['review_summary_source', titleFromToken(review.read_model_source)]);
      }
      if (review.graph_state_path) {
        sourceRows.push(['graph_state', review.graph_state_path]);
      }
      if (review.workflow_state_path) {
        sourceRows.push(['workflow_state', review.workflow_state_path]);
      }
      const rows = sourceRows.concat(Object.entries(artifacts))
        .map(([key, value]) => `<tr><td>${escapeHtml(key)}</td><td>${escapeHtml(value)}</td></tr>`)
        .join('');
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
    renderActionAvailability();
    updateHeaderStatusOverview();
    checkHealth();
  </script>
</body>
</html>
"""
