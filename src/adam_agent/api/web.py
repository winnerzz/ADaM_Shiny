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
      padding: 12px 22px;
      border-bottom: 1px solid var(--line);
      background: var(--panel);
    }
    .brand-block {
      display: grid;
      gap: 7px;
      min-width: 240px;
    }
    .header-actions {
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 8px;
    }
    .llm-config-button {
      min-height: 34px;
      padding: 0 11px;
      border-color: rgba(15, 118, 110, 0.42);
      background: #f2fbf9;
      color: var(--accent-dark);
    }
    .llm-config-button:hover {
      border-color: var(--accent);
      background: #e5f5f2;
    }
    .llm-mode-chip {
      display: inline-flex;
      align-items: center;
      min-height: 28px;
      padding: 4px 9px;
      border: 1px solid #cbd5e1;
      border-radius: 999px;
      background: #fff;
      color: var(--muted);
      font-size: 12px;
      font-weight: 800;
      white-space: nowrap;
    }
    .llm-mode-chip.real {
      border-color: #f0d19b;
      background: #fff8ea;
      color: var(--warn);
    }
    .language-toggle {
      display: inline-flex;
      align-items: center;
      gap: 4px;
      min-height: 30px;
      padding: 3px;
      border: 1px solid #cbd5e1;
      border-radius: 999px;
      background: #fff;
    }
    .language-toggle button {
      min-height: 24px;
      padding: 0 8px;
      border: 0;
      border-radius: 999px;
      color: var(--muted);
      background: transparent;
      font-size: 12px;
      font-weight: 800;
    }
    .language-toggle button.active {
      color: #fff;
      background: var(--accent);
    }
    h1 { margin: 0; font-size: 22px; }
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
      min-width: min(560px, 50vw);
      max-width: 640px;
    }
    .status-card {
      padding: 8px 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .status-row {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      margin-bottom: 6px;
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
      gap: 6px;
      margin-top: 7px;
    }
    .header-next-chip { display: none; }
    .header-now-chip { display: none; }
    .status-chip {
      min-height: 34px;
      padding: 6px 8px;
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
      height: 5px;
      overflow: hidden;
      margin-top: 7px;
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
      grid-template-columns: 280px minmax(0, 1fr) 340px;
      gap: 18px;
      align-items: start;
      padding: 18px;
      min-height: calc(100vh - 74px);
    }
    main.wide-main {
      grid-template-columns: 280px minmax(0, 1fr);
    }
    aside, section, .card {
      background: var(--panel);
      border: 1px solid #cfd7e3;
      border-radius: 8px;
      box-shadow: 0 1px 2px rgba(15, 23, 42, 0.04);
    }
    .workstream > section {
      border-color: #b9c5d4;
      box-shadow: 0 2px 8px rgba(15, 23, 42, 0.05);
    }
    aside {
      padding: 12px;
      position: sticky;
      top: 14px;
      max-height: calc(100vh - 28px);
      overflow-y: auto;
      overscroll-behavior: contain;
      scrollbar-gutter: stable;
    }
    .workstream {
      display: grid;
      gap: 18px;
      min-width: 0;
    }
    .left-rail, .inspector-rail {
      align-self: start;
      max-height: calc(100dvh - 32px);
      padding-bottom: 18px;
    }
    .inspector-rail {
      display: grid;
      gap: 12px;
      background: #fbfdff;
    }
    .inspector-title {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      padding-bottom: 10px;
      border-bottom: 1px solid var(--line);
    }
    .inspector-title strong {
      display: block;
      font-size: 14px;
    }
    .inspector-empty {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.45;
    }
    .inspector-empty strong {
      display: block;
      margin-bottom: 4px;
      color: var(--text);
      font-size: 13px;
    }
    .legacy-step-anchors { display: none; }
    .sr-only {
      position: absolute;
      width: 1px;
      height: 1px;
      overflow: hidden;
      clip: rect(0, 0, 0, 0);
      white-space: nowrap;
    }
    section {
      margin-bottom: 0;
      overflow: hidden;
    }
    .section-head {
      display: flex;
      justify-content: space-between;
      align-items: center;
      gap: 12px;
      padding: 14px 16px;
      border-bottom: 1px solid #cfd7e3;
      background: #f8fafc;
    }
    .section-head h2 {
      padding-left: 10px;
      border-left: 4px solid var(--accent);
      line-height: 1.25;
    }
    .section-body {
      padding: 16px;
      background: #fff;
    }
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
    .side-workflow-panel {
      margin-bottom: 12px;
      padding-bottom: 12px;
      border-bottom: 1px solid var(--line);
    }
    .side-workflow-title {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 8px;
    }
    .side-workflow-target {
      display: block;
      margin: 3px 0 4px;
      color: var(--text);
      font-size: 14px;
      font-weight: 800;
      line-height: 1.25;
      overflow-wrap: anywhere;
    }
    .side-workflow-detail {
      margin-bottom: 9px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .side-progress-steps {
      display: grid;
      gap: 6px;
    }
    .side-progress-step {
      display: grid;
      grid-template-columns: 18px minmax(0, 1fr);
      gap: 7px;
      align-items: start;
      padding: 7px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
    }
    .side-progress-dot {
      display: grid;
      place-items: center;
      width: 18px;
      height: 18px;
      border-radius: 999px;
      color: #fff;
      background: #bdc8d7;
      font-size: 10px;
      font-weight: 800;
    }
    .side-progress-step strong {
      display: block;
      margin-bottom: 1px;
      color: var(--text);
      font-size: 12px;
    }
    .side-progress-step span {
      display: block;
      color: var(--muted);
      font-size: 11px;
      line-height: 1.3;
      overflow-wrap: anywhere;
    }
    .side-progress-step.done { border-color: #b8dfc9; background: #f2fbf5; }
    .side-progress-step.done .side-progress-dot { background: var(--ok); }
    .side-progress-step.active { border-color: #a7d8cf; background: #eef8f6; }
    .side-progress-step.active .side-progress-dot { background: var(--accent); }
    .side-progress-step.blocked { border-color: #efc4be; background: #fff8f7; }
    .side-progress-step.blocked .side-progress-dot { background: var(--danger); }
    .side-progress-step.waiting { border-color: #f0d19b; background: #fff8ea; }
    .side-progress-step.waiting .side-progress-dot { background: var(--warn); }
    .side-queue-panel {
      margin-bottom: 12px;
      padding-bottom: 12px;
      border-bottom: 1px solid var(--line);
    }
    .side-queue-title {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 7px;
    }
    .side-queue-panel .dataset-board {
      max-height: 40dvh;
      overflow: auto;
      padding-right: 4px;
    }
    .side-review-panel {
      margin-bottom: 12px;
      padding-bottom: 12px;
      border-bottom: 1px solid var(--line);
    }
    .side-review-panel .review-queue-panel {
      margin: 0;
      padding: 0;
      border: 0;
      background: transparent;
    }
    .side-review-panel .review-queue-head {
      margin-bottom: 7px;
    }
    .side-review-panel .review-queue-title {
      font-size: 13px;
    }
    .side-review-panel .review-queue-detail {
      font-size: 11px;
    }
    .side-review-panel .review-queue-item {
      grid-template-columns: 1fr;
      padding: 8px;
      font-size: 11px;
    }
    .side-review-panel .review-queue-command-actions button {
      min-height: 28px;
      padding: 5px 8px;
    }
    .grid2 { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; }
    .grid3 { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 12px; }
    .grid5 { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 12px; }
    .metric-grid { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 10px; margin-bottom: 12px; }
    .dashboard-current-panel {
      margin-bottom: 12px;
    }
    .card { padding: 13px; min-width: 0; }
    .card, .metric, .setup-action-card, .drop-card, .operation-banner, .active-dataset-panel, .dashboard-audit-details, .focus-panel, .graph-canvas, .note {
      box-shadow: none;
    }
    .metric {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .metric-value { display: block; margin-bottom: 3px; font-size: 22px; font-weight: 800; color: var(--text); }
    .metric-label { color: var(--muted); font-size: 12px; }
    .choice-card { cursor: pointer; }
    .choice-card:hover { border-color: rgba(15, 118, 110, 0.5); background: #fbfffe; }
    .choice-card.active { border-color: var(--accent); background: #eef8f6; }
    .setup-actions {
      display: grid;
      grid-template-columns: 1fr;
      gap: 10px;
    }
    .setup-action-card {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 14px;
      align-items: center;
      min-height: 82px;
      padding: 16px;
      border: 1px solid rgba(15, 118, 110, 0.34);
      border-radius: 8px;
      background: #f8fcfb;
    }
    .setup-action-card h3 {
      margin-bottom: 5px;
      font-size: 15px;
    }
    .setup-action-card p {
      display: none;
    }
    .demo-inline-action {
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      align-items: center;
      justify-content: space-between;
      padding: 8px 4px 0;
      border: 0;
      border-top: 1px solid #e1e7ef;
      border-radius: 0;
      background: transparent;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .demo-inline-action.active {
      border-color: var(--accent);
      background: #f3fbf9;
      color: var(--text);
    }
    .demo-inline-action strong {
      color: var(--text);
      font-size: 12px;
    }
    .demo-inline-action div {
      color: transparent;
      font-size: 0;
    }
    .demo-inline-action strong {
      color: var(--text);
    }
    .demo-inline-action div::after {
      content: " optional sample";
      color: var(--muted);
      font-size: 12px;
      font-weight: 400;
    }
    .demo-inline-action button {
      min-height: 30px;
      padding: 6px 10px;
      font-size: 12px;
    }
    .upload-panel {
      margin-top: 14px;
      padding-top: 14px;
      border-top: 1px solid var(--line);
    }
    .upload-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(190px, 1fr));
      gap: 10px;
    }
    .drop-card {
      display: grid;
      gap: 7px;
      align-content: start;
      border: 1px dashed #b9c4d3;
      background: #fbfdff;
    }
    .drop-card h3 {
      margin-bottom: 0;
      font-size: 14px;
    }
    .drop-card p {
      display: none;
    }
    .drop-card input { margin-top: 0; }
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
    button.secondary.danger {
      color: var(--danger);
      border-color: #efc4be;
      background: #fff8f7;
    }
    button.secondary.danger:hover { background: #fde9e7; }
    button:disabled { opacity: 0.55; cursor: not-allowed; }
    button.button-pending {
      opacity: 0.85;
      cursor: wait;
      position: relative;
    }
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
    .quiet-helper {
      display: none;
    }
    .operation-banner {
      margin-bottom: 0;
      padding: 2px 4px 0;
      border: 0;
      border-radius: 0;
      background: transparent;
    }
    .operation-banner.busy {
      background: transparent;
    }
    .operation-banner.fail {
      background: transparent;
      color: var(--danger);
    }
    .operation-banner.done {
      background: transparent;
    }
    .next-action-panel {
      display: grid;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 14px;
      align-items: center;
      margin-bottom: 12px;
      padding: 14px;
      border: 1px solid #a7d8cf;
      border-left: 5px solid var(--accent);
      border-radius: 8px;
      background: #f2fbf9;
    }
    .command-center-grid {
      display: grid;
      grid-template-columns: minmax(280px, 0.9fr) minmax(320px, 1.1fr);
      gap: 12px;
      align-items: stretch;
    }
    .command-action-stack {
      display: grid;
      gap: 12px;
      align-content: start;
    }
    .next-action-panel.warn {
      border-color: #f0d19b;
      border-left-color: #c98200;
      background: #fff8ea;
    }
    .next-action-panel.fail {
      border-color: #efc4be;
      border-left-color: var(--danger);
      background: #fff8f7;
    }
    .next-action-panel.done {
      border-color: #b8dfc9;
      border-left-color: var(--ok);
      background: #f2fbf5;
    }
    .next-action-eyebrow {
      display: block;
      margin-bottom: 4px;
      color: var(--muted);
      font-size: 11px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .next-action-title {
      margin-bottom: 4px;
      color: var(--text);
      font-size: 18px;
      font-weight: 800;
      line-height: 1.25;
    }
    .next-action-detail {
      display: none;
    }
    .next-action-buttons {
      display: flex;
      flex-wrap: wrap;
      justify-content: flex-end;
      gap: 8px;
      min-width: 210px;
    }
    .next-action-buttons button {
      white-space: nowrap;
    }
    .sticky-action-bar {
      position: sticky;
      bottom: 0;
      z-index: 20;
      display: none;
      grid-template-columns: minmax(0, 1fr) auto;
      gap: 12px;
      align-items: center;
      margin-top: 14px;
      padding: 10px 12px;
      border: 1px solid #a7d8cf;
      border-radius: 8px 8px 0 0;
      background: rgba(255, 255, 255, 0.96);
      box-shadow: 0 -8px 24px rgba(15, 23, 42, 0.08);
      backdrop-filter: blur(8px);
    }
    .sticky-action-bar.warn {
      border-color: #f0d19b;
      background: rgba(255, 248, 234, 0.96);
    }
    .sticky-action-bar.fail {
      border-color: #efc4be;
      background: rgba(255, 248, 247, 0.96);
    }
    .sticky-action-bar.done {
      border-color: #b8dfc9;
      background: rgba(242, 251, 245, 0.96);
    }
    .sticky-action-title {
      display: block;
      color: var(--text);
      font-size: 13px;
      font-weight: 800;
      line-height: 1.3;
      overflow-wrap: anywhere;
    }
    .sticky-action-detail {
      margin-top: 2px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
      overflow-wrap: anywhere;
    }
    .sticky-action-buttons {
      display: flex;
      flex-wrap: wrap;
      justify-content: flex-end;
      gap: 8px;
      min-width: 190px;
    }
    .sticky-action-buttons button {
      white-space: nowrap;
    }
    .active-dataset-panel, .dashboard-audit-details {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .active-dataset-panel {
      min-height: 100%;
      border-color: rgba(15, 118, 110, 0.34);
      background: #f8fcfb;
    }
    .active-dataset-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 8px;
    }
    .active-dataset-title {
      display: block;
      margin: 2px 0 3px;
      font-size: 18px;
      font-weight: 800;
    }
    .active-dataset-body {
      display: grid;
      gap: 9px;
    }
    .active-dataset-summary {
      display: grid;
      gap: 8px;
      padding-top: 2px;
    }
    .active-dataset-summary-item {
      display: grid;
      grid-template-columns: 96px minmax(0, 1fr);
      gap: 9px;
      align-items: start;
      padding: 0 0 7px;
      border-bottom: 1px solid #e5ebf2;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
    }
    .active-dataset-summary-item:last-child {
      border-bottom: 0;
      padding-bottom: 0;
    }
    .active-dataset-summary-item strong {
      color: var(--text);
      font-size: 12px;
    }
    .active-dataset-summary-item.warn span { color: #6f4700; }
    .active-dataset-summary-item.fail span { color: var(--danger); }
    .active-dataset-row {
      display: grid;
      grid-template-columns: 112px minmax(0, 1fr);
      gap: 9px;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      font-size: 13px;
      line-height: 1.45;
    }
    .active-dataset-row strong {
      color: var(--text);
      font-size: 12px;
    }
    .active-dataset-row.warn {
      border-color: #f0d19b;
      background: #fff8ea;
    }
    .active-dataset-row.fail {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .active-stage-strip {
      display: grid;
      grid-template-columns: repeat(5, minmax(0, 1fr));
      gap: 6px;
    }
    .focus-panel {
      display: grid;
      gap: 12px;
      padding: 14px;
      border: 1px solid rgba(15, 118, 110, 0.24);
      border-radius: 8px;
      background: #fbfffe;
    }
    .focus-panel .active-dataset-panel {
      padding: 0;
      border: 0;
      background: transparent;
    }
    .compact-metrics {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 8px;
    }
    .compact-metrics .metric {
      min-height: 62px;
      padding: 9px;
      background: #fff;
    }
    .compact-metrics .metric-value {
      font-size: 18px;
    }
    .operation-head {
      display: none;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 6px;
    }
    .operation-title {
      display: block;
      margin-bottom: 2px;
      font-size: 14px;
      font-weight: 800;
    }
    .progress-track {
      height: 6px;
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
    .review-queue-actions {
      margin-top: 5px;
      color: var(--muted);
      font-size: 11px;
      font-weight: 700;
    }
    .review-queue-command-actions {
      display: flex;
      flex-wrap: wrap;
      gap: 6px;
      margin-top: 7px;
    }
    .review-queue-command-actions button {
      min-height: 30px;
      padding: 6px 10px;
      font-size: 11px;
    }
    .study-loop-panel {
      margin: 0 0 12px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .study-loop-head {
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 12px;
      margin-bottom: 9px;
    }
    .study-loop-title {
      display: block;
      margin: 2px 0 3px;
      font-size: 15px;
      font-weight: 800;
    }
    .study-loop-list {
      display: grid;
      gap: 7px;
    }
    .study-loop-item {
      display: grid;
      grid-template-columns: minmax(86px, 0.35fr) 1fr;
      gap: 10px;
      padding: 9px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
    }
    .study-loop-item.warn { border-color: #f0d19b; background: #fff8ea; }
    .study-loop-item.fail { border-color: #efc4be; background: #fff8f7; }
    .study-loop-item.info { border-color: rgba(15, 118, 110, 0.24); background: #f4fbfa; }
    .study-loop-target {
      color: var(--text);
      font-size: 13px;
      font-weight: 800;
    }
    .study-loop-action {
      color: var(--text);
      font-weight: 800;
    }
    .graph-canvas {
      min-height: 180px;
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .panel-kicker {
      margin: -4px 0 9px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
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
    .dependency-plain {
      display: grid;
      gap: 10px;
    }
    .dependency-plain-card {
      padding: 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
    }
    .dependency-plain-card.active {
      border-color: rgba(15, 118, 110, 0.42);
      background: #fbfffe;
    }
    .dependency-plain-card.blocked {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .dependency-plain-card.waiting {
      border-color: #f0d19b;
      background: #fff8ea;
    }
    .dependency-plain-head {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      margin-bottom: 8px;
    }
    .dependency-plain-title {
      font-size: 15px;
      font-weight: 800;
    }
    .dependency-plain-body {
      display: grid;
      gap: 7px;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.45;
    }
    .dependency-plain-row {
      display: grid;
      grid-template-columns: 110px minmax(0, 1fr);
      gap: 8px;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
    }
    .dependency-plain-row strong {
      color: var(--text);
      font-size: 12px;
    }
    .dependency-plain-row.primary {
      border-color: rgba(15, 118, 110, 0.28);
      background: #f2fbf9;
    }
    .dependency-plain-row.blocked {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .trust-boundary {
      padding: 9px 10px;
      border: 1px solid #f0d19b;
      border-radius: 7px;
      background: #fff8ea;
      color: #6f4700;
      font-size: 12px;
      line-height: 1.45;
    }
    .dataset-board { display: grid; gap: 8px; }
    .dataset-lane {
      display: grid;
      gap: 5px;
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .dataset-lane-head {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      color: var(--muted);
      font-size: 10px;
      font-weight: 800;
      text-transform: uppercase;
      letter-spacing: 0;
    }
    .dataset-lane-head span:last-child {
      color: var(--text);
      font-size: 11px;
      text-transform: none;
    }
    .dataset-lane .muted {
      font-size: 11px;
      line-height: 1.25;
    }
    .dataset-lane.review { border-color: #f0d19b; background: #fffdf7; }
    .dataset-lane.waiting { border-color: #f0d19b; background: #fff8ea; }
    .dataset-lane.blocked { border-color: #efc4be; background: #fff8f7; }
    .dataset-lane.done { border-color: #b8dfc9; background: #f2fbf5; }
    .dataset-card {
      padding: 8px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
      cursor: pointer;
    }
    .dataset-card:hover { border-color: rgba(15, 118, 110, 0.35); background: #fbfffe; }
    .dataset-card.active { border-color: rgba(15, 118, 110, 0.45); background: #fbfffe; }
    .dataset-card.blocked { border-color: #efc4be; background: #fff8f7; }
    .dataset-card.waiting { border-color: #f0d19b; background: #fff8ea; }
    .dataset-top {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
      margin-bottom: 4px;
    }
    .dataset-name { font-size: 13px; font-weight: 800; }
    .dataset-context {
      color: var(--muted);
      font-size: 11px;
      line-height: 1.3;
    }
    .dataset-next {
      margin: 4px 0 0;
      color: var(--text);
      font-size: 11px;
      line-height: 1.3;
    }
    .dataset-next strong {
      font-weight: 800;
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
    .stage.waiting { color: var(--warn); background: #fff8ea; border-color: #f0d19b; }
    .stage.review-only { color: #8a5b00; background: #fff6dc; border-color: #e4c36b; }
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
    .agent-trace-list {
      display: grid;
      gap: 7px;
      margin-bottom: 9px;
    }
    .agent-trace-card {
      display: grid;
      grid-template-columns: 130px 1fr 150px;
      gap: 8px;
      align-items: start;
      padding: 9px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fff;
      font-size: 12px;
      line-height: 1.4;
    }
    .agent-trace-card.warn { border-color: #f0d19b; background: #fff8ea; }
    .agent-trace-card.fail { border-color: #efc4be; background: #fff8f7; }
    .agent-trace-card strong {
      display: block;
      margin-bottom: 2px;
      color: var(--text);
      font-size: 12px;
    }
    .agent-trace-card span { color: var(--muted); overflow-wrap: anywhere; }
    .dashboard-audit-details {
      margin-top: 12px;
    }
    .dashboard-audit-details summary {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      color: var(--text);
    }
    .dashboard-audit-details summary span {
      color: var(--muted);
      font-size: 12px;
      font-weight: 400;
    }
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
    .pill.info { color: var(--accent-dark); background: #e6f3f2; }
    .note {
      margin: 0 0 12px;
      padding: 10px 12px;
      border: 0;
      border-left: 3px solid #d8dee8;
      border-radius: 8px;
      background: #fbfdff;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.45;
    }
    .note.strong {
      color: var(--text);
      border-color: rgba(15, 118, 110, 0.42);
      background: #eef8f6;
    }
    .note.warn {
      border: 1px solid #f0d19b;
      border-left: 4px solid #c98200;
      background: #fff8ea;
      color: #6d4200;
    }
    .evidence-summary-table {
      overflow: auto;
      border: 1px solid #dfe6ef;
      border-radius: 8px;
      background: #fff;
    }
    .evidence-summary-table table {
      min-width: 640px;
    }
    .evidence-summary-table td,
    .evidence-summary-table th {
      padding: 9px 10px;
    }
    .evidence-summary-role {
      color: var(--text);
      font-weight: 800;
    }
    .evidence-summary-status {
      display: inline-flex;
      align-items: center;
      min-height: 22px;
      padding: 2px 8px;
      border-radius: 999px;
      background: #e8f6ee;
      color: var(--ok);
      font-size: 12px;
      font-weight: 800;
      white-space: nowrap;
    }
    .evidence-summary-status.warn {
      background: #fff4df;
      color: var(--warn);
    }
    .evidence-summary-status.optional {
      background: #eef3f8;
      color: var(--muted);
    }
    .evidence-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 12px;
    }
    .evidence-grid.compact {
      grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
      align-items: start;
    }
    .evidence-grid.compact .evidence-card {
      display: grid;
      grid-template-columns: 1fr;
      gap: 9px;
      align-items: start;
    }
    .evidence-grid.compact .evidence-card h3 {
      margin-bottom: 0;
    }
    .input-profile {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
      gap: 8px;
      margin-bottom: 10px;
    }
    .input-profile-card {
      min-height: 58px;
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fff;
    }
    .input-profile-card strong {
      display: block;
      color: var(--text);
      font-size: 16px;
      line-height: 1.2;
    }
    .input-profile-card span {
      display: block;
      margin-top: 2px;
      color: var(--muted);
      font-size: 11px;
      line-height: 1.25;
    }
    .input-profile-card.warn {
      border-color: #f0d19b;
      background: #fff8ea;
    }
    .input-profile-card.reference {
      border-color: #f0d19b;
      background: #fffdf7;
    }
    .evidence-details {
      margin-top: 8px;
      padding: 10px 12px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .evidence-details summary {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
      color: var(--text);
      font-size: 13px;
      font-weight: 800;
    }
    .evidence-details summary span {
      color: var(--muted);
      font-size: 12px;
      font-weight: 600;
    }
    .evidence-details .evidence-grid {
      margin-top: 10px;
    }
    .evidence-card {
      padding: 13px;
      min-width: 0;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: var(--panel);
    }
    .evidence-card h3 {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 8px;
    }
    .file-list { display: grid; gap: 8px; }
    .file-item {
      padding: 9px 10px;
      border: 1px solid var(--line);
      border-radius: 7px;
      background: #fbfdff;
    }
    .file-item.reference-evidence {
      border-color: #f0d19b;
      background: #fffdf7;
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
    .file-actions {
      display: flex;
      justify-content: flex-end;
      margin-top: 8px;
    }
    .file-actions button {
      padding: 6px 10px;
      font-size: 12px;
    }
    .optional-evidence-note {
      margin-top: 10px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.4;
    }
    .field-help {
      margin-top: 4px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.35;
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
    .compare-verdict {
      margin-bottom: 12px;
      padding: 12px;
      border: 1px solid #b8dfc9;
      border-radius: 8px;
      background: #f2fbf5;
      color: var(--text);
    }
    .compare-verdict.warn {
      border-color: #f0d19b;
      background: #fff8ea;
    }
    .compare-verdict.fail {
      border-color: #efc4be;
      background: #fff8f7;
    }
    .compare-verdict strong {
      display: block;
      margin-bottom: 3px;
      font-size: 14px;
    }
    .compare-verdict span {
      display: block;
      color: var(--muted);
      font-size: 13px;
      line-height: 1.45;
    }
    .modal-backdrop {
      position: fixed;
      inset: 0;
      z-index: 80;
      display: grid;
      place-items: center;
      padding: 18px;
      background: rgba(15, 23, 42, 0.38);
    }
    .settings-modal {
      width: min(920px, 100%);
      max-height: min(760px, calc(100vh - 36px));
      overflow: auto;
      border: 1px solid #c8d2df;
      border-radius: 8px;
      background: #fff;
      box-shadow: 0 22px 60px rgba(15, 23, 42, 0.28);
    }
    .settings-modal-head {
      position: sticky;
      top: 0;
      z-index: 1;
      display: flex;
      align-items: flex-start;
      justify-content: space-between;
      gap: 14px;
      padding: 16px 18px;
      border-bottom: 1px solid #cfd7e3;
      background: #f8fafc;
    }
    .settings-modal-head h2 {
      margin: 0 0 4px;
      font-size: 18px;
    }
    .settings-modal-body {
      display: grid;
      gap: 14px;
      padding: 18px;
    }
    .settings-group {
      padding: 14px;
      border: 1px solid #d5dde8;
      border-radius: 8px;
      background: #fbfdff;
    }
    .settings-group h3 {
      margin-bottom: 10px;
      padding-left: 9px;
      border-left: 3px solid var(--accent);
      font-size: 14px;
    }
    .compare-mini-grid {
      display: grid;
      grid-template-columns: repeat(4, minmax(0, 1fr));
      gap: 10px;
      margin-bottom: 12px;
    }
    .compare-mini {
      min-height: 70px;
      padding: 10px;
      border: 1px solid var(--line);
      border-radius: 8px;
      background: #fbfdff;
    }
    .compare-mini strong {
      display: block;
      color: var(--text);
      font-size: 20px;
      line-height: 1.2;
    }
    .compare-mini span {
      display: block;
      margin-top: 4px;
      color: var(--muted);
      font-size: 12px;
      line-height: 1.3;
    }
    ul.clean { margin: 0; padding-left: 18px; color: var(--muted); font-size: 13px; line-height: 1.5; }
    details { margin-top: 12px; }
    summary { cursor: pointer; font-weight: 700; font-size: 13px; }
    .hidden { display: none !important; }
    @media (max-width: 1120px) {
      main { grid-template-columns: 1fr; }
      aside, .left-rail, .inspector-rail {
        position: static;
        max-height: none;
        overflow: visible;
      }
      .command-center-grid { grid-template-columns: 1fr; }
      .grid3, .grid5, .metric-grid, .agent-audit-grid, .agent-trace-card, .dependency-summary { grid-template-columns: 1fr; }
      .upload-grid { grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); }
      .side-queue-panel .dataset-board { max-height: none; overflow: visible; }
    }
    @media (max-width: 760px) {
      header { align-items: flex-start; flex-direction: column; }
      .header-status { min-width: 0; width: 100%; max-width: none; }
      .grid2 { grid-template-columns: 1fr; }
      .setup-actions { grid-template-columns: 1fr; }
      .setup-action-card { grid-template-columns: 1fr; }
      .upload-grid { grid-template-columns: 1fr; }
      .evidence-grid.compact .evidence-card { grid-template-columns: 1fr; }
      .compact-metrics { grid-template-columns: repeat(2, minmax(0, 1fr)); }
      .next-action-panel { grid-template-columns: 1fr; }
      .next-action-buttons { justify-content: flex-start; min-width: 0; }
      .sticky-action-bar { grid-template-columns: 1fr; }
      .sticky-action-buttons { justify-content: flex-start; min-width: 0; }
      .review-queue-item { grid-template-columns: 1fr; }
      .study-loop-item { grid-template-columns: 1fr; }
      .status-meta-grid { grid-template-columns: 1fr; }
      .compare-mini-grid { grid-template-columns: 1fr; }
      .dependency-plain-row { grid-template-columns: 1fr; }
    }
  </style>
</head>
<body>
  <header>
    <div class="brand-block">
      <h1>ADaM Agent Studio</h1>
      <div class="header-actions">
        <button class="secondary llm-config-button" id="openLlmSettingsButton" type="button" data-i18n="llmConfig">LLM Config</button>
        <span class="llm-mode-chip" id="llmModeChip">Mock LLM</span>
        <span class="language-toggle" aria-label="Language">
          <button type="button" data-lang-option="zh">中文</button>
          <button type="button" data-lang-option="en">EN</button>
        </span>
      </div>
    </div>
    <div class="header-status">
      <div class="status-card">
        <div class="status-row">
          <span class="status-label" data-i18n="currentStatus">Current Status</span>
          <span class="pill warn" id="health">Checking API...</span>
        </div>
        <div class="status-detail" id="globalStatusDetail">Waiting for the local API health check.</div>
        <div class="status-meta-grid" id="headerStatusGrid">
          <div class="status-chip header-now-chip" aria-hidden="true"><span data-i18n="now">Now</span><strong id="headerOperation">Idle</strong></div>
          <div class="status-chip"><span data-i18n="study">Study</span><strong id="headerStudy">Not loaded</strong></div>
          <div class="status-chip"><span data-i18n="target">Target</span><strong id="headerTarget">None</strong></div>
          <div class="status-chip header-next-chip" aria-hidden="true"><span data-i18n="next">Next</span><strong id="headerNextAction">Setup</strong></div>
        </div>
        <div class="header-progress-track"><div class="header-progress-bar" id="headerOperationProgress"></div></div>
      </div>
    </div>
  </header>

  <main id="appMain">
    <aside class="left-rail">
      <div class="side-workflow-panel" id="sideWorkflowPanel">
        <div class="side-workflow-title">
          <span class="status-label" data-i18n="workflow">Workflow</span>
          <span class="pill warn" id="sideWorkflowStatus">setup</span>
        </div>
        <span class="side-workflow-target" id="sideWorkflowTarget">No study loaded</span>
        <div class="side-workflow-detail" id="sideWorkflowDetail"></div>
        <div class="side-progress-steps" id="sideWorkflowSteps"></div>
      </div>
      <div class="side-queue-panel hidden" id="sideDatasetQueuePanel">
        <div class="side-queue-title">
          <span class="status-label" data-i18n="datasetQueue">Dataset Queue</span>
          <span class="pill" id="sideDatasetQueueStatus">empty</span>
        </div>
        <div id="datasetBoard" class="dataset-board"><div class="muted">No dataset selected yet.</div></div>
      </div>
      <div class="side-review-panel">
        <div class="review-queue-panel hidden" id="humanReviewQueuePanel">
          <div class="review-queue-head">
            <div>
              <span class="status-label" data-i18n="humanReviewQueue">Human Review Queue</span>
              <span class="review-queue-title" id="humanReviewQueueTitle">No open review gate</span>
              <div class="muted" id="humanReviewQueueDetail">Graph review gates will appear here when the workflow needs a human decision.</div>
            </div>
            <span class="pill" id="humanReviewQueueStatus">clear</span>
          </div>
          <div class="review-queue-list" id="humanReviewQueueList"></div>
        </div>
      </div>
      <div class="legacy-step-anchors" aria-hidden="true">
        <div class="step active" data-step="1"><div class="step-number">1</div><div><strong>Start</strong><p>Upload study evidence. Demo is optional.</p></div></div>
        <div class="step" data-step="2"><div class="step-number">2</div><div><strong>Inputs</strong><p>Check recognized SDTM, specs, and references.</p></div></div>
        <div class="step" data-step="3"><div class="step-number">3</div><div><strong>Target</strong><p>Choose which ADaM dataset to generate.</p></div></div>
        <div class="step" data-step="4"><div class="step-number">4</div><div><strong>Code</strong><p>Generate R without running it.</p></div></div>
        <div class="step" data-step="5"><div class="step-number">5</div><div><strong>Review & Run</strong><p>Approve code, then execute in R sandbox.</p></div></div>
        <div class="step" data-step="6"><div class="step-number">6</div><div><strong>Results</strong><p>Inspect generated ADaM and warnings.</p></div></div>
      </div>
    </aside>

    <div class="workstream">
      <section id="currentWorkSection">
        <div class="section-head">
          <h2 data-i18n="currentWork">Current Work</h2>
          <span class="pill warn" id="graphStatus">waiting</span>
        </div>
        <div class="section-body">
          <div class="note strong hidden" id="dashboardEmptyGuide">
            Start by uploading SDTM source data. Specs are best when available; reference ADaM is only for output review and comparison.
          </div>
          <div class="command-center-grid">
            <div class="active-dataset-panel" id="activeDatasetPanel">
              <div class="active-dataset-head">
                <div>
                  <span class="status-label" data-i18n="currentDataset">Current Dataset</span>
                  <span class="active-dataset-title" id="activeDatasetTitle">No dataset selected</span>
                  <div class="muted" id="activeDatasetSubtitle"></div>
                </div>
                <span class="pill warn" id="activeDatasetStatus">waiting</span>
              </div>
              <div class="active-dataset-body" id="activeDatasetBody">
                <div class="muted">No active dataset yet.</div>
              </div>
            </div>
            <div class="command-action-stack">
              <div class="next-action-panel" id="primaryNextActionPanel">
                <div>
                  <span class="next-action-eyebrow" data-i18n="nextStep">Next Step</span>
                  <div class="next-action-title" id="primaryNextActionTitle">Start by uploading your study files</div>
                  <div class="next-action-detail" id="primaryNextActionDetail"></div>
                </div>
                <div class="next-action-buttons" id="primaryNextActionButtons">
                  <button data-primary-action="startUpload" type="button">Start Upload</button>
                </div>
              </div>
              <div class="operation-banner" id="operationBanner">
                <div class="operation-head">
                  <div>
                    <span class="operation-title" id="operationTitle">Ready for study setup</span>
                    <div class="muted" id="operationDetail"></div>
                  </div>
                  <span class="pill" id="operationStatus">idle</span>
                </div>
                <div class="progress-track"><div class="progress-bar" id="operationProgress"></div></div>
              </div>
            </div>
          </div>
          <div id="dashboardRuntimePanels" class="hidden">
            <div class="dashboard-current-panel focus-panel hidden" id="dashboardCurrentPanel" aria-hidden="true"></div>
            <div class="metric-grid compact-metrics hidden" id="metricGrid">
              <div class="metric"><span class="metric-value" id="metricInputs">0</span><span class="metric-label">input files</span></div>
              <div class="metric"><span class="metric-value" id="metricTargets">0</span><span class="metric-label">ADaM targets</span></div>
              <div class="metric"><span class="metric-value" id="metricRunnable">0</span><span class="metric-label">runnable now</span></div>
              <div class="metric"><span class="metric-value" id="metricBlocked">0</span><span class="metric-label">blocked</span></div>
            </div>
          </div>
          <span class="sr-only">Study Dashboard</span>
        </div>
      </section>

      <section id="studySetupSection" class="hidden">
        <div class="section-head">
          <h2 data-i18n="startStudy">Start A Study</h2>
          <span class="pill warn" id="workspaceStatus">not started</span>
        </div>
        <div class="section-body">
          <div class="setup-actions">
            <div class="setup-action-card choice-card" id="uploadChoice">
              <div>
              <h3 data-i18n="uploadEvidence">Upload Study Evidence</h3>
              <p class="muted"></p>
              </div>
              <button id="startUploadButton" data-i18n="startUpload">Start Upload</button>
            </div>
            <div class="demo-inline-action" id="demoChoice">
              <div><strong>Want to explore first?</strong> Load a small Shiny sample study to see the workflow. This is not the normal production entry.</div>
              <button class="secondary" id="createDemoButton" data-i18n="tryDemo">Try Sample Demo</button>
            </div>
          </div>
          <p class="note hidden" id="workspaceMessage"></p>

          <div id="uploadPanel" class="upload-panel hidden">
            <div class="upload-grid">
              <div class="card drop-card">
                <h3 data-i18n="sdtmSource">SDTM Source Data</h3>
                <p class="muted">Required runtime input. CSV and SAS7BDAT can be recognized.</p>
                <input id="uploadSdtm" type="file" multiple>
                <button data-upload-role="sdtm" data-i18n="uploadSdtm">Upload SDTM</button>
                <div class="file-meta" id="uploadStatusSdtm"></div>
              </div>
              <div class="card drop-card">
                <h3 data-i18n="adamSpecs">ADaM Specs</h3>
                <p class="muted">Best source for derivation instructions. If missing, a draft spec review gate is required.</p>
                <input id="uploadSpec" type="file" multiple>
                <button data-upload-role="spec" data-i18n="uploadSpecs">Upload Specs</button>
                <div class="file-meta" id="uploadStatusSpec"></div>
              </div>
              <div class="card drop-card">
                <h3 data-i18n="referenceAdam">Reference ADaM</h3>
                <p class="muted">Existing ADaM for compare/output-shape evidence only.</p>
                <input id="uploadReference" type="file" multiple>
                <button data-upload-role="reference" data-i18n="uploadReference">Upload Reference ADaM</button>
                <div class="file-meta" id="uploadStatusReference"></div>
              </div>
              <div class="card drop-card">
                <h3 data-i18n="define">Define</h3>
                <p class="muted">define.xml or related metadata evidence.</p>
                <input id="uploadDefine" type="file" multiple>
                <button data-upload-role="define" data-i18n="uploadDefine">Upload Define</button>
                <div class="file-meta" id="uploadStatusDefine"></div>
              </div>
              <div class="card drop-card">
                <h3 data-i18n="legacyCode">Legacy Code</h3>
                <p class="muted">SAS/R programs used as lineage evidence.</p>
                <input id="uploadLegacy" type="file" multiple>
                <button data-upload-role="legacy" data-i18n="uploadLegacy">Upload Legacy Code</button>
                <div class="file-meta" id="uploadStatusLegacy"></div>
              </div>
            </div>
            <p class="optional-evidence-note hidden">Reference ADaM is used for compare/output-shape evidence only. It is not derivation authority and does not override user specs.</p>
          </div>
        </div>
      </section>

      <section id="recognizedEvidenceSection" class="hidden">
        <div class="section-head">
          <h2 data-i18n="recognizedEvidence">Recognized Evidence</h2>
          <span class="muted" id="inputSummaryLine">Nothing scanned yet.</span>
        </div>
        <div class="section-body">
          <div id="inputProfile" class="input-profile">
            <div class="note">No evidence yet.</div>
          </div>
          <div id="inputEvidenceNotes" class="note hidden" style="margin-top: 12px;"></div>
          <div id="inputWarnings" class="note hidden" style="margin-top: 12px;"></div>
          <details class="evidence-details" id="evidenceDetails">
            <summary><span data-i18n="showRecognizedFiles">Show recognized files</span> <span id="evidenceDetailsSummary">No files yet.</span></summary>
            <div id="evidenceCards" class="evidence-grid compact">
              <div class="note">No study evidence recognized yet.</div>
            </div>
          </details>
        </div>
      </section>

      <section id="chooseOutputSection" class="hidden">
        <div class="section-head">
          <h2 data-i18n="chooseOutput">Choose Output</h2>
          <span id="planStatus" class="pill warn">waiting</span>
        </div>
        <div class="section-body">
          <p class="note quiet-helper">Select one or more ADaM datasets to plan together. The active dataset is the one shown in the review/code panel; study-level start can move all runnable datasets to their review gates without running R.</p>
          <div class="button-row" id="targetButtons"></div>
          <div id="targetSelectionSummary" class="target-selection-summary">No target selected.</div>
          <div class="grid2" style="margin-top:12px;">
            <div class="field">
              <label for="manualTarget" data-i18n="addTarget">Add another ADaM target</label>
              <input id="manualTarget" placeholder="Example: ADLB, ADCM, ADSL">
            </div>
            <div class="field">
              <label>&nbsp;</label>
              <button class="secondary" id="addTargetButton" data-i18n="addTargetButton">Add Target</button>
            </div>
          </div>
          <div id="planView" class="note">Load inputs first, then choose a target.</div>
          <div class="button-row">
            <button class="secondary" id="finalizeInputsButton" disabled data-i18n="finalizeInputs">Finalize Inputs / Draft Spec</button>
            <button class="secondary" id="startStudyLoopButton" disabled data-i18n="startRunnable">Start Runnable Datasets</button>
          </div>
          <div id="specActionHints" class="action-hints"></div>
          <div id="draftSpecPane" class="note">Finalize inputs after upload. If no approved spec is present, the app will generate a draft spec for review.</div>
          <div class="button-row">
            <button class="secondary" id="approveDraftSpecButton" disabled data-i18n="approveDraftSpec">Approve Draft Spec</button>
          </div>
        </div>
      </section>

      <section id="generateRunSection" class="hidden">
        <div class="section-head">
          <h2 data-i18n="generateReviewRun">Generate, Review, Run</h2>
          <span id="codeStatus" class="pill warn">not generated</span>
        </div>
        <div class="section-body">
          <div class="button-row">
            <button id="generateCodeButton" disabled data-i18n="generateRCode">Generate R Code</button>
            <button id="approveButton" disabled data-i18n="approveCode">Approve Code</button>
            <button id="runApprovedButton" disabled data-i18n="runApprovedCode">Run Approved Code</button>
          </div>
          <div id="generationActionHints" class="action-hints"></div>
          <p class="note">Generation creates R code only. Running happens after approval, using the local R sandbox.</p>
          <div class="tabs">
            <button class="tab active" data-view="summary" data-i18n="summary">Summary</button>
            <button class="tab" data-view="code" data-i18n="rCode">R Code</button>
            <button class="tab" data-view="risk" data-i18n="risks">Assumptions & Risks</button>
            <button class="tab" data-view="output" data-i18n="generatedAdam">Generated ADaM</button>
            <button class="tab" data-view="timeline" data-i18n="auditTimeline">Audit Timeline</button>
          </div>
          <div id="reviewPane"><p class="note">No code generated yet.</p></div>
          <details>
            <summary data-i18n="advancedSetup">Advanced setup and audit files (usually not needed)</summary>
            <p class="note">Use this panel only when troubleshooting local R or inspecting audit file locations. LLM provider settings are in the top LLM Config panel.</p>
            <div class="grid3" style="margin-top:12px;">
              <div class="field">
                <label for="studyDir">Study folder</label>
                <input id="studyDir">
                <div class="field-help">Backend workspace. The app creates this automatically.</div>
              </div>
              <div class="field">
                <label for="runId">Technical run id</label>
                <input id="runId">
                <div class="field-help">Audit identifier for this attempt. Usually leave unchanged.</div>
              </div>
              <div class="field">
                <label for="configPath">Pipeline config file</label>
                <input id="configPath" value="studies\\_template\\configs\\mock_downstream.json">
                <div class="field-help">Developer fallback config. The top LLM Config panel controls model calls.</div>
              </div>
              <div class="field">
                <label for="rscriptPath">Local Rscript executable</label>
                <input id="rscriptPath" value="C:\\Dev\\R-4.5.2\\bin\\Rscript.exe">
                <div class="field-help">Used only when running approved R locally.</div>
              </div>
              <div class="field">
                <label for="reviewer">Reviewer</label>
                <input id="reviewer" value="local_user">
                <div class="field-help">Name recorded on approvals.</div>
              </div>
              <div class="field">
                <label for="reviewNotes">Review notes</label>
                <input id="reviewNotes" value="Approved for local sandbox execution.">
                <div class="field-help">Short approval note saved to the audit trail.</div>
              </div>
            </div>
            <div id="advancedPane" class="note">Audit artifacts appear after a run.</div>
          </details>
        </div>
      </section>
      <div class="sticky-action-bar" id="stickyNextActionBar">
        <div>
          <span class="status-label" data-i18n="nextStep">Next Step</span>
          <span class="sticky-action-title" id="stickyNextActionTitle">Start by uploading your study files</span>
          <div class="sticky-action-detail" id="stickyNextActionDetail"></div>
        </div>
        <div class="sticky-action-buttons" id="stickyNextActionButtons">
          <button data-primary-action="startUpload" type="button" data-i18n="startUpload">Start Upload</button>
        </div>
      </div>
    </div>

    <aside class="inspector-rail hidden" id="inspectorRail">
      <div class="inspector-title">
        <div>
          <span class="status-label" data-i18n="runContext">Run Context</span>
          <strong data-i18n="studyDashboard">Study Dashboard</strong>
        </div>
        <span class="pill warn" id="inspectorStatus">waiting</span>
      </div>
      <div class="inspector-empty" id="inspectorEmptyGuide">
        <strong>No dataset selected</strong>
        Waiting for a target.
      </div>
      <div id="dependencyPanel" class="hidden">
        <h3>Why this dataset is waiting</h3>
        <div class="panel-kicker">Plain-language dependency explanation from the current graph plan. Reference ADaM is comparison evidence only.</div>
        <div id="dependencyGraph" class="graph-canvas"><div class="muted">Load inputs to build the study graph.</div></div>
      </div>
      <details class="dashboard-audit-details hidden" id="advancedRunAudit">
        <summary>Advanced run audit <span>Study loop status, agent decisions, graph traces, and risk flags</span></summary>
        <div class="study-loop-panel" id="studyLoopResultPanel">
          <div class="study-loop-head">
            <div>
              <span class="status-label">Study Loop Result</span>
              <span class="study-loop-title" id="studyLoopResultTitle">No batch start yet</span>
              <div class="muted" id="studyLoopResultDetail">Start Runnable Datasets will show which datasets moved to review gates and which stayed blocked.</div>
            </div>
            <span class="pill" id="studyLoopResultStatus">idle</span>
          </div>
          <div class="study-loop-list" id="studyLoopResultList"></div>
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
          <div class="agent-trace-list" id="agentNodeTrace"></div>
          <div class="note" id="agentAuditRiskNote">No agent risk flags yet.</div>
        </div>
      </details>
    </aside>
  </main>

  <div class="modal-backdrop hidden" id="llmSettingsModal" role="dialog" aria-modal="true" aria-labelledby="llmSettingsTitle">
    <div class="settings-modal">
      <div class="settings-modal-head">
        <div>
          <h2 id="llmSettingsTitle">LLM Configuration</h2>
          <div class="muted">Change provider settings only when you are ready to use a real model or test a relay endpoint.</div>
        </div>
        <button class="secondary" id="closeLlmSettingsButton" type="button">Close</button>
      </div>
      <div class="settings-modal-body">
        <div class="settings-group">
          <h3>Model Access</h3>
          <div class="grid3">
            <div class="field">
              <label for="modelMode">Model mode</label>
              <select id="modelMode">
                <option value="mock">Mock / offline</option>
                <option value="real">Real LLM API</option>
              </select>
              <div class="field-help">Mock is for UI/pipeline checks; Real calls your selected API.</div>
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
              <div class="field-help">Choose OpenAI-compatible for local relays or API gateways.</div>
            </div>
            <div class="field">
              <label for="llmModel">Model</label>
              <input id="llmModel" value="gpt-5.5">
              <div class="field-help">Model name sent to the selected provider.</div>
            </div>
          </div>
          <div class="grid3">
            <div class="field">
              <label for="llmBaseUrl">Base URL</label>
              <input id="llmBaseUrl" placeholder="Optional, for a relay or compatible endpoint">
              <div class="field-help">Only needed for a relay, local server, or non-default endpoint.</div>
            </div>
            <div class="field">
              <label for="llmApiKey">API key</label>
              <input id="llmApiKey" type="password" placeholder="Used for this browser request only">
              <div class="field-help">Kept in this browser form; not saved as a study artifact.</div>
            </div>
            <div class="field">
              <label>&nbsp;</label>
              <label><input id="llmAllowExternal" type="checkbox"> allow external API for demo data</label>
              <button class="secondary" id="testLlmButton" type="button">Test Connection</button>
              <div class="field-help">Required before sending demo-data context to an external API.</div>
            </div>
          </div>
          <div id="llmStatus" class="note">Mock mode is active. No external LLM call will be made unless Real LLM API is selected.</div>
        </div>
      </div>
    </div>
  </div>

  <script>
    const I18N = {
      en: {
        llmConfig: 'LLM Config',
        currentStatus: 'Current Status',
        now: 'Now',
        study: 'Study',
        target: 'Target',
        next: 'Next',
        workflow: 'Workflow',
        datasetQueue: 'Dataset Queue',
        humanReviewQueue: 'Human Review Queue',
        currentWork: 'Current Work',
        currentDataset: 'Current Dataset',
        nextStep: 'Next Step',
        startStudy: 'Start A Study',
        uploadEvidence: 'Upload Study Evidence',
        startUpload: 'Start Upload',
        tryDemo: 'Try Sample Demo',
        sdtmSource: 'SDTM Source Data',
        uploadSdtm: 'Upload SDTM',
        adamSpecs: 'ADaM Specs',
        uploadSpecs: 'Upload Specs',
        referenceAdam: 'Reference ADaM',
        uploadReference: 'Upload Reference ADaM',
        define: 'Define',
        uploadDefine: 'Upload Define',
        legacyCode: 'Legacy Code',
        uploadLegacy: 'Upload Legacy Code',
        recognizedEvidence: 'Recognized Evidence',
        showRecognizedFiles: 'Show recognized files',
        chooseOutput: 'Choose Output',
        addTarget: 'Add another ADaM target',
        addTargetButton: 'Add Target',
        finalizeInputs: 'Finalize Inputs / Draft Spec',
        startRunnable: 'Start Runnable Datasets',
        approveDraftSpec: 'Approve Draft Spec',
        generateReviewRun: 'Generate, Review, Run',
        generateRCode: 'Generate R Code',
        approveCode: 'Approve Code',
        runApprovedCode: 'Run Approved Code',
        summary: 'Summary',
        rCode: 'R Code',
        risks: 'Assumptions & Risks',
        generatedAdam: 'Generated ADaM',
        auditTimeline: 'Audit Timeline',
        advancedSetup: 'Advanced setup and audit files (usually not needed)',
        runContext: 'Run Context',
        studyDashboard: 'Study Dashboard'
      },
      zh: {
        llmConfig: '模型设置',
        currentStatus: '当前状态',
        now: '当前',
        study: '研究',
        target: '目标',
        next: '下一步',
        workflow: '流程',
        datasetQueue: '数据集队列',
        humanReviewQueue: '人工审核队列',
        currentWork: '当前任务',
        currentDataset: '当前数据集',
        nextStep: '下一步',
        startStudy: '开始研究',
        uploadEvidence: '上传研究材料',
        startUpload: '开始上传',
        tryDemo: '试用示例',
        sdtmSource: 'SDTM 源数据',
        uploadSdtm: '上传 SDTM',
        adamSpecs: 'ADaM Spec',
        uploadSpecs: '上传 Spec',
        referenceAdam: '参考 ADaM',
        uploadReference: '上传参考 ADaM',
        define: 'Define',
        uploadDefine: '上传 Define',
        legacyCode: '历史程序',
        uploadLegacy: '上传历史程序',
        recognizedEvidence: '已识别材料',
        showRecognizedFiles: '查看文件明细',
        chooseOutput: '选择输出',
        addTarget: '添加 ADaM 目标',
        addTargetButton: '添加目标',
        finalizeInputs: '确认输入 / Draft Spec',
        startRunnable: '启动可运行数据集',
        approveDraftSpec: '批准 Draft Spec',
        generateReviewRun: '生成 / 审核 / 运行',
        generateRCode: '生成 R 代码',
        approveCode: '批准代码',
        runApprovedCode: '运行已批准代码',
        summary: '摘要',
        rCode: 'R 代码',
        risks: '假设与风险',
        generatedAdam: '生成的 ADaM',
        auditTimeline: '审计时间线',
        advancedSetup: '高级设置和审计文件（通常不用）',
        runContext: '运行上下文',
        studyDashboard: '研究面板'
      }
    };
    const TEXT_I18N = {
      zh: {
        'Mock LLM': 'Mock 模型',
        'Real LLM': '真实模型',
        'Checking API...': '检查 API...',
        'API ready': 'API 就绪',
        'unavailable': '不可用',
        'Not loaded': '未加载',
        'None': '无',
        'Setup': '设置',
        'setup': '设置',
        'waiting': '等待',
        'not started': '未开始',
        'No study loaded': '未加载研究',
        'Load or upload study evidence.': '加载或上传研究材料。',
        'No dataset selected': '未选择数据集',
        'Select an output.': '请选择输出。',
        'Start by uploading your study files': '先上传研究文件',
        'Start Upload': '开始上传',
        'Try Sample Demo': '试用示例',
        'Prepare Plan': '准备计划',
        'Finalize Inputs / Draft Spec': '确认输入 / Draft Spec',
        'Approve Dependency Plan': '批准依赖计划',
        'Reject Dependency Plan': '拒绝依赖计划',
        'Approve Draft Spec': '批准 Draft Spec',
        'Reject Draft Spec': '拒绝 Draft Spec',
        'Start Runnable Datasets': '启动可运行数据集',
        'Generate R Code': '生成 R 代码',
        'Generate Revised Draft Spec': '重新生成 Draft Spec',
        'Approve Code': '批准代码',
        'Run Approved Code': '运行已批准代码',
        'Review Failure': '审核失败',
        'Retry Run': '重试运行',
        'Show Results': '查看结果',
        'Run Compare': '运行对比',
        'Run Compare Again': '再次运行对比',
        'Show Draft Spec': '查看 Draft Spec',
        'Show Review Queue': '查看审核队列',
        'Refresh Progress': '刷新进度',
        'No target selected.': '未选择目标。',
        'No files yet.': '暂无文件。',
        'Nothing scanned yet.': '尚未扫描。',
        'No evidence yet.': '暂无材料。',
        'Waiting for a target.': '等待选择目标。',
        'No dataset selected yet.': '尚未选择数据集。',
        'idle': '空闲',
        'ready': '就绪',
        'review': '审核',
        'blocked': '阻塞',
        'completed': '完成',
        'failed': '失败',
        'not generated': '未生成',
        'Input': '输入',
        'Inputs': '输入',
        'Plan': '计划',
        'Spec': 'Spec',
        'Code Review': '代码审核',
        'Run': '运行',
        'not loaded': '未加载',
        'not prepared': '未准备',
        'choose target': '选择目标',
        'Load or upload study evidence': '加载或上传研究材料'
      }
    };
    const LANGUAGE_STORAGE_KEY = 'adam_agent_studio_language_v1';
    let currentLanguage = readLanguagePreference();

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
      terminalFailureReviewByDataset: {},
      draftSpecByDataset: {},
      draftSpecReviewByDataset: {},
      finalizedInputsByDataset: {},
      runReview: null,
      runReviewDetailLevel: null,
      selectedTarget: null,
      selectedTargetsForPlan: [],
      targetCandidates: [],
      targetEvidenceSources: {},
      lastStudyLoopResult: null,
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
    const fileListRoles = {
      sdtmFiles: 'sdtm',
      specFiles: 'spec',
      referenceFiles: 'reference',
      defineFiles: 'define',
      legacyFiles: 'legacy'
    };
    const byId = (id) => document.getElementById(id);

    function readLanguagePreference() {
      try {
        const value = window.localStorage?.getItem(LANGUAGE_STORAGE_KEY);
        return value === 'en' || value === 'zh' ? value : 'zh';
      } catch {
        return 'zh';
      }
    }

    function setLanguage(lang) {
      currentLanguage = lang === 'en' ? 'en' : 'zh';
      try {
        window.localStorage?.setItem(LANGUAGE_STORAGE_KEY, currentLanguage);
      } catch {
        // Language preference is optional browser state.
      }
      renderGraphAwareDashboard();
      applyI18n();
    }

    function t(key) {
      return I18N[currentLanguage]?.[key] || I18N.en[key] || key;
    }

    function tt(text) {
      const value = String(text ?? '');
      if (currentLanguage === 'en') return value;
      return TEXT_I18N[currentLanguage]?.[value] || value;
    }

    function applyI18n() {
      if (document.documentElement) {
        document.documentElement.lang = currentLanguage === 'zh' ? 'zh-CN' : 'en';
      }
      for (const node of safeQuerySelectorAll('[data-i18n]')) {
        node.textContent = t(node.dataset.i18n);
      }
      for (const button of safeQuerySelectorAll('[data-lang-option]')) {
        if (button.classList?.toggle) {
          button.classList.toggle('active', button.dataset.langOption === currentLanguage);
        }
      }
      translateVisibleText();
    }

    function safeQuerySelectorAll(selector) {
      return document.querySelectorAll ? document.querySelectorAll(selector) : [];
    }

    function translateVisibleText() {
      if (currentLanguage === 'en') return;
      const simpleIds = [
        'llmModeChip',
        'health',
        'headerOperation',
        'headerStudy',
        'headerTarget',
        'headerNextAction',
        'sideWorkflowTarget',
        'sideWorkflowDetail',
        'sideWorkflowStatus',
        'sideWorkflowSteps',
        'graphStatus',
        'activeDatasetTitle',
        'activeDatasetStatus',
        'activeDatasetBody',
        'primaryNextActionTitle',
        'operationStatus',
        'workspaceStatus',
        'inputSummaryLine',
        'evidenceDetailsSummary',
        'targetSelectionSummary',
        'planStatus',
        'codeStatus',
        'inspectorStatus',
        'inspectorEmptyGuide'
      ];
      for (const id of simpleIds) translateNodeText(byId(id));
    }

    function translateNodeText(node) {
      if (!node) return;
      if (!node.children?.length) {
        node.textContent = tt(node.textContent);
        return;
      }
      for (const child of node.childNodes || []) {
        if (child.nodeType === 3) child.textContent = tt(child.textContent);
      }
      for (const child of node.children || []) translateNodeText(child);
    }

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
      if (!node) return;
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
      byId('operationDetail').textContent = detail || '';
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
      byId('globalStatusDetail').textContent = detail || '';
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

    const SESSION_STORAGE_KEY = 'adam_agent_studio_session_v1';

    function saveBrowserSession() {
      if (!window.localStorage) return;
      const payload = {
        study_dir: studyDir(),
        study_id: state.studyId,
        run_id: byId('runId').value.trim(),
        selected_target: state.selectedTarget,
        selected_targets: selectedTargets(),
        config_path: byId('configPath').value.trim(),
        rscript_path: byId('rscriptPath').value.trim(),
      };
      if (!payload.study_dir && !payload.run_id) return;
      try {
        window.localStorage.setItem(SESSION_STORAGE_KEY, JSON.stringify(payload));
      } catch {
        // Browser storage is optional; graph state remains durable on the backend.
      }
    }

    function readBrowserSession() {
      if (!window.localStorage) return null;
      try {
        const raw = window.localStorage.getItem(SESSION_STORAGE_KEY);
        return raw ? JSON.parse(raw) : null;
      } catch {
        return null;
      }
    }

    async function restoreBrowserSession() {
      const saved = readBrowserSession();
      if (!saved?.study_dir || !saved?.run_id) return false;
      byId('studyDir').value = saved.study_dir || '';
      byId('runId').value = saved.run_id || '';
      if (saved.config_path) byId('configPath').value = saved.config_path;
      if (saved.rscript_path) byId('rscriptPath').value = saved.rscript_path;
      state.studyId = saved.study_id || null;
      state.selectedTarget = saved.selected_target || null;
      state.selectedTargetsForPlan = Array.isArray(saved.selected_targets)
        ? saved.selected_targets.map((target) => String(target || '').toUpperCase()).filter(Boolean)
        : [];
      if (state.selectedTarget) recordTargetSource(state.selectedTarget, 'browser_session');
      beginOperation('Restoring last study', 'Reloading saved study inputs and graph progress from the local backend.');
      try {
        await scanInputs({restoreMode: true});
        await refreshGraphReadModels();
        if (runId()) await loadReviewSummary(runId(), {detailLevel: 'summary'});
        completeOperation('Study restored', 'The browser reconnected to the saved local graph run.');
        addEvent('Study restored', `${state.studyId || 'Local study'} / ${runId()} was restored after page refresh.`);
        renderTargetButtons(state.targetCandidates || []);
        renderGraphAwareDashboard();
        renderActionAvailability();
        return true;
      } catch (error) {
        failOperation('Study restore failed', error);
        return false;
      }
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

    function recordTargetSource(target, source) {
      const normalized = String(target || '').toUpperCase();
      if (!isAdamDatasetToken(normalized)) return;
      const current = new Set(state.targetEvidenceSources?.[normalized] || []);
      current.add(source);
      state.targetEvidenceSources[normalized] = Array.from(current).sort();
    }

    function targetSources(target) {
      return state.targetEvidenceSources?.[String(target || '').toUpperCase()] || [];
    }

    function isAdamDatasetToken(value) {
      const raw = String(value || '').toUpperCase();
      if (raw.includes('_')) return false;
      const token = raw.replace(/[^A-Z0-9]/g, '');
      if (!/^AD[A-Z0-9]{2,6}$/.test(token)) return false;
      if (['ADAM', 'ADAMS', 'ADDATA', 'ADAMDATA', 'ADSLIB', 'ADVERSE'].includes(token)) return false;
      return true;
    }

    function isReferenceOnlyTarget(target) {
      const sources = targetSources(target);
      return sources.includes('reference_adam') && !sources.some((source) => source !== 'reference_adam');
    }

    function targetCanAutoPlan(target) {
      const sources = targetSources(target);
      if (!sources.length) return true;
      return sources.some((source) => source !== 'reference_adam');
    }

    function targetSourceHint(target) {
      const sources = targetSources(target);
      if (isReferenceOnlyTarget(target)) return 'reference only';
      if (sources.includes('input_spec') && sources.includes('reference_adam')) return 'spec + reference';
      if (sources.includes('input_spec')) return 'spec evidence';
      if (sources.includes('legacy_code')) return sources.includes('reference_adam') ? 'legacy + reference' : 'legacy evidence';
      if (sources.includes('manual')) return 'manual target';
      if (sources.includes('graph_state') || sources.includes('progress')) return 'run history';
      return 'candidate';
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
      const chip = byId('llmModeChip');
      if (chip) {
        chip.textContent = realMode ? 'Real LLM' : 'Mock LLM';
        chip.className = realMode ? 'llm-mode-chip real' : 'llm-mode-chip';
      }
      byId('llmStatus').className = realMode ? 'note warn' : 'note';
      byId('llmStatus').textContent = realMode
        ? 'Real LLM mode is selected. Test the connection before generating code; the API key is used only for this browser request.'
        : 'Mock mode is active. No external LLM call will be made unless Real LLM API is selected.';
    }

    function openLlmSettings() {
      const modal = byId('llmSettingsModal');
      modal.classList.remove('hidden');
      byId('modelMode').focus();
    }

    function closeLlmSettings() {
      byId('llmSettingsModal').classList.add('hidden');
      byId('openLlmSettingsButton').focus();
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
        byId('globalStatusDetail').textContent = payload.status === 'ok' ? '' : 'Local API responded but is not ready.';
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
      updateMainSectionVisibility();
      try {
        const payload = await api('/product-workspace', {method: 'POST'});
        applyWorkspacePayload(payload);
        state.inputSummary = payload.input_summary;
        renderInputSummary(payload.input_summary);
        saveBrowserSession();
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
      updateMainSectionVisibility();
      try {
        const payload = await api('/demo-study', {method: 'POST'});
        applyWorkspacePayload(payload);
        await scanInputs();
        autoSelectFirstTarget(inferTargets(state.inputSummary));
        saveBrowserSession();
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
        saveBrowserSession();
        addEvent(`${role} uploaded`, uploadDiffMessage(payload));
        byId(uploadStatus[role]).textContent = `${payload.saved_files.length} file(s) uploaded.`;
        byId('workspaceMessage').textContent = `Uploaded ${payload.saved_files.length} file(s). ${uploadDiffMessage(payload)}`;
        completeOperation(`${role} upload complete`, uploadDiffMessage(payload));
        updateMainSectionVisibility();
        setStep(2);
      } catch (error) {
        byId(uploadStatus[role]).textContent = 'Upload failed.';
        byId('workspaceMessage').textContent = String(error);
        failOperation(`${role} upload failed`, error);
      }
    }

    function attachDeleteInputHandlers() {
      for (const button of document.querySelectorAll('[data-delete-input-file]')) {
        if (button.dataset.deleteInputBound === '1') continue;
        button.dataset.deleteInputBound = '1';
        button.addEventListener('click', () => deleteInputFile(button.dataset.deleteInputRole, button.dataset.deleteInputFile));
      }
    }

    async function deleteInputFile(role, fileName) {
      const normalizedRole = String(role || '').trim();
      const normalizedFile = String(fileName || '').trim();
      if (!normalizedRole || !normalizedFile || !studyDir()) return;
      const ok = window.confirm(`Remove ${normalizedFile} from ${titleFromToken(normalizedRole)} inputs? Existing plans, draft specs, generated code, and approvals may become stale.`);
      if (!ok) return;
      beginOperation('Removing input file', `Deleting ${normalizedFile}, rescanning inputs, and invalidating stale graph state if needed.`);
      try {
        const payload = await api(`/studies/files?study_dir=${encodeURIComponent(studyDir())}&role=${encodeURIComponent(normalizedRole)}&file_name=${encodeURIComponent(normalizedFile)}${state.studyId ? `&study_id=${encodeURIComponent(state.studyId)}` : ''}`, {
          method: 'DELETE'
        });
        state.inputSummary = payload.input_summary;
        invalidateUiStateAfterInputChange(payload);
        await refreshGraphReadModels();
        renderInputSummary(payload.input_summary);
        if (runId()) await loadReviewSummary(runId(), {detailLevel: 'summary', refreshGraph: false});
        saveBrowserSession();
        addEvent('Input file removed', `${normalizedFile} was removed. ${uploadDiffMessage(payload)}`);
        completeOperation('Input file removed', uploadDiffMessage(payload));
        renderDraftSpecPane();
        renderPane();
        renderActionAvailability();
      } catch (error) {
        failOperation('Input file removal failed', error);
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
      state.terminalFailureReviewByDataset = {};
      state.draftSpecByDataset = {};
      state.draftSpecReviewByDataset = {};
      state.finalizedInputsByDataset = {};
      state.runReview = null;
      state.runReviewDetailLevel = null;
      state.lastStudyLoopResult = null;
      state.selectedTargetsForPlan = [];
      state.tablePages = {};
      state.compareResults = {};
      setPill('planStatus', 'stale');
      setPill('codeStatus', 'stale');
      byId('planView').innerHTML = '<p class="note warn">Inputs changed. Re-check the output selection, then refresh the dependency plan before generation continues.</p>';
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

    async function scanInputs({restoreMode = false} = {}) {
      const payload = await api(`/study-inputs?study_dir=${encodeURIComponent(studyDir())}`);
      state.inputSummary = payload;
      renderInputSummary(payload);
      if (!restoreMode) addEvent('Inputs scanned', 'The app refreshed study evidence and target candidates.');
      if (byId('runId').value.trim()) await refreshGraphReadModels();
      saveBrowserSession();
      return payload;
    }

    function renderInputSummary(summary) {
      const inferredTargets = inferTargets(summary);
      renderInputProfile(summary, inferredTargets);
      renderEvidenceCards(summary);
      const sdtm = summary?.sdtm?.length || 0;
      const specs = summary?.specs?.length || 0;
      const refs = summary?.reference_adam?.length || 0;
      const define = summary?.define?.length || 0;
      const legacy = summary?.legacy_code?.length || 0;
      const visible = [
        sdtm ? `${sdtm} SDTM` : '',
        specs ? `${specs} spec` : '',
        refs ? `${refs} reference ADaM` : '',
        define ? `${define} define` : '',
        legacy ? `${legacy} legacy` : ''
      ].filter(Boolean);
      byId('inputSummaryLine').textContent = visible.length
        ? `${visible.join(', ')}`
        : 'None';
      byId('evidenceDetailsSummary').textContent = visible.length ? visible.join(' | ') : 'No files yet.';
      const evidenceNotes = inputEvidenceNotes(summary);
      const notesNode = byId('inputEvidenceNotes');
      notesNode.textContent = evidenceNotes.join(' ');
      notesNode.classList.toggle('hidden', !evidenceNotes.length);
      const warnings = [...(summary?.warnings || []), ...((summary?.invalid_files || []).map(inputWarningText))];
      const warningsNode = byId('inputWarnings');
      warningsNode.textContent = warnings.join(' ');
      warningsNode.classList.toggle('hidden', !warnings.length);
      renderTargetButtons(inferredTargets);
      renderGraphAwareDashboard();
      const existingRunId = byId('runId').value.trim();
      if (existingRunId) {
        loadReviewSummary(existingRunId, {detailLevel: 'summary', refreshGraph: false});
      }
    }

    function renderInputProfile(summary, inferredTargets = []) {
      const node = byId('inputProfile');
      if (!node) return;
      const groups = inputEvidenceGroups(summary);
      const inputCount = groups.reduce((total, group) => total + group.files.length, 0);
      if (!inputCount) {
        node.innerHTML = '<div class="note">No evidence yet.</div>';
        return;
      }
      const specCount = summary?.specs?.length || 0;
      const referenceCount = summary?.reference_adam?.length || 0;
      const rows = [
        {
          role: 'SDTM',
          count: summary?.sdtm?.length || 0,
          meaning: 'runtime source data',
          status: (summary?.sdtm?.length || 0) ? 'ready' : 'missing',
          tone: (summary?.sdtm?.length || 0) ? '' : 'warn'
        },
        {
          role: 'ADaM Specs',
          count: specCount,
          meaning: specCount ? 'primary derivation instruction' : 'missing; draft spec review required',
          status: specCount ? 'ready' : 'draft required',
          tone: specCount ? '' : 'warn'
        },
        {
          role: 'Reference ADaM',
          count: referenceCount,
          meaning: referenceCount ? 'compare only, not derivation authority' : 'not uploaded',
          status: referenceCount ? 'compare only' : 'optional',
          tone: referenceCount ? 'warn' : 'optional'
        },
        {
          role: 'Define',
          count: summary?.define?.length || 0,
          meaning: 'metadata evidence for labels, domains, and variables',
          status: (summary?.define?.length || 0) ? 'ready' : 'optional',
          tone: (summary?.define?.length || 0) ? '' : 'optional'
        },
        {
          role: 'Legacy Code',
          count: summary?.legacy_code?.length || 0,
          meaning: 'lineage evidence when uploaded specs are missing',
          status: (summary?.legacy_code?.length || 0) ? 'ready' : 'optional',
          tone: (summary?.legacy_code?.length || 0) ? '' : 'optional'
        },
        {
          role: 'Target Candidates',
          count: inferredTargets.length,
          meaning: 'ADaM outputs inferred from spec, reference, or code names',
          status: inferredTargets.length ? 'available' : 'none yet',
          tone: inferredTargets.length ? '' : 'optional'
        }
      ];
      node.innerHTML = `
        <div class="evidence-summary-table">
          <table>
            <thead><tr><th>Role</th><th>Count</th><th>Meaning</th><th>Status</th></tr></thead>
            <tbody>
              ${rows.map((row) => `
                <tr>
                  <td class="evidence-summary-role">${escapeHtml(row.role)}</td>
                  <td>${escapeHtml(row.count)}</td>
                  <td>${escapeHtml(row.meaning)}</td>
                  <td><span class="evidence-summary-status ${escapeHtml(row.tone)}">${escapeHtml(row.status)}</span></td>
                </tr>
              `).join('')}
            </tbody>
          </table>
        </div>
      `;
    }

    function inputWarningText(item) {
      const fileName = String(item?.path || '').split(/[\\/]/).filter(Boolean).pop() || 'file';
      return `Skipped ${fileName}: ${item?.reason || 'unsupported input file'}`;
    }

    function renderEvidenceCards(summary) {
      const node = byId('evidenceCards');
      if (!node) return;
      const groups = inputEvidenceGroups(summary).filter((group) => group.files.length);
      if (!groups.length) {
        node.innerHTML = '<div class="note">No study evidence recognized yet.</div>';
        return;
      }
      node.innerHTML = groups.map((group) => `
        <div class="evidence-card" data-evidence-role="${escapeHtml(group.role)}">
          <h3>${escapeHtml(group.title)} <span class="pill">${group.files.length} file(s)</span></h3>
          <div class="file-meta">${escapeHtml(group.description)}</div>
          <div id="${escapeHtml(group.containerId)}" class="file-list">${renderFiles(group.containerId, group.files)}</div>
        </div>
      `).join('');
      attachDeleteInputHandlers();
    }

    function inputEvidenceGroups(summary) {
      return [
        {containerId: 'sdtmFiles', role: 'sdtm', title: 'SDTM', files: summary?.sdtm || [], description: 'Source data used by the R runner.'},
        {containerId: 'specFiles', role: 'spec', title: 'Specs', files: summary?.specs || [], description: 'Primary derivation instructions. Used before any generated draft spec.'},
        {containerId: 'referenceFiles', role: 'reference', title: 'Reference ADaM', files: summary?.reference_adam || [], description: 'Comparison/output-shape evidence only. Not derivation authority.'},
        {containerId: 'defineFiles', role: 'define', title: 'Define', files: summary?.define || [], description: 'Metadata evidence for labels, domains, and variables.'},
        {containerId: 'legacyFiles', role: 'legacy', title: 'Legacy Code', files: summary?.legacy_code || [], description: 'Lineage evidence for draft specs when uploaded specs are missing.'}
      ];
    }

    function inputEvidenceNotes(summary) {
      const notes = [];
      const inputCount =
        (summary?.sdtm?.length || 0) +
        (summary?.specs?.length || 0) +
        (summary?.reference_adam?.length || 0) +
        (summary?.define?.length || 0) +
        (summary?.legacy_code?.length || 0);
      if (!inputCount) return [];
      if (!(summary?.specs || []).length) {
        notes.push('No uploaded spec found. The graph can draft a spec from available evidence, but human review is required before code generation.');
      }
      if (!(summary?.reference_adam || []).length) {
        notes.push('No reference ADaM uploaded. Final compare can be skipped or run later after reference evidence is added.');
      } else {
        notes.push('Reference ADaM is comparison evidence only, not derivation authority.');
      }
      if (!(summary?.define || []).length && !(summary?.legacy_code || []).length) {
        notes.push('No define or legacy code uploaded. Draft spec generation will rely mostly on SDTM structure and available targets.');
      }
      return notes.length ? notes : ['Uploaded evidence is ready for target selection and planning.'];
    }

    function renderFiles(containerId, files) {
      if (!files || !files.length) return '<div class="muted">No files found.</div>';
      const role = fileListRoles[containerId] || '';
      return files.map((file) => `
        <div class="file-item ${role === 'reference' ? 'reference-evidence' : ''}">
          <div class="file-title">
            <span>${escapeHtml(file.dataset || file.file_name)}</span>
            <span class="pill ${fileStatusPillClass(file)}">${escapeHtml(fileStatusLabel(file))}</span>
          </div>
          <div class="file-meta"><strong>${escapeHtml(inputAuthorityLabel(role))}</strong></div>
          <div class="file-meta">${escapeHtml(file.file_name)} | ${escapeHtml(file.format)} | ${file.row_count ?? file.line_count ?? '-'} ${file.preview_type === 'code' || file.preview_type === 'text' ? 'lines' : 'rows'}</div>
          <div class="file-meta">${escapeHtml(fileSummary(file))}</div>
          ${file.text_preview ? `<div class="mini-pre">${escapeHtml(file.text_preview)}</div>` : ''}
          ${role ? `<div class="file-actions"><button class="secondary danger" data-delete-input-role="${escapeHtml(role)}" data-delete-input-file="${escapeHtml(file.file_name)}">Remove</button></div>` : ''}
        </div>
      `).join('');
    }

    function inputAuthorityLabel(role) {
      const labels = {
        sdtm: 'Source data. Used as runtime input.',
        spec: 'Primary derivation instruction. Used before any generated draft spec.',
        reference: 'Comparison/output-shape evidence only. Not derivation authority.',
        define: 'Metadata evidence. Helps interpret variables and labels.',
        legacy: 'Derivation evidence. Helps draft specs when uploaded specs are missing.'
      };
      return labels[role] || 'Study evidence.';
    }

    function fileStatusLabel(file) {
      if (file?.status === 'not_previewed' && file?.format === 'sas7bdat') return 'runtime input';
      return file?.status || 'unknown';
    }

    function fileStatusPillClass(file) {
      if (file?.status === 'ok') return '';
      if (file?.status === 'not_previewed' && file?.format === 'sas7bdat') return '';
      return 'warn';
    }

    function fileSummary(file) {
      if (file.preview_type === 'code' || file.preview_type === 'text') {
        const targets = (file.detected_targets || []).join(', ');
        const deps = (file.detected_dependencies || []).join(', ');
        return [targets ? `ADaM tokens: ${targets}` : '', deps ? `Dependency hints: ${deps}` : '', file.note || ''].filter(Boolean).join(' | ');
      }
      if (file.status === 'not_previewed' && file.format === 'sas7bdat') {
        return `SAS dataset recognized. It can be used by the R runner when the required R package is available. Browser preview may be limited.${file.note ? ` ${file.note}` : ''}`;
      }
      const columns = (file.columns || []).slice(0, 10).join(', ');
      return columns || file.note || 'No preview details.';
    }

    function inferTargets(summary) {
      const inputSources = new Set(['input_spec', 'reference_adam', 'legacy_code']);
      const preservedSources = {};
      for (const [target, sources] of Object.entries(state.targetEvidenceSources || {})) {
        const kept = (sources || []).filter((source) => !inputSources.has(source));
        if (kept.length) preservedSources[target] = kept;
      }
      state.targetEvidenceSources = preservedSources;
      const preservedTargets = Object.keys(preservedSources);
      const candidates = new Set();
      for (const spec of summary?.specs || []) {
        const dataset = String(spec.dataset || '').toUpperCase();
        if (isAdamDatasetToken(dataset)) {
          candidates.add(dataset);
          recordTargetSource(dataset, 'input_spec');
        }
        for (const token of inferAdTokens(`${spec.file_name} ${spec.dataset || ''} ${(spec.columns || []).join(' ')}`)) {
          candidates.add(token);
          recordTargetSource(token, 'input_spec');
        }
      }
      for (const ref of summary?.reference_adam || []) {
        const dataset = String(ref.dataset || '').toUpperCase();
        if (isAdamDatasetToken(dataset)) {
          candidates.add(dataset);
          recordTargetSource(dataset, 'reference_adam');
        }
      }
      for (const legacy of summary?.legacy_code || []) {
        for (const token of inferAdTokens(`${legacy.file_name} ${legacy.dataset || ''}`)) {
          candidates.add(token);
          recordTargetSource(token, 'legacy_code');
        }
      }
      const merged = new Set([...preservedTargets, ...candidates]);
      state.targetCandidates = Array.from(merged).sort();
      return state.targetCandidates;
    }

    function inferAdTokens(text) {
      const normalized = String(text || '').toUpperCase();
      const tokens = new Set();
      for (const match of normalized.matchAll(/\bAD[A-Z0-9]{2,6}\b/g)) {
        if (isAdamDatasetToken(match[0])) tokens.add(match[0]);
      }
      for (const match of normalized.matchAll(/ADS[_-]?(AD[A-Z0-9]{2,6})/g)) {
        if (isAdamDatasetToken(match[1])) tokens.add(match[1]);
      }
      return Array.from(tokens);
    }

    function autoSelectFirstTarget(targets) {
      const available = targets.length ? targets : inferTargets(state.inputSummary);
      const autoPlanned = available.filter(targetCanAutoPlan);
      const preferred = preferredInitialTarget(autoPlanned.length ? autoPlanned : available);
      state.selectedTarget = preferred || autoPlanned[0] || available[0] || null;
      state.selectedTargetsForPlan = state.selectedTarget && targetCanAutoPlan(state.selectedTarget) ? [state.selectedTarget] : [];
      renderTargetButtons(available);
      if (state.selectedTargetsForPlan.length) preparePlan();
    }

    function preferredInitialTarget(targets) {
      const normalized = (targets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      if (normalized.includes('ADSL')) return 'ADSL';
      return normalized[0] || null;
    }

    function renderTargetButtons(targets) {
      const node = byId('targetButtons');
      if (!targets.length) {
        node.innerHTML = '<span class="muted">No ADaM targets inferred yet.</span>';
        updateHeaderStatusOverview();
        return;
      }
      if (!state.selectedTarget || !targets.includes(state.selectedTarget)) {
        state.selectedTarget = targets[0];
      }
      const allowed = new Set(targets);
      state.selectedTargetsForPlan = selectedTargets().filter((target) => allowed.has(target));
      if (!state.selectedTargetsForPlan.length && state.selectedTarget && targetCanAutoPlan(state.selectedTarget)) {
        state.selectedTargetsForPlan = [state.selectedTarget];
      }
      const planned = planSelectionSet();
      node.innerHTML = targets.map((target) => `
        <span class="target-option ${planned.has(target) ? 'planned' : ''} ${target === state.selectedTarget ? 'active' : ''}">
          <label class="target-check">
            <input type="checkbox" data-target-toggle="${escapeHtml(target)}" ${planned.has(target) ? 'checked' : ''}>
            <span class="target-name">${escapeHtml(target)}</span>
            <span class="target-hint">${escapeHtml(targetSourceHint(target))}</span>
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
      renderDraftSpecPane();
      renderGraphAwareDashboard();
    }

    function renderTargetSelectionSummary() {
      const planned = selectedTargets();
      const active = state.selectedTarget || '';
      byId('targetSelectionSummary').textContent = planned.length
        ? `Selected: ${planned.join(', ')}${active ? ` | Viewing: ${active}` : ''}`
        : 'No target selected.';
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
      recordTargetSource(value, 'manual');
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
      state.runReviewDetailLevel = null;
      state.lastStudyLoopResult = null;
      state.targetCandidates = state.selectedTarget ? [state.selectedTarget] : [];
      state.targetEvidenceSources = {};
      if (state.selectedTarget) recordTargetSource(state.selectedTarget, 'manual');
      state.selectedTargetsForPlan = state.selectedTarget ? [state.selectedTarget] : [];
      state.tablePages = {};
      state.compareResults = {};
      resetActiveDatasetView();
    }

    function resetActiveDatasetView() {
      syncActiveDatasetState();
      state.selectedResultView = 'generated';
      setPill('codeStatus', codeStatusForActiveDataset());
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
      state.runProgress = progress || null;
      const progressTargets = (progress?.target_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const requestedTargets = (progress?.requested_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      if (progressTargets.length) {
        for (const target of progressTargets) recordTargetSource(target, 'progress');
        state.targetCandidates = Array.from(new Set([...(state.targetCandidates || []), ...progressTargets])).sort();
      }
      if (requestedTargets.length) {
        state.selectedTargetsForPlan = Array.from(new Set(requestedTargets)).sort();
      }
      const activeTarget = String(state.selectedTarget || '').toUpperCase();
      if (requestedTargets.length) {
        const requestedTargetSet = new Set(requestedTargets);
        if (!activeTarget || !requestedTargetSet.has(activeTarget)) {
          state.selectedTarget = requestedTargets[0];
        }
      } else if (progressTargets.length && (!activeTarget || !progressTargets.includes(activeTarget))) {
        state.selectedTarget = progressTargets[0];
      }
      if (progress && Object.prototype.hasOwnProperty.call(progress, 'study_loop_result')) {
        state.lastStudyLoopResult = progress.study_loop_result && Object.keys(progress.study_loop_result).length
          ? progress.study_loop_result
          : null;
      }
      saveBrowserSession();
    }

    function applyGraphState(graph) {
      state.graphState = graph || null;
      const recoveredPlan = planFromGraphState(graph);
      if (recoveredPlan) {
        state.plan = recoveredPlan;
        setPill('planStatus', recoveredPlan.dependency_review_status || 'planned');
      }
      const graphTargets = graph?.target_datasets || [];
      if (graphTargets.length) {
        for (const target of graphTargets) recordTargetSource(target, 'graph_state');
        state.targetCandidates = Array.from(new Set([...(state.targetCandidates || []), ...graphTargets])).sort();
      }
      const requestedTargets = (graph?.requested_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      if (requestedTargets.length) {
        for (const target of requestedTargets) recordTargetSource(target, 'graph_state');
        state.selectedTargetsForPlan = requestedTargets;
      }
      const activeGraphTarget = String(state.selectedTarget || '').toUpperCase();
      if (requestedTargets.length) {
        const requestedGraphTargetSet = new Set(requestedTargets);
        if (!activeGraphTarget || !requestedGraphTargetSet.has(activeGraphTarget)) {
          state.selectedTarget = requestedTargets[0];
        }
      } else if (graphTargets.length && (!activeGraphTarget || !graphTargets.includes(activeGraphTarget))) {
        state.selectedTarget = graphTargets[0];
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
          const existingDraftReview = state.draftSpecReviewByDataset[target] || {};
          state.draftSpecReviewByDataset[target] = {
            ...existingDraftReview,
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
        if (graphDatasetInDraftSpecReview(datasetState)) {
          delete state.generatedByDataset[target];
        } else if (code.code_path && code.status) {
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
          const existingReview = state.reviewByDataset[target] || {};
          state.reviewByDataset[target] = {
            ...existingReview,
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
        if (execution.terminal_failure_review) {
          state.terminalFailureReviewByDataset[target] = {
            ...(state.terminalFailureReviewByDataset[target] || {}),
            ...execution.terminal_failure_review
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
      saveBrowserSession();
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
        dependency_resolution: graph.dependency_resolution || [],
        dependency_warning_records: graph.dependency_plan?.dependency_planning_warning_records || []
      };
    }

    function generatedFor(dataset) {
      return dataset ? state.generatedByDataset[dataset] || null : null;
    }

    function graphDatasetInDraftSpecReview(datasetState) {
      const interrupt = datasetState?.current_interrupt || {};
      return String(interrupt.name || '') === 'draft_spec_review'
        && String(interrupt.status || 'open') === 'open';
    }

    function waitingRuntimeDependenciesFor(target) {
      const progress = datasetProgressFor(target);
      return Array.from(new Set((progress?.waiting_for_runtime_dependencies || [])
        .map((item) => String(item || '').toUpperCase())
        .filter(Boolean)));
    }

    function runtimeDependencyWaitText(target, dependencies) {
      const normalizedTarget = String(target || '').toUpperCase();
      const list = (dependencies || []).join(', ');
      return `${normalizedTarget} is waiting for real local runtime output from ${list}. Complete that upstream ADaM first, then refresh progress. Reference ADaM files do not satisfy this runtime dependency.`;
    }

    function hasRuntimeDependencyWait(target) {
      return waitingRuntimeDependenciesFor(target).length > 0;
    }

    function targetInDraftSpecReview(target) {
      const progress = datasetProgressFor(target);
      if (String(progress?.next_action || '') === 'review_draft_spec') return true;
      const graphDataset = state.graphState?.datasets?.[target] || {};
      return graphDatasetInDraftSpecReview(graphDataset);
    }

    function canApproveGeneratedCode(dataset) {
      const generated = generatedFor(dataset);
      return Boolean(generated && generated.status !== 'stale' && generated.generated_code);
    }

    function activeDependencyBlock() {
      if (!state.selectedTarget) return null;
      return (state.plan?.blocked_datasets || []).find((item) => item.dataset === state.selectedTarget) || null;
    }

    function activeDependencyReviewSummary() {
      const summary = state.runProgress?.dependency_review_summary || {};
      const status = String(summary.status || currentDependencyReviewStatus() || '').trim();
      const interrupt = state.runProgress?.current_interrupt || state.graphState?.current_interrupt || {};
      const openDependencyInterrupt = String(interrupt.name || '') === 'dependency_review'
        && String(interrupt.status || 'open') === 'open';
      const runnable = state.runProgress?.runnable_datasets || state.plan?.runnable_datasets || [];
      if (status === 'warning' && !dependencyReviewBlocksDatasetStart(runnable)) return null;
      if (!openDependencyInterrupt && !['blocked', 'warning', 'review_required', 'stale', 'rejected'].includes(status)) return null;
      return {
        ...summary,
        status,
        open_interrupt: openDependencyInterrupt || summary.open_interrupt === true,
        review_required: summary.review_required !== false
      };
    }

    function dependencyReviewSummaryText(summary) {
      const decisions = Array.isArray(summary?.decisions) ? summary.decisions : [];
      const decisionTexts = decisions
        .filter((item) => String(item.source || '') !== 'no_dependency_evidence')
        .map((item) => {
          const dataset = String(item.dataset || '').toUpperCase();
          const dependencies = (item.dependencies || []).map((dep) => String(dep || '').toUpperCase()).filter(Boolean);
          const evidence = String(item.source || '').trim();
          const available = (item.available_dependencies || []).map((dep) => String(dep || '').toUpperCase()).filter(Boolean);
          const availability = available.length ? ` Available now: ${available.join(', ')}.` : '';
          return dependencies.length
            ? `${dataset} uses ${dependencies.join(', ')} from ${evidence || 'uploaded evidence'}.${availability}`
            : `${dataset} has dependency evidence from ${evidence || 'uploaded evidence'}.`;
        });
      const after = (summary?.actionable_after_approval || [])
        .map((item) => `${String(item.dataset || '').toUpperCase()}: ${item.action_label || titleFromToken(item.next_action || 'continue')}`)
        .filter(Boolean);
      const waiting = (summary?.waiting_after_approval || [])
        .map((item) => `${String(item.dataset || '').toUpperCase()} still waits for ${(item.waiting_for || []).join(', ')}`)
        .filter(Boolean);
      return [
        summary?.detail || 'Review the dependency plan before continuing.',
        ...decisionTexts,
        after.length ? `After approval: ${after.join('; ')}.` : '',
        waiting.length ? `Still waiting: ${waiting.join('; ')}.` : ''
      ].filter(Boolean).join(' ');
    }

    function graphActionGate(progress, actionGroup) {
      if (!progress || !progress.next_action) return null;
      const next = String(progress.next_action || '');
      const labels = {
        finalize: 'Finalize Inputs / Draft Spec',
        approveDraft: 'Approve Draft Spec',
        generate: 'Generate R Code',
        approveCode: 'Approve Code',
        runApproved: 'Run Approved Code'
      };
      const allowed = {
        finalize: ['finalize_inputs', 'reconfirm_inputs'],
        approveDraft: ['review_draft_spec'],
        generate: ['generate_code', 'repair_generated_code', 'revise_approved_spec'],
        approveCode: ['review_code'],
        runApproved: ['execute_approved_code', 'retry_approved_execution']
      }[actionGroup] || [];
      const graphLabel = progress.action_label || titleFromToken(next);
      if (progress.blocked) {
        return {
          ready: false,
          reason: progress.blocked_reason || graphLabel || 'The graph has blocked this dataset.',
          pill: 'blocked'
        };
      }
      if (allowed.includes(next)) {
        return {
          ready: true,
          reason: `Graph next action: ${graphLabel}`,
          pill: titleFromToken(next),
          nextAction: next
        };
      }
      return {
        ready: false,
        reason: `Graph next action is ${graphLabel}; ${labels[actionGroup] || 'this action'} is not the current graph step.`,
        pill: titleFromToken(next),
        nextAction: next
      };
    }

    function graphAllowsCodeGeneration(progress) {
      return ['generate_code', 'repair_generated_code', 'revise_approved_spec'].includes(String(progress?.next_action || ''));
    }

    function nativeFullRunResumeAvailable(target) {
      const normalized = String(target || '').toUpperCase();
      const progress = datasetProgressFor(normalized);
      if (state.graphState) {
        return runtimeHasNativeFullRunContract(state.graphState.runtime_persistence || {}, normalized, progress);
      }
      return runtimeHasNativeFullRunContract(state.runProgress?.runtime_persistence || {}, normalized, progress);
    }

    function hasNativeFullRunContract(target) {
      return nativeFullRunResumeAvailable(target);
    }

    function hasNativeFullRunExecutionContract(target) {
      const normalized = String(target || '').toUpperCase();
      const progress = datasetProgressFor(normalized);
      if (progressAllowsNativeApprovedExecution(progress)) return true;
      if (state.graphState) {
        return runtimeHasNativeFullRunExecutionContract(state.graphState.runtime_persistence || {}, normalized);
      }
      return runtimeHasNativeFullRunExecutionContract(state.runProgress?.runtime_persistence || {}, normalized);
    }

    function progressAllowsNativeApprovedExecution(progress) {
      const next = String(progress?.next_action || '');
      if (!['execute_approved_code', 'retry_approved_execution'].includes(next)) return false;
      if (progress?.blocked) return false;
      return String(progress?.code_status || '').toLowerCase() === 'approved'
        || String(progress?.status || '').toLowerCase() === 'ready_to_execute';
    }

    function runtimeHasNativeFullRunExecutionContract(runtime, normalized) {
      const contract = runtime?.native_dataset_full_run || {};
      if (String(contract.dataset || '').toUpperCase() === normalized
        && String(contract.contract || '') === 'single_dataset_spec_code_review_execute'
        && String(contract.boundary || '') === 'lg3_backend_contract') {
        return true;
      }
      const studyContract = runtime?.native_study_product_loop?.full_run_datasets?.[normalized] || {};
      return String(studyContract.contract || '') === 'single_dataset_spec_code_review_execute'
        && String(studyContract.boundary || '') === 'lg3_backend_contract';
    }

    function runtimeHasNativeFullRunContract(runtime, normalized, progress) {
      const contract = runtime?.native_dataset_full_run || {};
      if (String(contract.dataset || '').toUpperCase() === normalized
        && String(contract.contract || '') === 'single_dataset_spec_code_review_execute'
        && String(contract.boundary || '') === 'lg3_backend_contract') {
        return nativeFullRunCompatibilityGateOpen(contract, normalized, progress);
      }
      const studyContract = runtime?.native_study_product_loop?.full_run_datasets?.[normalized] || {};
      return String(studyContract.contract || '') === 'single_dataset_spec_code_review_execute'
        && String(studyContract.boundary || '') === 'lg3_backend_contract'
        && nativeFullRunCompatibilityGateOpen(studyContract, normalized, progress);
    }

    function nativeFullRunCompatibilityGateOpen(contract, normalized, progress) {
      if (contract && Object.prototype.hasOwnProperty.call(contract, 'compatibility_resume_currently_available')) {
        return contract.compatibility_resume_currently_available === true;
      }
      if (['review_draft_spec', 'review_code'].includes(String(progress?.next_action || ''))) return true;
      const graphDataset = state.graphState?.datasets?.[normalized] || {};
      const interrupt = graphDataset.current_interrupt || {};
      return ['draft_spec_review', 'code_review'].includes(String(interrupt.name || ''))
        && String(interrupt.status || 'open') === 'open';
    }

    function graphCommandRequestBody({dataset = null, interrupt = null, action = 'approve', notes = null, payload = {}} = {}) {
      const body = {
        study_dir: studyDir(),
        action,
        reviewer: byId('reviewer').value.trim() || 'local_user',
        notes: notes ?? byId('reviewNotes').value.trim(),
        payload
      };
      const normalizedDataset = String(dataset || '').toUpperCase();
      if (normalizedDataset) body.dataset = normalizedDataset;
      if (interrupt) body.interrupt = interrupt;
      return JSON.stringify(body);
    }

    function currentDependencyReviewStatus() {
      return String(state.runProgress?.dependency_review_status || state.plan?.dependency_review_status || '').trim();
    }

    function currentDependencyWarningRecords() {
      const progressRecords = state.runProgress?.dependency_warning_records;
      if (Array.isArray(progressRecords) && progressRecords.length) return progressRecords;
      const planRecords = state.plan?.dependency_warning_records;
      if (Array.isArray(planRecords) && planRecords.length) return planRecords;
      const embedded = state.plan?.dependency_plan?.dependency_planning_warning_records;
      return Array.isArray(embedded) ? embedded : [];
    }

    function dependencyWarningAllowsDatasetReviewHandoff(runnable = []) {
      const records = currentDependencyWarningRecords();
      if (!records.length) return false;
      const runnableSet = new Set((runnable || []).map((dataset) => String(dataset || '').toUpperCase()).filter(Boolean));
      if (!runnableSet.size) return false;
      return records.every((record) => (
        record &&
        record.code === 'input_spec_gap_no_default_dependency' &&
        runnableSet.has(String(record.dataset || '').toUpperCase())
      ));
    }

    function dependencyReviewBlocksDatasetStart(runnable = []) {
      const status = currentDependencyReviewStatus();
      if (state.runProgress?.plan_stale) return true;
      if (['blocked', 'review_required', 'rejected', 'stale'].includes(status)) return true;
      if (status === 'warning') return !dependencyWarningAllowsDatasetReviewHandoff(runnable);
      return false;
    }

    function actionAvailability() {
      const target = state.selectedTarget;
      const blocked = activeDependencyBlock();
      const progress = datasetProgressFor(target);
      const waitingRuntimeDependencies = waitingRuntimeDependenciesFor(target);
      const waitingRuntimeReason = waitingRuntimeDependencies.length
        ? runtimeDependencyWaitText(target, waitingRuntimeDependencies)
        : '';
      const progressBlocked = Boolean(progress?.blocked);
      const progressBlockReason = progress?.blocked_reason || '';
      const generated = generatedFor(target);
      const execution = executionFor(target);
      const draft = draftSpecFor(target);
      const draftReview = draftSpecReviewFor(target);
      const finalized = finalizedInputsFor(target);
      const hasSpecGate = targetSpecGateSatisfied(target);
      const targetIsPlanned = Boolean(target && selectedTargets().includes(target));
      const graphProgressMissingTarget = Boolean(state.runProgress && target && targetIsPlanned && !progress);
      const graphProgressMissingCanFinalize = Boolean(
        graphProgressMissingTarget &&
        state.plan &&
        !blocked &&
        !waitingRuntimeDependencies.length &&
        !generated &&
        !execution &&
        !draftReview?.approved &&
        !hasSpecGate
      );
      const graphProgressMissingReason = graphProgressMissingTarget
        ? `Graph progress has no dataset step for ${target}. Refresh graph state or prepare the dependency plan again before continuing.`
        : '';
      const planRequiredReason = target && !targetIsPlanned && isReferenceOnlyTarget(target)
        ? `${target} is currently reference-only evidence. Select its checkbox to request generation before finalizing inputs.`
        : target && !targetIsPlanned
          ? `${target} is only being viewed. Select its checkbox to include it in this generation plan.`
          : '';
      const finalizeGate = graphActionGate(progress, 'finalize');
      const draftGate = graphActionGate(progress, 'approveDraft');
      const generateGate = graphActionGate(progress, 'generate');
      const approveCodeGate = graphActionGate(progress, 'approveCode');
      const runApprovedGate = graphActionGate(progress, 'runApproved');
      const effectiveSpecGate = hasSpecGate || Boolean(generateGate?.ready && graphAllowsCodeGeneration(progress));
      const selected = selectedTargets();
      const progressDatasets = state.runProgress?.datasets || [];
      const startable = progressDatasets.filter((item) =>
        selected.includes(String(item.dataset || '').toUpperCase()) &&
        !item.blocked &&
        ['finalize_inputs', 'generate_code', 'review_draft_spec', 'review_code'].includes(String(item.next_action || ''))
      );
      const blockedCount = (state.runProgress?.blocked_datasets || state.plan?.blocked_datasets || []).length;
      const runnableForDependencyGate = state.runProgress?.runnable_datasets || state.plan?.runnable_datasets || [];
      const dependencyBlocksStart = dependencyReviewBlocksDatasetStart(runnableForDependencyGate);
      const startStudyReady = Boolean(
        selected.length &&
        state.plan &&
        !state.runProgress?.plan_stale &&
        !dependencyBlocksStart &&
        startable.length
      );
      const finalizeReady = finalizeGate
        ? Boolean(target && targetIsPlanned && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && finalizeGate.ready)
        : Boolean(target && targetIsPlanned && !blocked && !progressBlocked && !waitingRuntimeDependencies.length && (!graphProgressMissingTarget || graphProgressMissingCanFinalize));
      const draftApprovalReady = draftGate
        ? Boolean(target && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && draftGate.ready && draft && !draftReview?.approved)
        : Boolean(target && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && draft && !draftReview?.approved && !finalized?.input_spec_available && !targetHasInputSpec(target));
      const generateReady = generateGate
        ? Boolean(target && targetIsPlanned && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && generateGate.ready && effectiveSpecGate)
        : Boolean(target && targetIsPlanned && !blocked && !progressBlocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && hasSpecGate);
      const codeApprovalReady = approveCodeGate?.nextAction === 'review_code'
        ? canApproveGeneratedCode(target)
        : Boolean(generated);
      const approveReady = approveCodeGate
        ? Boolean(target && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && approveCodeGate.ready && codeApprovalReady)
        : Boolean(canApproveGeneratedCode(target) && !blocked && !progressBlocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget);
      const nativeExecutionContract = hasNativeFullRunExecutionContract(target);
      const runReady = runApprovedGate
        ? Boolean(target && !blocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && runApprovedGate.ready && generated && (nativeExecutionContract || progressAllowsNativeApprovedExecution(progress)))
        : Boolean(target && !blocked && !progressBlocked && !waitingRuntimeDependencies.length && !graphProgressMissingTarget && generated && reviewFor(target)?.approved && nativeExecutionContract);
      return {
        finalize: {
          ready: finalizeReady,
          label: 'Finalize Inputs / Draft Spec',
          reason: !target
            ? 'Choose an ADaM output first.'
            : !targetIsPlanned
              ? planRequiredReason
            : !state.plan
              ? 'Clicking will prepare the dependency plan first, then finalize inputs if the target is runnable.'
              : graphProgressMissingTarget && !graphProgressMissingCanFinalize
                ? graphProgressMissingReason
              : waitingRuntimeReason
                ? waitingRuntimeReason
              : progressBlocked
                ? progressBlockReason
                : finalizeGate
                ? finalizeGate.reason
                : blocked
                ? `${target} is blocked by ${blocked.blocked_by}. Resolve or approve the dependency plan first.`
                : hasSpecGate
                  ? `${target} already has an input spec or approved draft spec; finalizing again is optional.`
                  : graphProgressMissingCanFinalize
                    ? `${target} is selected in the current plan but has not started yet. Finalize inputs now to create its spec gate.`
                  : `Ready to check whether ${target} has an input spec or needs a draft spec.`
        },
        startStudy: {
          ready: startStudyReady,
          label: 'Start Runnable Datasets',
          reason: !selected.length
            ? 'Select at least one ADaM output for this study run.'
            : !state.plan
              ? 'Prepare the dependency plan before starting runnable datasets.'
              : state.runProgress?.plan_stale
                ? 'Inputs changed after planning. Re-run dependency planning before starting datasets.'
                : dependencyBlocksStart
                  ? 'Resolve the study dependency review before starting dataset review gates.'
                  : blockedCount && !startable.length
                    ? 'All selected datasets are currently blocked by dependency decisions.'
                    : startable.length
                      ? `Start ${startable.map((item) => item.dataset).join(', ')} and stop at draft/code review gates. This is not dependency proof. R will not run.`
                      : 'No new runnable dataset needs to be started; existing graph progress is preserved.'
        },
        approveDraft: {
          ready: draftApprovalReady,
          label: 'Approve Draft Spec',
          reason: !target
            ? 'Choose an ADaM output first.'
            : graphProgressMissingTarget
              ? graphProgressMissingReason
            : waitingRuntimeReason
              ? waitingRuntimeReason
            : draftGate?.ready
              ? `Graph requires draft-spec review for ${target}. Review and approve the draft before code generation.`
            : finalized?.input_spec_available || targetHasInputSpec(target)
              ? `${target} has an uploaded input spec, so no draft-spec approval is needed.`
            : draftReview?.approved || finalized?.approved_draft_spec_available
                ? `${target} draft spec is already approved for this run.`
                : draftGate && !draftGate.ready
                  ? draftGate.reason
                : draftGate && !draft
                  ? 'Graph is waiting for draft-spec review, but the draft spec is not loaded in this browser. Refresh the run state or finalize inputs again.'
                : draft
                  ? `Review the generated draft spec for ${target}; approve it before code generation.`
                  : 'Finalize inputs first. If no uploaded spec exists, the app will create a draft spec for review.'
        },
        generate: {
          ready: generateReady,
          label: generateGate?.nextAction === 'revise_approved_spec' ? 'Generate Revised Draft Spec' : 'Generate R Code',
          reason: !target
            ? 'Choose an ADaM output first.'
            : !targetIsPlanned
              ? planRequiredReason
            : !state.plan
              ? 'Clicking will prepare the dependency plan first, then generate only if the target is runnable.'
              : graphProgressMissingTarget
                ? graphProgressMissingReason
              : waitingRuntimeReason
                ? waitingRuntimeReason
              : progressBlocked
                ? progressBlockReason
                : generateGate
                ? generateGate.reason
                : blocked
                ? `${target} is blocked by ${blocked.blocked_by}; generation is paused until dependency review is resolved.`
                : generateGate?.nextAction === 'revise_approved_spec'
                  ? 'Graph requires a revised draft spec before new R code can be generated.'
                : !effectiveSpecGate
                  ? 'Confirm the uploaded input spec or review/approve the generated draft spec first.'
                  : generated?.status === 'stale'
                    ? 'Inputs changed after code generation; regenerate R code before review.'
                    : generated
                      ? `${target} already has generated code. Regenerate only if the current code is stale or rejected.`
                      : `Ready to call the selected code generator for ${target}.`
        },
        approveCode: {
          ready: approveReady,
          label: 'Approve Code',
          reason: !target
            ? 'Choose an ADaM output first.'
            : graphProgressMissingTarget
              ? graphProgressMissingReason
            : waitingRuntimeReason
              ? waitingRuntimeReason
            : progressBlocked
              ? progressBlockReason
            : approveCodeGate?.ready && !codeApprovalReady
              ? !generated
                ? 'Generate R code first.'
                : generated.status === 'stale'
                  ? 'Generated code is stale because inputs changed; regenerate before approval.'
                  : !generated.generated_code
                    ? 'Generated-code metadata exists, but the code text is not loaded in this browser. Reload the run review before approving.'
                    : approveCodeGate.reason
            : approveCodeGate
              ? approveCodeGate.reason
            : blocked
              ? `${target} is blocked by ${blocked.blocked_by}; code approval is paused until dependency review is resolved.`
            : execution?.status === 'completed'
              ? `${target} already completed local execution. Regenerate or review only if the current code changed.`
            : execution?.status === 'terminal_failure' || execution?.status === 'failed'
              ? `${target} execution failed. Review diagnostics before retrying or regenerating code.`
                : !generated
                  ? 'Generate R code first.'
                  : approveCodeGate?.nextAction !== 'review_code'
                    ? approveCodeGate?.reason || `Code approval is not the current graph step for ${target}.`
                  : generated.status === 'stale'
                    ? 'Generated code is stale because inputs changed; regenerate before approval.'
                    : !generated.generated_code
                      ? 'Generated-code metadata exists, but the code text is not loaded in this browser. Reload the run review before approving.'
                      : `Ready for human code approval for ${target}. This will not run R.`
        },
        runApproved: {
          ready: runReady,
          label: 'Run Approved Code',
          pill: execution?.status === 'completed' ? 'rerun' : execution?.status === 'terminal_failure' || execution?.status === 'failed' ? 'diagnose' : null,
          reason: !target
            ? 'Choose an ADaM output first.'
            : graphProgressMissingTarget
              ? graphProgressMissingReason
            : waitingRuntimeReason
              ? waitingRuntimeReason
            : progressBlocked
              ? progressBlockReason
            : !nativeExecutionContract && !progressAllowsNativeApprovedExecution(progress)
              ? 'Product UI can execute only graph-owned native full-run code. Compatibility split-flow code must be handled through manual/API migration endpoints.'
            : runApprovedGate
              ? runApprovedGate.reason
            : blocked
              ? `${target} is blocked by ${blocked.blocked_by}; local execution is paused until dependency review is resolved.`
            : execution?.status === 'completed'
              ? `${target} already completed local execution. Rerun only if you intentionally want to repeat the approved code.`
            : execution?.status === 'terminal_failure' || execution?.status === 'failed'
              ? `${target} execution failed. Review diagnostics before retrying or regenerating code.`
              : !generated
                ? 'Generate R code first.'
                : !reviewFor(target)?.approved
                  ? 'Approve the generated code before running local R.'
                  : `Ready to execute the graph-approved code artifact for ${target}.`
        }
      };
    }

    function renderActionAvailability() {
      const availability = actionAvailability();
      setButtonAvailability('finalizeInputsButton', availability.finalize);
      setButtonAvailability('startStudyLoopButton', availability.startStudy);
      setButtonAvailability('approveDraftSpecButton', availability.approveDraft);
      setButtonAvailability('generateCodeButton', availability.generate);
      setButtonAvailability('approveButton', availability.approveCode);
      setButtonAvailability('runApprovedButton', availability.runApproved);
      renderActionHints('specActionHints', visibleSpecActionHints(availability));
      renderActionHints('generationActionHints', visibleGenerationActionHints(availability));
    }

    function setButtonAvailability(id, item) {
      const button = byId(id);
      if (!button) return;
      if (button.dataset.pendingAction === '1') return;
      button.title = item.reason;
      button.setAttribute('aria-disabled-reason', item.reason);
      button.dataset.actionReady = String(Boolean(item.ready));
      button.disabled = !item.ready;
    }

    function setPendingButton(button, label = 'Recording...') {
      if (!button) return () => {};
      const previous = {
        textContent: button.textContent,
        disabled: button.disabled,
        title: button.title,
        ariaBusy: button.getAttribute ? button.getAttribute('aria-busy') : null,
        pendingAction: button.dataset?.pendingAction || ''
      };
      if (button.dataset) button.dataset.pendingAction = '1';
      button.disabled = true;
      button.textContent = label;
      button.title = 'Saving this review decision. The graph state is being updated.';
      if (button.setAttribute) button.setAttribute('aria-busy', 'true');
      if (button.classList) button.classList.add('button-pending');
      return () => {
        button.textContent = previous.textContent;
        button.disabled = previous.disabled;
        button.title = previous.title;
        if (button.dataset) {
          if (previous.pendingAction) button.dataset.pendingAction = previous.pendingAction;
          else delete button.dataset.pendingAction;
        }
        if (button.setAttribute && previous.ariaBusy) button.setAttribute('aria-busy', previous.ariaBusy);
        else if (button.removeAttribute) button.removeAttribute('aria-busy');
        if (button.classList) button.classList.remove('button-pending');
      };
    }

    function setPendingButtons(buttons, primaryButton, label = 'Recording...') {
      const unique = Array.from(new Set((buttons || []).filter(Boolean)));
      if (primaryButton && !unique.includes(primaryButton)) unique.push(primaryButton);
      const restores = unique.map((candidate) => setPendingButton(candidate, candidate === primaryButton ? label : 'Please wait'));
      return () => restores.reverse().forEach((restore) => restore());
    }

    function setPendingButtonGroup(button, selector, label = 'Recording...') {
      const buttons = [];
      if (selector && document.querySelectorAll) {
        for (const candidate of document.querySelectorAll(selector)) buttons.push(candidate);
      }
      if (button && !buttons.includes(button)) buttons.push(button);
      const restores = buttons.map((candidate) => setPendingButton(candidate, candidate === button ? label : 'Please wait'));
      return () => restores.reverse().forEach((restore) => restore());
    }

    function renderActionHints(id, items) {
      const node = byId(id);
      if (!node) return;
      if (!items.length) {
        node.innerHTML = '';
        return;
      }
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

    function visibleSpecActionHints(availability) {
      return [availability.approveDraft, availability.startStudy, availability.finalize].filter(shouldShowActionHint);
    }

    function visibleGenerationActionHints(availability) {
      return [availability.approveCode, availability.runApproved, availability.generate].filter(shouldShowActionHint);
    }

    function shouldShowActionHint(item) {
      if (!item) return false;
      if (item.ready) return true;
      const target = state.selectedTarget || '';
      if (item.label === 'Approve Draft Spec' && (targetInDraftSpecReview(target) || draftSpecFor(target))) return true;
      if (item.label === 'Approve Code' && generatedFor(target) && !reviewFor(target)?.approved) return true;
      if (item.label === 'Run Approved Code' && reviewFor(target)?.approved && !executionFor(target)) return true;
      if (item.pill === 'diagnose') return true;
      return false;
    }

    function reviewFor(dataset) {
      return dataset ? state.reviewByDataset[dataset] || null : null;
    }

    function executionFor(dataset) {
      return dataset ? state.executionByDataset[dataset] || null : null;
    }

    function terminalFailureReviewFor(dataset) {
      return dataset ? state.terminalFailureReviewByDataset[dataset] || null : null;
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
      const activeProgress = datasetProgressFor(state.selectedTarget);
      if (String(activeProgress?.next_action || '') === 'review_draft_spec') return 'draft review';
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
          return 'no upstream ADaM evidence was found; confirm this during spec or code review before trusting the run.';
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

    async function startNativeStudyLoop() {
      const targets = selectedTargets();
      if (!targets.length) return;
      if (!state.plan) await preparePlan();
      const availability = actionAvailability().startStudy;
      if (!availability.ready) {
        byId('planView').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
      beginOperation('Starting runnable datasets', `Dispatching ${targets.join(', ')} to graph-owned draft/code review gates. This is not dependency proof. R will not run.`);
      try {
        const payload = await api('/runs/native-study-loop', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            study_id: state.studyId,
            run_id: runId(),
            target_datasets: targets,
            config_path: byId('configPath').value.trim() || null,
            rscript_path: byId('rscriptPath').value.trim() || null,
            approved_dependency_datasets: [],
            ...llmOverridePayload()
          })
        });
        await refreshGraphReadModels();
        const started = payload.started_datasets || [];
        const resultText = (payload.dataset_results || []).map((item) => `${item.dataset}: ${titleFromToken(item.next_action)}`).join('; ');
        addEvent('Study loop started', payload.message);
        completeOperation(
          started.length ? 'Review gates ready' : 'Study loop checked',
          resultText || payload.message || 'No new dataset was started.'
        );
        setPill('planStatus', payload.status || 'started');
        if (!state.lastStudyLoopResult || state.lastStudyLoopResult.source !== 'graph_progress') {
          state.lastStudyLoopResult = {
            ...payload,
            source: 'command_response',
            requested_targets: targets,
            recorded_at: new Date().toLocaleTimeString([], {hour: '2-digit', minute: '2-digit', second: '2-digit'})
          };
        }
        renderPlan(state.plan || {requested_datasets: targets, runnable_datasets: started, blocked_datasets: payload.blocked_datasets || []});
        renderDraftSpecPane();
        renderPane();
        renderGraphAwareDashboard();
      } catch (error) {
        failOperation('Study loop start failed', error);
        byId('planView').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
      }
    }

    async function approveDraftSpec(button = byId('approveDraftSpecButton')) {
      const draft = draftSpecFor(state.selectedTarget);
      if (!draft) return;
      const restorePending = setPendingButton(button, 'Recording...');
      beginOperation('Saving draft spec approval', `Recording approval for ${draft.dataset}. This only updates the graph state; R is not running.`);
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            dataset: draft.dataset,
            interrupt: 'draft_spec_review',
            action: 'approve',
            notes: byId('reviewNotes').value.trim() || 'Approved for code generation in this run.'
          })
        });
        const reviewedDataset = String(payload.dataset || draft.dataset || '').toUpperCase();
        state.draftSpecReviewByDataset[reviewedDataset] = {
          ...payload,
          approved: payload.approved !== false,
          graph_command: true
        };
        await refreshGraphReadModels();
        const stillWaitingForDraft = targetInDraftSpecReview(reviewedDataset);
        setPill('codeStatus', stillWaitingForDraft ? 'draft review' : generatedFor(reviewedDataset) ? 'review' : 'not generated');
        addEvent(
          'Draft spec approved',
          `${reviewedDataset} draft spec approval was recorded through the graph command gate.`
        );
        completeOperation(
          'Draft spec approved',
          `${reviewedDataset} can now use the approved draft spec for code generation. Use Generate R Code when ready.`
        );
        renderDraftSpecPane();
        renderPane();
        renderActionAvailability();
      } catch (error) {
        byId('draftSpecPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Draft spec approval failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    async function rejectDraftSpec(button = null) {
      const draft = draftSpecFor(state.selectedTarget);
      if (!draft) return;
      const notes = byId('reviewNotes').value.trim();
      if (!notes) {
        byId('draftSpecPane').innerHTML = `${draftSpecReviewHtml(draft)}<p class="note warn">Add a short review note before rejecting this draft spec, so the next draft has correction guidance.</p>`;
        return;
      }
      const restorePending = setPendingButton(button, 'Recording...');
      beginOperation('Saving draft spec rejection', `Recording rejection for ${draft.dataset}. Code generation will stay blocked until the draft is revised or inputs change.`);
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            dataset: draft.dataset,
            interrupt: 'draft_spec_review',
            action: 'reject',
            notes
          })
        });
        const reviewedDataset = String(payload.dataset || draft.dataset || '').toUpperCase();
        state.draftSpecReviewByDataset[reviewedDataset] = {
          ...payload,
          approved: false,
          graph_command: true
        };
        delete state.generatedByDataset[reviewedDataset];
        await refreshGraphReadModels();
        setPill('codeStatus', 'draft rejected');
        addEvent('Draft spec rejected', `${reviewedDataset} draft spec was rejected. Revise inputs or generate a new draft before code generation.`);
        completeOperation('Draft spec rejected', `${reviewedDataset} is back at draft-spec review. Update notes or inputs, then finalize again.`);
        renderDraftSpecPane();
        renderPane();
        renderActionAvailability();
      } catch (error) {
        byId('draftSpecPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Draft spec rejection failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    function renderDraftSpecPane() {
      const node = byId('draftSpecPane');
      if (!node) return;
      if (!state.selectedTarget) {
        node.innerHTML = '<p class="note">Choose an output dataset before finalizing inputs.</p>';
        return;
      }
      const finalized = finalizedInputsFor(state.selectedTarget);
      const progress = datasetProgressFor(state.selectedTarget);
      const graphRequiresDraftReview = progress?.next_action === 'review_draft_spec';
      const draft = draftSpecFor(state.selectedTarget);
      const review = draftSpecReviewFor(state.selectedTarget);
      if (graphRequiresDraftReview && draft) {
        renderDraftSpecReviewTable(node, draft, review);
        return;
      }
      if (finalized?.input_spec_available || targetHasInputSpec(state.selectedTarget)) {
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} has an uploaded input spec. The code generator will use that spec directly. ${artifactRecordedNote('Input spec artifact')}</p>`;
        return;
      }
      if (finalized?.approved_draft_spec_available) {
        node.innerHTML = `<p class="note strong">${escapeHtml(state.selectedTarget)} already has a user-approved draft spec for this run. The code generator can use it now. ${artifactRecordedNote('Approved draft-spec artifact')}</p>`;
        return;
      }
      if (!draft) {
        node.innerHTML = `<p class="note warn">No input spec found for ${escapeHtml(state.selectedTarget)}. Generate a draft spec from uploaded SDTM/reference/define/legacy evidence, review it, then approve it before generating R code.</p>`;
        return;
      }
      renderDraftSpecReviewTable(node, draft, review);
    }

    function renderDraftSpecReviewTable(node, draft, review) {
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
    }

    function artifactRecordedNote(label) {
      return `${label || 'Artifact'} recorded. Technical path is available under Advanced setup and audit files.`;
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
      const targets = dashboardTargets();
      const runnable = progress?.runnable_datasets || state.plan?.runnable_datasets || [];
      const blocked = progress?.blocked_datasets || state.plan?.blocked_datasets || [];
      byId('metricInputs').textContent = String(fileCount);
      byId('metricTargets').textContent = String(targets.length);
      byId('metricRunnable').textContent = String(runnable.length);
      byId('metricBlocked').textContent = String(blocked.length);
      setPill('graphStatus', studyStatusPill(progress, blocked, targets));
      setPill('inspectorStatus', studyStatusPill(progress, blocked, targets));
      updateDashboardPanelVisibility(fileCount, targets, runnable, blocked);
      renderActiveDatasetPanel(runnable, blocked);
      renderStudyProgress(targets, runnable, blocked);
      renderHumanReviewQueue();
      renderStudyLoopResult();
      renderDependencyGraph(targets, runnable, blocked);
      renderDatasetBoard(targets, runnable, blocked);
      setPill('sideDatasetQueueStatus', targets.length ? `${targets.length} target(s)` : 'empty');
      renderAgentAuditPanel();
      renderActionAvailability();
      renderPrimaryNextAction();
      updateMainSectionVisibility();
      applyI18n();
    }

    function updateMainSectionVisibility() {
      const hasInputs = recognizedInputCount() > 0;
      const hasTargets = Boolean(state.selectedTarget || selectedTargets().length || dashboardTargets().length || state.plan || state.runProgress);
      const hasCodeContext = Boolean(
        state.generated ||
        state.review ||
        state.execution ||
        Object.keys(state.generatedByDataset || {}).length ||
        Object.keys(state.reviewByDataset || {}).length ||
        Object.keys(state.executionByDataset || {}).length ||
        state.runReview
      );
      const uploadOpen = !nodeHasClass(byId('uploadPanel'), 'hidden') || Boolean(studyDir());
      toggleHidden('studySetupSection', !uploadOpen && !hasInputs);
      toggleHidden('recognizedEvidenceSection', !hasInputs);
      toggleHidden('chooseOutputSection', !hasInputs);
      toggleHidden('generateRunSection', !(hasTargets || hasCodeContext));
    }

    function nodeHasClass(node, className) {
      if (!node) return false;
      if (node.classList?.contains) return node.classList.contains(className);
      return String(node.className || '').split(/\s+/).includes(className);
    }

    function updateDashboardPanelVisibility(fileCount, targets, runnable, blocked) {
      const hasInputs = fileCount > 0;
      const selected = selectedTargets();
      const hasTargetContext = Boolean(state.selectedTarget || selected.length || targets.length || state.plan || state.runProgress);
      const hasReviewItems = humanReviewQueueItems().length > 0;
      const hasAudit = Boolean(
        state.lastStudyLoopResult ||
        (activeAgentDecisions().length || activeAgentNodeTrace().length || activeRiskFlags().length) ||
        state.runProgress?.native_resume ||
        state.graphState
      );
      toggleHidden('dashboardEmptyGuide', true);
      toggleHidden('dashboardRuntimePanels', !hasInputs);
      toggleHidden('dashboardCurrentPanel', !(hasInputs && Boolean(state.selectedTarget)));
      toggleHidden('sideDatasetQueuePanel', !(hasInputs && (selected.length || targets.length || state.plan || state.runProgress)));
      toggleHidden('humanReviewQueuePanel', !hasReviewItems);
      toggleHidden('metricGrid', !(hasInputs && (state.plan || state.runProgress || targets.length)));
      toggleHidden('dependencyPanel', !(hasInputs && shouldShowDependencyExplanation(blocked)));
      toggleHidden('advancedRunAudit', !(hasInputs && hasAudit));
      toggleHidden('inspectorEmptyGuide', hasInputs && (shouldShowDependencyExplanation(blocked) || hasAudit));
      const hideInspector = !(hasInputs || state.selectedTarget || hasReviewItems || hasAudit);
      toggleHidden('inspectorRail', hideInspector);
      byId('appMain')?.classList.toggle('wide-main', hideInspector);
    }

    function toggleHidden(id, shouldHide) {
      const node = byId(id);
      if (!node) return;
      node.classList.toggle('hidden', Boolean(shouldHide));
    }

    function shouldShowDependencyExplanation(blocked) {
      const target = state.selectedTarget || '';
      if (!target) return false;
      if (waitingRuntimeDependenciesFor(target).length) return true;
      if ((blocked || []).some((item) => String(item.dataset || '').toUpperCase() === target)) return true;
      const progress = datasetProgressFor(target);
      return Boolean(
        progress?.blocked ||
        progress?.next_action === 'resolve_dependency' ||
        activeDependencyBlock()
      );
    }

    function renderPrimaryNextAction() {
      const panel = byId('primaryNextActionPanel');
      if (!panel) return;
      const view = primaryNextActionView();
      panel.className = `next-action-panel ${view.tone || ''}`.trim();
      byId('primaryNextActionTitle').textContent = view.title;
      byId('primaryNextActionDetail').textContent = view.detail;
      byId('primaryNextActionButtons').innerHTML = view.buttons.map(primaryNextActionButtonHtml).join('');
      renderStickyNextAction(view);
      attachPrimaryNextActionHandlers();
      applyI18n();
    }

    function renderStickyNextAction(view = primaryNextActionView()) {
      const bar = byId('stickyNextActionBar');
      if (!bar) return;
      bar.className = `sticky-action-bar ${view.tone || ''}`.trim();
      byId('stickyNextActionTitle').textContent = view.title;
      byId('stickyNextActionDetail').textContent = view.detail;
      byId('stickyNextActionButtons').innerHTML = view.buttons.map(primaryNextActionButtonHtml).join('');
    }

    function renderActiveDatasetPanel(runnable, blocked) {
      const target = state.selectedTarget || '';
      const body = byId('activeDatasetBody');
      if (!target) {
        byId('activeDatasetTitle').textContent = 'No dataset selected';
        byId('activeDatasetSubtitle').textContent = '';
        setPill('activeDatasetStatus', 'waiting');
        body.innerHTML = '<div class="muted">Select an output.</div>';
        return;
      }
      const status = datasetStatus(target, runnable, blocked);
      const progress = datasetProgressFor(target);
      const isBlocked = Boolean((blocked || []).find((item) => item.dataset === target) || progress?.blocked);
      const waiting = waitingRuntimeDependenciesFor(target);
      const qualityStatus = datasetOutputQualityStatus(target);
      const qualityText = activeDatasetQualityText(target, status, qualityStatus);
      const nextText = nextActionText(target, status, isBlocked);
      const sourceText = targetSourcePlainText(target);
      const failureText = terminalFailureActionSummary(target);
      const reasonText = waiting.length
        ? runtimeDependencyWaitText(target, waiting)
        : dependencyDecisionSummary(target, dependencyDecisionFor(target), dependenciesForTarget(target));
      const evidenceText = activeDatasetEvidenceBoundaryText(target, qualityText, sourceText);
      const rows = [
        {label: 'Status', text: activeDatasetStatusPlainText(target, status, qualityStatus), tone: status === 'failed' || isBlocked ? 'fail' : status === 'waiting' ? 'warn' : ''},
        {label: 'Reason', text: reasonText, tone: waiting.length ? 'warn' : ''},
        {label: 'Evidence', text: evidenceText, tone: isReferenceOnlyTarget(target) || evidenceText.includes('draft spec') ? 'warn' : ''},
        {label: 'Next action', text: nextText, tone: isBlocked || waiting.length || status === 'failed' ? 'fail' : 'warn'}
      ];
      if (failureText) rows.push({label: 'Failure choices', text: failureText, tone: 'fail'});
      byId('activeDatasetTitle').textContent = target;
      byId('activeDatasetSubtitle').textContent = activeDatasetSubtitle(target, status);
      setPill('activeDatasetStatus', status);
      body.innerHTML = `
        <div class="active-stage-strip">
          <div class="stage done">inputs</div>
          <div class="stage ${state.plan ? isBlocked ? 'blocked' : 'done' : 'active'}">plan</div>
          <div class="stage ${codeStageClassFor(target, progress, true, selectedTargets().includes(target), isBlocked || waiting.length, isReferenceOnlyTarget(target))}">code</div>
          <div class="stage ${reviewStageClassFor(target, progress)}">review</div>
          <div class="stage ${runStageClassFor(target, progress, ['structural_stub', 'not_real_derivation'].includes(qualityStatus))}">run</div>
        </div>
        <div class="active-dataset-summary">
        ${rows.map((row) => `
          <div class="active-dataset-summary-item ${row.tone || ''}">
            <strong>${escapeHtml(row.label)}</strong>
            <span>${escapeHtml(row.text)}</span>
          </div>
        `).join('')}
        </div>
      `;
    }

    function activeDatasetStatusPlainText(target, status, qualityStatus) {
      if (qualityStatus === 'real_runtime_output') return `${target} has a completed local R output.`;
      if (targetInDraftSpecReview(target)) return `${target} is waiting for draft spec review.`;
      if (generatedFor(target) && !reviewFor(target)?.approved) return `${target} has generated code waiting for human review.`;
      if (reviewFor(target)?.approved && !executionFor(target)) return `${target} code is approved and ready to run locally.`;
      if (executionFor(target)?.status === 'terminal_failure') return `${target} failed during local R execution.`;
      return `${target} is ${status || 'waiting'}.`;
    }

    function activeDatasetEvidenceBoundaryText(target, qualityText, sourceText) {
      const parts = [];
      if (targetHasInputSpec(target) || finalizedInputsFor(target)?.input_spec_available) {
        parts.push('Uploaded spec is the primary instruction.');
      } else if (draftSpecReviewFor(target)?.approved || finalizedInputsFor(target)?.approved_draft_spec_available) {
        parts.push('Approved draft spec is valid for this run only.');
      } else if (draftSpecFor(target)) {
        parts.push('Generated draft spec exists but needs review.');
      } else {
        parts.push('No approved spec yet; draft spec review is required.');
      }
      if (isReferenceOnlyTarget(target)) {
        parts.push('Reference-only candidate: reference ADaM is comparison evidence only, not derivation authority.');
      } else {
        parts.push(`Target source: ${sourceText}.`);
      }
      if (qualityText.includes('completed local R runtime output') || qualityText.includes('completed local R')) {
        parts.push('Runtime output exists.');
      }
      return parts.join(' ');
    }

    function activeDatasetSubtitle(target, status) {
      if (status === 'reference evidence') return 'Reference file exists, but generation still needs a spec or reviewed draft spec.';
      if (waitingRuntimeDependenciesFor(target).length) return 'This dataset is paused until its upstream runtime output exists.';
      if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview) return 'Generated output exists for review and optional comparison.';
      if (targetInDraftSpecReview(target)) return 'Draft spec review is open before code generation.';
      if (generatedFor(target) && !reviewFor(target)) return 'Generated code is waiting for human review before local R execution.';
      return `Current status: ${status || 'candidate'}.`;
    }

    function activeDatasetSpecText(target) {
      if (targetHasInputSpec(target) || finalizedInputsFor(target)?.input_spec_available) return 'Uploaded input spec is available and is the primary derivation instruction.';
      if (draftSpecReviewFor(target)?.approved || finalizedInputsFor(target)?.approved_draft_spec_available) return 'A draft spec has been approved for this run only.';
      if (draftSpecFor(target)) return 'A generated draft spec is waiting for review. It is not approved production logic yet.';
      return 'No uploaded spec is available. The graph must generate and review a draft spec before code generation.';
    }

    function activeDatasetCodeText(target) {
      const generated = generatedFor(target);
      const execution = executionFor(target);
      if (execution?.status === 'terminal_failure') return 'Local R execution failed. Review diagnostics before repair, retry, or skip.';
      if (!generated) return 'No R code has been generated for this dataset.';
      if (generated.status === 'stale') return 'Generated code is stale because inputs changed.';
      if (!generated.generated_code) return 'Generated-code metadata exists, but code text is not loaded in this browser.';
      if (reviewFor(target)?.approved) return 'Generated R code has been approved for local execution.';
      return 'Generated R code needs human review before it can run.';
    }

    function activeDatasetQualityText(target, status, qualityStatus) {
      if (qualityStatus === 'real_runtime_output') return `${target} has a completed local R runtime output. It can be considered runtime evidence for downstream order, subject to review.`;
      if (qualityStatus === 'structural_stub' || qualityStatus === 'not_real_derivation') return `${target} output is review-only and cannot satisfy downstream runtime dependencies.`;
      if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview) return `${target} has generated output available for review. Completion is evidence, not clinical proof.`;
      if (status === 'reference evidence') return 'Reference ADaM is compare/output-shape evidence only, not runtime output generated by this graph.';
      return 'No generated runtime output exists yet.';
    }

    function targetSourcePlainText(target) {
      const sources = targetSources(target);
      if (!sources.length) return 'Manual or graph candidate. Confirm the target before planning.';
      const labels = sources.map((source) => ({
        input_spec: 'uploaded spec',
        legacy_code: 'legacy code',
        reference_adam: 'reference ADaM',
        manual: 'manual selection'
      }[source] || source));
      if (isReferenceOnlyTarget(target)) return `${labels.join(', ')}. Reference-only candidate: selecting it requests generation, but the reference file is not derivation authority.`;
      return labels.join(', ');
    }

    function terminalFailureActionSummary(target) {
      const progress = datasetProgressFor(target);
      const execution = executionFor(target);
      if (!(execution?.status === 'terminal_failure' || progress?.execution_status === 'terminal_failure')) return '';
      const actions = Array.isArray(progress?.available_actions) ? progress.available_actions : [];
      if (!actions.length) return 'Refresh progress to load graph-owned failure actions.';
      return actions
        .map((item) => terminalFailureActionPlainText(item.action, item.label))
        .filter(Boolean)
        .join(' / ');
    }

    function terminalFailureActionPlainText(action, label) {
      const labels = {
        retry_execution: 'rerun the approved code',
        repair_code: 'repair the generated code',
        revise_spec: 'go back to spec review',
        request_inputs: 'ask for more input evidence',
        skip_dataset: 'skip this dataset'
      };
      return labels[action] || label || titleFromToken(action);
    }

    function primaryNextActionView() {
      const inputCount = recognizedInputCount();
      const selected = selectedTargets();
      const target = state.selectedTarget || '';
      const progress = datasetProgressFor(target);
      const availability = actionAvailability();
      const compare = target ? (state.compareResults[target] || datasetReviewFor(target)?.compare_summary) : null;
      const waitingRuntimeDependencies = waitingRuntimeDependenciesFor(target);
      if (!inputCount) {
        return {
          title: 'Start by uploading your study files',
          detail: 'Upload SDTM source data first. Specs are preferred. Reference ADaM can be uploaded later for final comparison only.',
          buttons: [{label: 'Start Upload', action: 'startUpload', primary: true}]
        };
      }
      if (!selected.length || !target) {
        return {
          title: 'Choose the ADaM output you want to generate',
          detail: 'Pick one or more ADaM datasets. The selected dataset becomes the active review panel below.',
          buttons: [{label: 'Go To Output Selection', action: 'scrollTargets', primary: true}],
          tone: 'warn'
        };
      }
      if (!state.plan || state.runProgress?.plan_stale) {
        return {
          title: state.runProgress?.plan_stale ? 'Inputs changed, refresh the plan' : 'Build the dependency plan',
          detail: 'The plan explains whether the selected output can run now or needs another ADaM dataset first.',
          buttons: [{label: 'Prepare Plan', action: 'preparePlan', primary: true}],
          tone: 'warn'
        };
      }
      const dependencyReview = activeDependencyReviewSummary();
      if (dependencyReview) {
        return {
          title: 'Review dependency plan',
          detail: dependencyReviewSummaryText(dependencyReview),
          buttons: [
            {label: 'Approve Dependency Plan', action: 'approveDependencyPlan', primary: true},
            {label: 'Reject Dependency Plan', action: 'rejectDependencyPlan'},
            {label: 'Show Dependency Note', action: 'scrollDependency'}
          ],
          tone: dependencyReview.status === 'blocked' || dependencyReview.status === 'rejected' ? 'fail' : 'warn'
        };
      }
      const dependencyBlocked = availability.finalize.reason && !availability.finalize.ready && (
        String(progress?.next_action || '') === 'resolve_dependency' ||
        Boolean(activeDependencyBlock())
      );
      if (dependencyBlocked) {
        return {
          title: `${target} is waiting for dependency evidence`,
          detail: availability.finalize.reason,
          buttons: [
            {label: 'Show Dependency Note', action: 'scrollDependency', primary: true},
            {label: 'Refresh Progress', action: 'refreshProgress'}
          ],
          tone: 'fail'
        };
      }
      if (waitingRuntimeDependencies.length) {
        return {
          title: `${target} is waiting for ${waitingRuntimeDependencies.join(', ')}`,
          detail: availability.startStudy.ready
            ? `${runtimeDependencyWaitText(target, waitingRuntimeDependencies)} ${availability.startStudy.reason}`
            : runtimeDependencyWaitText(target, waitingRuntimeDependencies),
          buttons: availability.startStudy.ready
            ? [{label: 'Start Upstream Dataset', action: 'startStudy', primary: true}, {label: 'Refresh Progress', action: 'refreshProgress'}]
            : [{label: 'Refresh Progress', action: 'refreshProgress', primary: true}],
          tone: 'warn'
        };
      }
      if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview) {
        const nextDataset = nextDatasetNeedingAction(target);
        if (nextDataset) {
          return {
            title: `${target} finished. Continue with ${nextDataset}`,
            detail: `${target} has a generated output in this run. ${nextActionText(nextDataset, datasetStatus(nextDataset, state.runProgress?.runnable_datasets || state.plan?.runnable_datasets || [], state.runProgress?.blocked_datasets || state.plan?.blocked_datasets || []), Boolean((state.runProgress?.blocked_datasets || state.plan?.blocked_datasets || []).find((item) => item.dataset === nextDataset)))}`,
            buttons: [
              {label: `Open ${nextDataset}`, action: 'selectTarget', target: nextDataset, primary: true},
              {label: `Inspect ${target}`, action: 'showOutput'}
            ],
            tone: 'done'
          };
        }
      }
      if (availability.approveDraft.ready || targetInDraftSpecReview(target)) {
        return {
          title: `Review the draft spec for ${target}`,
          detail: 'No R code should be generated until this draft spec is approved or rejected.',
          buttons: [
            {label: 'Show Draft Spec', action: 'scrollDraft', primary: true},
            {label: 'Approve Draft Spec', action: 'approveDraft'},
            {label: 'Reject Draft Spec', action: 'rejectDraft'}
          ],
          tone: 'warn'
        };
      }
      if (availability.startStudy.ready) {
        return {
          title: 'Start all runnable datasets at their review gates',
          detail: availability.startStudy.reason,
          buttons: [{label: 'Start Runnable Datasets', action: 'startStudy', primary: true}]
        };
      }
      if (availability.finalize.ready) {
        return {
          title: targetHasInputSpec(target)
            ? `Compatibility spec check for ${target}`
            : `Compatibility draft-spec check for ${target}`,
          detail: `${availability.finalize.reason} Normal product flow should use Start Runnable Datasets so the graph owns the review gate.`,
          buttons: [{label: 'Manual Compatibility Check', action: 'finalizeInputs', primary: true}],
          tone: 'warn'
        };
      }
      if (availability.generate.ready) {
        return {
          title: `Generate R code for ${target}`,
          detail: 'This calls the selected code generator and stops before local R execution.',
          buttons: [{label: availability.generate.label, action: 'generateCode', primary: true}]
        };
      }
      if (availability.approveCode.ready) {
        return {
          title: `Review generated R code for ${target}`,
          detail: 'Approve only after checking assumptions and risk points. Approval still does not run R.',
          buttons: [
            {label: 'Show R Code', action: 'showCode', primary: true},
            {label: 'Approve Code', action: 'approveCode'}
          ],
          tone: 'warn'
        };
      }
      if (availability.runApproved.ready) {
        return {
          title: `Run approved R code for ${target}`,
          detail: 'This executes the approved code in the local R sandbox and writes the generated ADaM output.',
          buttons: [{label: 'Run Approved Code', action: 'runApproved', primary: true}]
        };
      }
      if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview) {
        const compareText = compare
          ? compare.status === 'match'
            ? 'Reference compare matches on the checked keys and columns.'
            : compare.status === 'differences'
              ? 'Reference compare found differences. Treat this as review evidence, not automatic failure.'
              : `Reference compare status: ${compare.status}.`
          : 'Open Results to preview the generated table and run compare if reference ADaM exists.';
        return {
          title: `Inspect ${target} output`,
          detail: compareText,
          buttons: [
            {label: 'Show Results', action: 'showOutput', primary: true},
            {label: compare ? 'Run Compare Again' : 'Run Compare', action: 'runCompare'}
          ],
          tone: compare?.status === 'differences' ? 'warn' : ''
        };
      }
      const gate = humanReviewQueueItems()[0];
      if (gate) {
        return {
          title: `${gate.dataset || 'Study'} needs human review`,
          detail: reviewQueueActionText(gate),
          buttons: [{label: 'Show Review Queue', action: 'scrollReviewQueue', primary: true}],
          tone: 'warn'
        };
      }
      return {
        title: 'Refresh the run state',
        detail: 'The browser does not see an active next step. Refresh graph progress before continuing.',
        buttons: [{label: 'Refresh Progress', action: 'refreshProgress', primary: true}],
        tone: 'warn'
      };
    }

    function primaryNextActionButtonHtml(item) {
      const targetAttr = item.target ? ` data-primary-target="${escapeHtml(item.target)}"` : '';
      return `<button class="${item.primary ? '' : 'secondary'}" data-primary-action="${escapeHtml(item.action)}"${targetAttr} type="button">${escapeHtml(tt(item.label))}</button>`;
    }

    function attachPrimaryNextActionHandlers() {
      for (const button of document.querySelectorAll('[data-primary-action]')) {
        if (button.dataset.primaryActionBound === '1') continue;
        button.dataset.primaryActionBound = '1';
        button.addEventListener('click', () => runPrimaryAction(button.dataset.primaryAction, button.dataset.primaryTarget || ''));
      }
    }

    async function runPrimaryAction(action, target = '') {
      if (action === 'loadDemo') return createDemoStudy();
      if (action === 'startUpload') return startUploadWorkspace();
      if (action === 'selectTarget') {
        const normalized = String(target || '').toUpperCase();
        if (normalized) {
          state.selectedTarget = normalized;
          renderTargetButtons(state.targetCandidates || []);
          resetActiveDatasetView();
          byId('datasetBoard')?.scrollIntoView({behavior: 'smooth', block: 'center'});
        }
        return;
      }
      if (action === 'preparePlan') return preparePlan();
      if (action === 'finalizeInputs') return finalizeInputsForDraftSpec();
      if (action === 'approveDependencyPlan') return submitDependencyReviewDecision('approve');
      if (action === 'rejectDependencyPlan') return submitDependencyReviewDecision('reject');
      if (action === 'approveDraft') return approveDraftSpec();
      if (action === 'rejectDraft') return rejectDraftSpec();
      if (action === 'startStudy') return startNativeStudyLoop();
      if (action === 'generateCode') return generateCode();
      if (action === 'approveCode') return approveCode();
      if (action === 'runApproved') return runApprovedCode();
      if (action === 'refreshProgress') {
        await refreshGraphReadModels();
        renderGraphAwareDashboard();
        return;
      }
      if (action === 'showCode') {
        state.selectedView = 'code';
        await ensureReviewSummaryDetail('summary');
        renderPane();
        document.querySelector('[data-view="code"]')?.scrollIntoView({behavior: 'smooth', block: 'center'});
        return;
      }
      if (action === 'showOutput') {
        state.selectedView = 'output';
        state.selectedResultView = 'generated';
        await ensureReviewSummaryDetail('full');
        renderPane();
        document.querySelector('[data-view="output"]')?.scrollIntoView({behavior: 'smooth', block: 'center'});
        return;
      }
      if (action === 'runCompare') {
        const review = datasetReviewFor(state.selectedTarget);
        if (review) {
          state.selectedView = 'output';
          state.selectedResultView = 'compare';
          await ensureReviewSummaryDetail('full');
          renderPane();
          await refreshCompare(datasetReviewFor(state.selectedTarget) || review);
        }
        return;
      }
      const scrollTargets = {
        scrollTargets: 'targetButtons',
        scrollDependency: 'dependencyGraph',
        scrollDraft: 'draftSpecPane',
        scrollReviewQueue: 'humanReviewQueuePanel'
      };
      const id = scrollTargets[action];
      if (id) byId(id)?.scrollIntoView({behavior: 'smooth', block: 'center'});
    }

    function renderStudyProgress(targets, runnable, blocked) {
      const summary = studyProgressSummary(targets, runnable, blocked);
      byId('sideWorkflowTarget').textContent = summary.title;
      byId('sideWorkflowDetail').textContent = summary.detail;
      setPill('sideWorkflowStatus', summary.action);
      updateHeaderStatusOverview();
      byId('sideWorkflowSteps').innerHTML = summary.steps.map((step, index) => `
        <div class="side-progress-step ${step.state}">
          <div class="side-progress-dot">${index + 1}</div>
          <div>
            <strong>${escapeHtml(step.label)}</strong>
            <span>${escapeHtml(step.detail)}</span>
          </div>
        </div>
      `).join('') + nativeResumeProgressNote();
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
      const waitingRuntimeDependencies = waitingRuntimeDependenciesFor(active);
      const activeStatus = active ? datasetStatus(active, runnable, blocked) : 'not selected';
      const activeNext = active
        ? (waitingRuntimeDependencies.length
          ? runtimeDependencyWaitText(active, waitingRuntimeDependencies)
          : activeProgress?.blocked_reason || activeProgress?.action_label || nextActionText(active, activeStatus, Boolean((blocked || []).find((item) => item.dataset === active))))
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
      if ((progress?.datasets || []).some((item) => (item.waiting_for_runtime_dependencies || []).length)) return 'waiting upstream';
      return progress?.next_action || (blocked.length ? 'blocked' : targets.length ? 'ready' : 'waiting');
    }

    function nativeResumeProgressNote() {
      const resume = state.runProgress?.native_resume;
      if (!resume) return '';
      const detail = resume.available
        ? `Native resume: available for ${humanNativeResumeScope(resume.scope)}.`
        : nativeResumeUnavailableText(resume);
      return `
        <div class="side-progress-step ${resume.available ? 'done' : ''}">
          <div class="side-progress-dot">R</div>
          <div>
            <strong>Recovery</strong>
            <span>${escapeHtml(detail)}</span>
          </div>
        </div>
      `;
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
      const planBlocked = dependencyReviewBlocksDatasetStart(progress.runnable_datasets || []);
      const specStatus = activeProgress?.spec_status || '';
      const codeStatus = activeProgress?.code_status || '';
      const executionStatus = activeProgress?.execution_status || '';
      const waitingDependencies = (activeProgress?.waiting_for_runtime_dependencies || []).map((item) => String(item || '').toUpperCase()).filter(Boolean);
      const waitingText = waitingDependencies.length ? `waiting for ${waitingDependencies.join(', ')}` : '';
      return [
        {label: 'Inputs', detail: inputCount ? `${inputCount} file(s)` : 'not loaded', state: inputCount ? 'done' : 'active'},
        {label: 'Plan', detail: progress.plan_stale ? 'replan needed' : planBlocked ? 'review needed' : 'ready', state: progress.plan_stale || planBlocked ? 'blocked' : 'done'},
        {label: 'Spec', detail: waitingText || specStatus || 'not finalized', state: waitingDependencies.length ? 'waiting' : ['input_spec_ready', 'approved'].includes(specStatus) ? 'done' : specStatus === 'draft_generated' ? 'active' : ''},
        {label: 'Code Review', detail: waitingText || codeStatus || 'not generated', state: waitingDependencies.length ? 'waiting' : codeStatus === 'approved' ? 'done' : codeStatus === 'generated' ? 'active' : codeStatus === 'stale' ? 'blocked' : ''},
        {label: 'Run', detail: waitingText || executionStatus || 'waiting', state: waitingDependencies.length ? 'waiting' : executionStatus === 'completed' ? 'done' : ['terminal_failure', 'failed', 'stale'].includes(executionStatus) ? 'blocked' : executionStatus ? 'active' : ''}
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
      attachReviewQueueGraphCommandHandlers();
      attachNativeResumeHandlers();
    }

    function humanReviewQueueItems() {
      const progress = state.runProgress || {};
      if (Array.isArray(progress.review_queue)) {
        return progress.review_queue.map((item) => ({
          scope: item.scope || (item.dataset ? 'dataset' : 'study'),
          dataset: String(item.dataset || '').toUpperCase(),
          name: item.name || item.interrupt || progressInterruptName(item.action),
          status: item.status || 'open',
          source: item.source || 'graph_progress',
          reason: item.reason || item.action_label || '',
          availableActions: Array.isArray(item.available_actions) ? item.available_actions : []
        })).filter(isProductHumanReviewGate);
      }
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
          reason: datasetProgress.action_label || datasetProgress.blocked_reason || '',
          availableActions: datasetProgress.available_actions || []
        });
        if (!datasetProgress.current_interrupt && ['review_code', 'review_draft_spec', 'review_terminal_failure', 'resolve_dependency'].includes(datasetProgress.next_action)) {
          addReviewQueueItem(items, seen, null, {
            scope: 'dataset',
            dataset: datasetProgress.dataset,
            status: datasetProgress.status,
            source: 'progress',
            interruptName: progressInterruptName(datasetProgress.next_action),
            reason: datasetProgress.action_label || datasetProgress.blocked_reason || '',
            availableActions: datasetProgress.available_actions || []
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
      return items.filter(isProductHumanReviewGate);
    }

    function isProductHumanReviewGate(item) {
      const name = String(item?.name || '').trim();
      return ['draft_spec_review', 'code_review', 'terminal_failure'].includes(name);
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
            reason: context.reason || '',
            availableActions: context.availableActions || []
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
          reason: interrupt.reason || context.reason || '',
          availableActions: context.availableActions || []
        };
      }
      if (context.interruptName) {
        return {
          scope: context.scope || 'dataset',
          dataset,
          name: context.interruptName,
          status: context.status || 'open',
          source: context.source || 'progress',
          reason: context.reason || '',
          availableActions: context.availableActions || []
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
          reason: context.reason || '',
          availableActions: context.availableActions || []
        };
      }
      return null;
    }

    function progressInterruptName(nextAction) {
      const names = {
        review_dependency_plan: 'dependency_notice',
        review_draft_spec: 'draft_spec_review',
        review_code: 'code_review',
        review_terminal_failure: 'terminal_failure',
        resolve_dependency: 'dependency_notice'
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
            ${reviewQueueActionHints(item)}
            ${reviewQueueGraphCommandActionHtml(item)}
            ${nativeResumeActionHtml(item.dataset, item.name)}
          </div>
        </div>
      `;
    }

    function reviewQueueActionHints(item) {
      const actions = item.availableActions || item.available_actions || [];
      if (!Array.isArray(actions) || !actions.length) return '';
      const hintActions = item.name === 'dependency_review'
        ? actions.filter((action) => new Set(graphCommandActionsForReviewItem(item).map((allowed) => allowed.action)).has(String(action.action || '').trim()))
        : actions;
      const labels = hintActions
        .map((action) => action.label || titleFromToken(action.action || 'review'))
        .filter(Boolean)
        .join(' / ');
      if (!labels) return '';
      return `<div class="review-queue-actions">Available graph actions: ${escapeHtml(labels)}</div>`;
    }

    function reviewQueueGraphCommandActionHtml(item) {
      const actions = graphCommandActionsForReviewItem(item);
      if (!actions.length) return '';
      const target = String(item.dataset || '').toUpperCase();
      const interrupt = String(item.name || '').trim();
      const buttons = actions.map((action) => `
        <button class="${action.tone === 'reject' ? 'secondary danger' : 'secondary'}" data-review-command-action="${escapeHtml(action.action)}" data-review-command-dataset="${escapeHtml(target)}" data-review-command-interrupt="${escapeHtml(interrupt)}">${escapeHtml(action.label)}</button>
      `).join('');
      return `<div class="review-queue-command-actions" aria-label="Graph command review actions">${buttons}</div>`;
    }

    function graphCommandActionsForReviewItem(item) {
      const interrupt = String(item.name || '').trim();
      const advertised = Array.isArray(item.availableActions || item.available_actions)
        ? item.availableActions || item.available_actions
        : [];
      const advertisedActions = advertised
        .map((action) => String(action.action || '').trim())
        .filter(Boolean);
      const allowedByGate = {
        dependency_review: ['approve', 'reject'],
        draft_spec_review: ['approve', 'reject'],
        code_review: ['approve', 'reject']
      }[interrupt] || [];
      const actionNames = advertisedActions.filter((action) => allowedByGate.includes(action));
      return Array.from(new Set(actionNames)).map((action) => ({
        action,
        label: graphCommandActionLabel(interrupt, action, advertised),
        tone: action === 'reject' ? 'reject' : 'approve'
      }));
    }

    function graphCommandActionLabel(interrupt, action, advertised) {
      const advertisedMatch = (advertised || []).find((item) => String(item.action || '').trim() === action);
      if (advertisedMatch?.label) return advertisedMatch.label;
      const labels = {
        dependency_review: {
          approve: 'Approve Dependency Plan',
          reject: 'Reject Dependency Plan'
        },
        draft_spec_review: {
          approve: 'Approve Draft Spec',
          reject: 'Reject Draft Spec'
        },
        code_review: {
          approve: 'Approve Code',
          reject: 'Reject Code'
        }
      };
      return labels[interrupt]?.[action] || titleFromToken(action);
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

    function renderStudyLoopResult() {
      const result = state.lastStudyLoopResult;
      const list = byId('studyLoopResultList');
      if (!result) {
        byId('studyLoopResultTitle').textContent = 'No batch start yet';
        byId('studyLoopResultDetail').textContent = 'Start Runnable Datasets will show which datasets moved to review gates and which stayed blocked.';
        setPill('studyLoopResultStatus', 'idle');
        list.innerHTML = '<div class="muted">No study-level dataset dispatch has been started in this browser session.</div>';
        return;
      }
      const started = (result.started_datasets || []).map((dataset) => String(dataset || '').toUpperCase()).filter(Boolean);
      const skipped = result.skipped_datasets || [];
      const blocked = result.blocked_datasets || [];
      const reviewQueue = result.review_queue || state.runProgress?.review_queue || [];
      byId('studyLoopResultTitle').textContent = started.length
        ? `${started.length} dataset(s) moved to review gates`
        : skipped.length
          ? 'Existing dataset progress preserved'
          : 'No new dataset moved';
      const sourceText = result.source === 'graph_progress'
        ? 'Recovered from graph progress.'
        : result.source === 'command_response'
          ? 'Recorded from the latest Start Runnable Datasets command.'
          : '';
      const resumeText = result.source === 'graph_progress'
        ? result.native_resume_available
          ? `Durable native resume is available for ${humanNativeResumeScope(result.native_resume_scope)}.`
          : 'Default recovery uses saved graph state; durable LangGraph checkpoint resume is not enabled for this run.'
        : '';
      const resumeQueueText = studyLoopNativeResumeQueueText(result);
      byId('studyLoopResultDetail').textContent = [
        sourceText,
        resumeText,
        resumeQueueText,
        result.recorded_at ? `Last start: ${result.recorded_at}.` : '',
        result.message || '',
        'This does not approve draft specs, approve code, or run R.'
      ].filter(Boolean).join(' ');
      setPill('studyLoopResultStatus', started.length ? 'review' : blocked.length ? 'blocked' : 'clear');
      const rows = [
        ...studyLoopStartedRows(result),
        ...studyLoopSkippedRows(skipped),
        ...studyLoopBlockedRows(blocked),
        ...studyLoopReviewQueueRows(reviewQueue, started, skipped)
      ];
      list.innerHTML = rows.length
        ? rows.join('')
        : '<div class="muted">No new dataset needed a start action. Existing graph progress was preserved.</div>';
      attachNativeResumeHandlers();
    }

    function humanNativeResumeScope(scope) {
      const normalized = String(scope || '').trim();
      if (normalized === 'native_pilot_interrupts_only') return 'pilot graph interrupts only';
      if (!normalized || normalized === 'none') return 'configured graph interrupts';
      return titleFromToken(normalized);
    }

    function nativeResumeUnavailableText(resume) {
      const reason = String(resume?.resume_unavailable_reason || resume?.runtime_binding_status || '').trim();
      if (reason === 'service_not_durable') {
        return 'Native resume: off. This run records a durable checkpoint, but the current service was not opened with it. Use the visible review buttons.';
      }
      if (reason === 'checkpoint_path_mismatch') {
        return 'Native resume: off. The current service is bound to a different checkpoint. Use the visible review buttons.';
      }
      if (reason === 'run_not_durable') {
        return 'Native resume: off. This run uses saved graph state recovery, not a durable LangGraph checkpoint. Use the visible review buttons.';
      }
      return 'Native resume: off. Use the visible review buttons; restart recovery reads saved graph state.';
    }

    function studyLoopNativeResumeQueueText(result) {
      const count = Number(result?.native_resume_queue_item_count || 0);
      const hasQueueItems = Boolean(
        result?.native_resume_has_queue_items ||
        count > 0 ||
        (Array.isArray(result?.native_resume_interrupts) && result.native_resume_interrupts.length)
      );
      if (!hasQueueItems) return '';
      const countText = count > 0
        ? `${count} review gate${count === 1 ? '' : 's'} visible in native resume queue.`
        : 'Review gates are visible in native resume queue.';
      const boundaryText = result?.native_resume_available
        ? 'Use explicit resume controls only when they are shown; this panel is status-only.'
        : nativeResumeUnavailableText(result).replace(/^Native resume: off\. /, '');
      return `${countText} ${boundaryText}`;
    }

    function studyLoopStartedRows(result) {
      const resultsByDataset = new Map((result.dataset_results || []).map((item) => [String(item.dataset || '').toUpperCase(), item]));
      const queueByDataset = new Map((result.review_queue || []).map((item) => [String(item.dataset || '').toUpperCase(), item]));
      return (result.started_datasets || [])
        .map((dataset) => String(dataset || '').toUpperCase())
        .filter(Boolean)
        .map((dataset) => {
          const item = resultsByDataset.get(dataset) || queueByDataset.get(dataset) || {};
          const nextAction = item.next_action || item.name || item.interrupt || 'review_required';
          const label = item.name || item.interrupt ? readableInterruptName(nextAction) : titleFromToken(nextAction);
          const warnings = item.warnings?.length ? ` Warnings: ${item.warnings.join('; ')}` : '';
          return studyLoopResultItemHtml({
            dataset,
            tone: 'warn',
            label,
            detail: `${dataset} stopped at ${label}. Review this gate before any code approval or local R execution.${warnings}`,
            extraHtml: nativeResumeActionHtml(dataset, nextAction)
          });
        });
    }

    function studyLoopBlockedRows(blocked) {
      return (blocked || []).map((item) => {
        const dataset = String(item.dataset || 'Dataset').toUpperCase();
        return studyLoopResultItemHtml({
          dataset,
          tone: 'fail',
          label: 'Blocked',
          detail: `${humanDependencyReason(item.reason)}${item.blocked_by ? ` Blocked by: ${item.blocked_by}.` : ''}`
        });
      });
    }

    function studyLoopSkippedRows(skipped) {
      return (skipped || []).map((item) => {
        const dataset = String(item.dataset || 'Dataset').toUpperCase();
        const nextAction = item.next_action ? titleFromToken(item.next_action) : 'Review existing progress';
        const interrupt = item.interrupt ? ` Current gate: ${readableInterruptName(item.interrupt)}.` : '';
        return studyLoopResultItemHtml({
          dataset,
          tone: 'info',
          label: 'Preserved',
          detail: `${dataset} already has graph progress (${item.status || 'unknown'}). Start Runnable Datasets left it unchanged. Next action: ${nextAction}.${interrupt}`,
          extraHtml: nativeResumeActionHtml(dataset, item.interrupt || item.name || item.next_action)
        });
      });
    }

    function studyLoopReviewQueueRows(reviewQueue, started, skipped) {
      const startedSet = new Set(started || []);
      const skippedSet = new Set((skipped || []).map((item) => String(item.dataset || '').toUpperCase()).filter(Boolean));
      return (reviewQueue || [])
        .filter((item) => {
          const dataset = String(item.dataset || '').toUpperCase();
          return dataset && !startedSet.has(dataset) && !skippedSet.has(dataset);
        })
        .map((item) => studyLoopResultItemHtml({
          dataset: String(item.dataset || 'Study').toUpperCase(),
          tone: 'warn',
          label: readableInterruptName(item.name || item.interrupt || progressInterruptName(item.action)),
          detail: item.reason || 'A review gate is open in graph progress.',
          extraHtml: nativeResumeActionHtml(
            String(item.dataset || '').toUpperCase(),
            item.name || item.interrupt || progressInterruptName(item.action)
          )
        }));
    }

    function studyLoopResultItemHtml(item) {
      return `
        <div class="study-loop-item ${item.tone || ''}">
          <div>
            <div class="study-loop-target">${escapeHtml(item.dataset || 'Study')}</div>
            <span class="pill ${item.tone === 'fail' ? 'fail' : item.tone === 'info' ? 'info' : 'warn'}">${escapeHtml(item.label || 'Review')}</span>
          </div>
          <div>
            <div class="study-loop-action">${escapeHtml(item.label || 'Review required')}</div>
            <div>${escapeHtml(item.detail || 'No detail recorded.')}</div>
            ${item.extraHtml || ''}
          </div>
        </div>
      `;
    }

    function nativeResumeQueueItems() {
      const progressQueue = state.runProgress?.native_resume?.interrupt_queue || [];
      return Array.isArray(progressQueue) ? progressQueue : [];
    }

    function nativeResumeQueueItem(dataset, interruptName) {
      const target = String(dataset || '').toUpperCase();
      const interrupt = normalizeNativeResumeInterruptName(interruptName);
      if (!target || !interrupt) return null;
      return nativeResumeQueueItems().find((item) =>
        String(item.dataset || '').toUpperCase() === target &&
        normalizeNativeResumeInterruptName(item.interrupt || item.name || item.next_action) === interrupt &&
        item.can_resume === true &&
        Array.isArray(item.available_actions) &&
        item.available_actions.length
      ) || null;
    }

    function normalizeNativeResumeInterruptName(value) {
      const raw = String(value || '').trim();
      const names = {
        review_draft_spec: 'draft_spec_review',
        review_code: 'code_review',
        review_terminal_failure: 'terminal_failure',
        draft_spec_review: 'draft_spec_review',
        code_review: 'code_review',
        terminal_failure: 'terminal_failure'
      };
      return names[raw] || raw;
    }

    function nativeResumeActionHtml(dataset, interruptName) {
      const item = nativeResumeQueueItem(dataset, interruptName);
      if (!item) return '';
      const target = String(item.dataset || dataset || '').toUpperCase();
      const interrupt = normalizeNativeResumeInterruptName(item.interrupt || interruptName);
      const buttons = item.available_actions.map((action) => {
        const actionName = String(action.action || '').trim();
        if (!actionName) return '';
        if (!nativeResumeActionAllowed(interrupt, actionName)) return '';
        const label = action.label || titleFromToken(actionName);
        const buttonClass = actionName === 'reject' ? 'secondary danger' : 'secondary';
        return `<button class="${buttonClass}" data-saved-graph-action="${escapeHtml(actionName)}" data-saved-graph-dataset="${escapeHtml(target)}" data-saved-graph-interrupt="${escapeHtml(interrupt)}">${escapeHtml(label)}</button>`;
      }).filter(Boolean).join('');
      if (!buttons) return '';
      return `<div class="review-queue-actions saved-graph-actions">Saved graph resume: ${buttons}</div>`;
    }

    function nativeResumeActionAllowed(interruptName, actionName) {
      const interrupt = normalizeNativeResumeInterruptName(interruptName);
      const action = String(actionName || '').trim();
      const allowed = {
        draft_spec_review: ['approve', 'reject'],
        code_review: ['approve', 'reject'],
        terminal_failure: ['retry_execution', 'repair_code', 'revise_spec', 'request_inputs', 'skip_dataset']
      }[interrupt] || [];
      return allowed.includes(action);
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
        node.innerHTML = '<div class="muted">Load inputs to explain whether the selected ADaM output can run now.</div>';
        return;
      }
      const blockedNames = new Set((blocked || []).map((item) => item.dataset));
      const orderedTargets = [
        ...(state.selectedTarget ? [state.selectedTarget] : []),
        ...targets.filter((target) => target !== state.selectedTarget)
      ];
      const rows = orderedTargets.map((target) => {
        const dependencies = dependenciesForTarget(target);
        const waitingDependencies = waitingRuntimeDependenciesFor(target);
        const status = datasetStatus(target, runnable, blocked);
        const isBlocked = blockedNames.has(target);
        const isWaiting = waitingDependencies.length > 0;
        const decision = dependencyDecisionFor(target);
        const dependencyText = dependencies.length
          ? isWaiting
            ? `${target} needs real local runtime output from ${waitingDependencies.join(', ')} before it can continue.`
            : `${target} needs ${dependencies.join(', ')} before it can run.`
          : `${target} has no upstream ADaM dependency detected from the current uploaded evidence.`;
        const sourceText = dependencySourceEvidenceText(sdtm);
        const actionText = nextActionText(target, status, isBlocked);
        const boundaryText = dependencyTrustBoundaryText(target, dependencies, decision);
        return `
          <div class="dependency-plain-card ${target === state.selectedTarget ? 'active' : ''} ${isBlocked ? 'blocked' : ''} ${isWaiting ? 'waiting' : ''}">
            <div class="dependency-plain-head">
              <span class="dependency-plain-title">${escapeHtml(target)}</span>
              <span class="pill ${isBlocked || status === 'failed' ? 'fail' : isWaiting ? 'warn' : status === 'ready' || status === 'completed' || status === 'reference' ? '' : 'warn'}">${escapeHtml(status)}</span>
            </div>
            <div class="dependency-plain-body">
              <div class="dependency-plain-row"><strong>What it means</strong><span>${escapeHtml(dependencyText)}</span></div>
              <div class="dependency-plain-row"><strong>Why</strong><span>${escapeHtml(dependencyDecisionSummary(target, decision, dependencies))}</span></div>
              <div class="dependency-plain-row"><strong>Evidence</strong><span>${escapeHtml(sourceText)}</span></div>
              <div class="dependency-plain-row"><strong>Next action</strong><span>${escapeHtml(actionText)}</span></div>
              <div class="trust-boundary">${escapeHtml(boundaryText)}</div>
            </div>
          </div>
        `;
      });
      node.innerHTML = `<div class="dependency-plain">${rows.join('') || '<div class="muted">No dependency plan yet.</div>'}</div>`;
    }

    function dependencySourceEvidenceText(sdtm) {
      if (!sdtm.length) return 'No SDTM source has been recognized yet.';
      const shown = sdtm.slice(0, 8).join(', ');
      return `Recognized SDTM domains: ${shown}${sdtm.length > 8 ? ', ...' : ''}.`;
    }

    function dependencyDecisionSummary(target, decision, dependencies) {
      const waitingDependencies = waitingRuntimeDependenciesFor(target);
      if (waitingDependencies.length) return `${target} is paused until ${waitingDependencies.join(', ')} has a real local runtime output in this run.`;
      if (dependencies.length) return `${target} has upstream ADaM dependency: ${dependencies.join(', ')}.`;
      if (decision?.source === 'input_spec_no_adam_dependency') return `${target} input spec does not show an upstream ADaM dependency.`;
      if (decision?.source === 'no_dependency_evidence') return `${target} has no upstream ADaM dependency evidence in the current uploaded materials; this must be confirmed in spec/code review.`;
      return `${target} dependency plan has not recorded an upstream ADaM dependency.`;
    }

    function dependencyTrustBoundaryText(target, dependencies, decision) {
      const hasReference = hasReferenceAdamEvidence(target) || dependencies.some((dependency) => hasReferenceAdamEvidence(dependency));
      if (hasReference) {
        return 'Reference ADaM is used only to preview shape and compare final output. It does not decide derivation logic and does not replace an uploaded spec or reviewed draft spec.';
      }
      if (decision?.source === 'no_dependency_evidence') {
        return 'No dependency evidence is not the same as clinical proof. The user still reviews the spec and generated code before local R execution.';
      }
      return 'This panel explains runtime order only. It does not prove the ADaM derivation is clinically correct.';
    }

    function dependencyRuntimeSummary(target, status, isBlocked) {
      const waitingDependencies = waitingRuntimeDependenciesFor(target);
      if (waitingDependencies.length) return runtimeDependencyWaitText(target, waitingDependencies);
      const quality = datasetOutputQualityStatus(target);
      if (isBlocked || status === 'blocked') return `${target} cannot generate until the dependency gate is resolved.`;
      if (status === 'ready') return `${target} can move to spec/code review; review must still confirm that the dependency assumption is correct.`;
      if (quality === 'structural_stub') return `${target} has a structural demo output for review only. It cannot satisfy downstream runtime dependencies.`;
      if (quality === 'not_real_derivation') return `${target} has a mock/offline output for review only. It cannot satisfy downstream runtime dependencies.`;
      if (quality === 'real_runtime_output' || status === 'completed') return `${target} has a completed local R runtime output for review.`;
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

    function nextDatasetNeedingAction(currentTarget = '') {
      const current = String(currentTarget || '').toUpperCase();
      const targets = dashboardTargets();
      const ordered = [
        ...targets.filter((target) => target !== current),
        ...targets.filter((target) => target === current)
      ];
      return ordered.find((target) => {
        const progress = datasetProgressFor(target);
        const status = datasetStatus(target, state.runProgress?.runnable_datasets || state.plan?.runnable_datasets || [], state.runProgress?.blocked_datasets || state.plan?.blocked_datasets || []);
        if (status === 'reference evidence') return false;
        if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview || progress?.execution_status === 'completed') return false;
        if (waitingRuntimeDependenciesFor(target).length) return true;
        if (progress?.blocked) return true;
        const next = String(progress?.next_action || '').toLowerCase();
        return Boolean(
          next ||
          !targetSpecGateSatisfied(target) ||
          generatedFor(target) ||
          selectedTargets().includes(target)
        );
      }) || null;
    }

    function dashboardTargets() {
      const progressTargets = (state.runProgress?.target_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const candidates = (state.targetCandidates || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      const selected = selectedTargets();
      const graphTargets = (state.graphState?.target_datasets || []).map((target) => String(target || '').toUpperCase()).filter(Boolean);
      return Array.from(new Set([...selected, ...progressTargets, ...graphTargets, ...candidates])).filter(Boolean);
    }

    function nextActionText(target, status, isBlocked) {
      const progress = datasetProgressFor(target);
      const waitingDependencies = waitingRuntimeDependenciesFor(target);
      if (waitingDependencies.length) return runtimeDependencyWaitText(target, waitingDependencies);
      if (progress?.blocked_reason) return progress.blocked_reason;
      if (progress?.action_label) return progress.action_label;
      if (isBlocked) {
        const block = (state.plan?.blocked_datasets || []).find((item) => item.dataset === target);
        return `Action required before generation: ${block ? `${block.dataset} needs ${block.blocked_by}` : 'resolve blocked dependencies'}.`;
      }
      if (state.runProgress && !progress) {
        if (status === 'reference evidence') return 'Reference ADaM is available for compare/output-shape evidence only. It is not an approved derivation rule or runtime input by itself.';
        if (status === 'ready') return `${target} is in the plan, but graph progress has no dataset step yet. Refresh graph state or prepare the dependency plan again before continuing.`;
        return `Graph progress has no dataset step for ${target}. Refresh graph state or prepare the dependency plan again before continuing.`;
      }
      if (!state.plan) return 'Next: prepare the dependency plan for this target.';
      if (!targetSpecGateSatisfied(target)) return 'Next: click Finalize Inputs / Draft Spec, then approve the draft spec if no uploaded spec exists.';
      if (!generatedFor(target)) return 'Next: click Generate R Code. This will not run R yet.';
      if (generatedFor(target)?.status === 'stale') return 'Inputs changed after code generation. Regenerate R code before review or execution.';
      if (!generatedFor(target)?.generated_code) return 'Generated-code state exists, but the code text is not loaded in this browser. Reload the run review before approving.';
      if (!reviewFor(target) && !executionFor(target)) return 'Next: review the generated R code, then approve local execution.';
      const completedExecution = executionFor(target)?.status === 'completed' || datasetProgressFor(target)?.execution_status === 'completed';
      if (completedExecution) {
        const quality = datasetOutputQualityStatus(target);
        if (quality === 'structural_stub' || quality === 'not_real_derivation') return 'Next: inspect this review-only/demo output. It cannot be used as runtime input for another dataset.';
        return 'Next: inspect the generated ADaM table, compare result, and downloads.';
      }
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
      const cards = targets.map((target) => {
        const progress = datasetProgressFor(target);
        const status = datasetStatus(target, runnable, blocked);
        const isActive = target === state.selectedTarget;
        const isPlanned = selectedTargets().includes(target);
        const waitingDependencies = waitingRuntimeDependenciesFor(target);
        const qualityStatus = datasetOutputQualityStatus(target);
        const reviewOnlyOutput = ['structural_stub', 'not_real_derivation'].includes(qualityStatus);
        const statusClass = progress?.blocked || status === 'blocked' || status === 'failed' ? 'fail' : waitingDependencies.length || reviewOnlyOutput ? 'warn' : ['ready', 'completed', 'reference'].includes(status) ? '' : 'warn';
        const isBlocked = blockedNames.has(target) || progress?.blocked;
        const isWaiting = waitingDependencies.length > 0;
        const isReferenceOnly = status === 'reference evidence' && !isPlanned;
        const lane = datasetLaneFor(target, status, progress, isBlocked, isWaiting, reviewOnlyOutput, isReferenceOnly);
        return {
          target,
          lane,
          html: `
          <div class="dataset-card ${isActive ? 'active' : ''} ${isBlocked ? 'blocked' : ''} ${isWaiting ? 'waiting' : ''}" data-card-target="${escapeHtml(target)}">
            <div class="dataset-top">
              <span class="dataset-name">${escapeHtml(target)}</span>
              <span class="pill ${statusClass}">${escapeHtml(status)}</span>
            </div>
            <div class="dataset-context">${escapeHtml(datasetPlanningContext(target, isPlanned, isActive, status))}</div>
            <div class="dataset-next"><strong>Next:</strong> ${escapeHtml(nextActionText(target, status, isBlocked))}</div>
          </div>
        `
        };
      });
      node.innerHTML = renderDatasetLanes(cards);
      for (const card of node.querySelectorAll('[data-card-target]')) {
        card.addEventListener('click', () => {
          state.selectedTarget = card.dataset.cardTarget;
          renderTargetButtons(state.targetCandidates || []);
          resetActiveDatasetView();
        });
      }
    }

    function datasetLaneFor(target, status, progress, isBlocked, isWaiting, reviewOnlyOutput, isReferenceOnly) {
      const next = String(progress?.next_action || '').toLowerCase();
      const executionStatus = String(progress?.execution_status || '').toLowerCase();
      if (executionFor(target)?.status === 'completed' || datasetReviewFor(target)?.output_preview || executionStatus === 'completed') return 'done';
      if (reviewOnlyOutput || isReferenceOnly) return 'done';
      if (isBlocked || status === 'blocked' || status === 'failed' || ['terminal_failure', 'failed'].includes(executionStatus)) return 'blocked';
      if (isWaiting || status === 'waiting upstream') return 'waiting';
      if (['review_draft_spec', 'review_code', 'review_terminal_failure', 'resolve_dependency'].includes(next) || targetInDraftSpecReview(target) || generatedFor(target)?.generated_code) return 'review';
      return 'ready';
    }

    function renderDatasetLanes(cards) {
      const lanes = [
        {key: 'review', title: 'Needs review', detail: 'Approve, reject, or repair before the graph continues.'},
        {key: 'ready', title: 'Ready to continue', detail: 'These datasets can move to the next graph step.'},
        {key: 'waiting', title: 'Waiting upstream', detail: 'Complete the named upstream ADaM first, then refresh progress.'},
        {key: 'blocked', title: 'Blocked', detail: 'A dependency or runtime failure needs a decision.'},
        {key: 'done', title: 'Finished or reference-only', detail: 'Inspect results or use reference ADaM only for comparison.'}
      ];
      return lanes.map((lane) => {
        const laneCards = cards.filter((card) => card.lane === lane.key);
        if (!laneCards.length) return '';
        return `
          <div class="dataset-lane ${lane.key}">
            <div class="dataset-lane-head">
              <span>${escapeHtml(lane.title)}</span>
              <span>${laneCards.length} dataset(s)</span>
            </div>
            <div class="muted">${escapeHtml(lane.detail)}</div>
            ${laneCards.map((card) => card.html).join('')}
          </div>
        `;
      }).join('') || '<div class="muted">No dataset is currently in the work queue.</div>';
    }

    function codeStageClassFor(target, progress, isActive, isPlanned, isBlocked, isReferenceOnly) {
      const codeStatus = String(progress?.code_status || '').toLowerCase();
      const executionStatus = String(progress?.execution_status || '').toLowerCase();
      const nextAction = String(progress?.next_action || '').toLowerCase();
      if (codeStatus === 'stale') return 'blocked';
      if (
        ['generated', 'approved'].includes(codeStatus) ||
        ['running', 'executing', 'completed', 'terminal_failure', 'failed'].includes(executionStatus) ||
        ['review_code', 'execute_approved_code', 'retry_approved_execution', 'review_terminal_failure', 'complete'].includes(nextAction)
      ) {
        return 'done';
      }
      return isActive && isPlanned && !isBlocked && !isReferenceOnly ? 'active' : '';
    }

    function reviewStageClassFor(target, progress) {
      const codeStatus = String(progress?.code_status || '').toLowerCase();
      const executionStatus = String(progress?.execution_status || '').toLowerCase();
      const nextAction = String(progress?.next_action || '').toLowerCase();
      if (codeStatus === 'stale') return 'blocked';
      if (
        codeStatus === 'approved' ||
        ['running', 'executing', 'completed', 'terminal_failure', 'failed'].includes(executionStatus) ||
        ['execute_approved_code', 'retry_approved_execution', 'review_terminal_failure', 'complete'].includes(nextAction)
      ) {
        return 'done';
      }
      if (nextAction === 'review_code' || codeStatus === 'generated') return 'active';
      return '';
    }

    function runStageClassFor(target, progress, reviewOnlyOutput) {
      if (reviewOnlyOutput) return 'review-only';
      if (hasRuntimeDependencyWait(target)) return 'waiting';
      const executionStatus = String(progress?.execution_status || '').toLowerCase();
      const nextAction = String(progress?.next_action || '').toLowerCase();
      if (executionStatus === 'completed' || nextAction === 'complete') return 'done';
      if (['terminal_failure', 'failed', 'stale'].includes(executionStatus) || nextAction === 'review_terminal_failure') return 'blocked';
      if (executionStatus || ['execute_approved_code', 'retry_approved_execution'].includes(nextAction)) return 'active';
      return '';
    }

    function datasetPlanningContext(target, isPlanned, isActive, status) {
      const parts = [];
      parts.push(isPlanned ? 'planned in this run' : 'view-only history/candidate');
      const waiting = waitingRuntimeDependenciesFor(target);
      if (waiting.length) parts.push(`waiting for real upstream output: ${waiting.join(', ')}`);
      if (status === 'reference evidence' && !isPlanned) parts.push('reference ADaM only: compare/output-shape evidence, not generation input');
      if (isActive) parts.push('active detail view');
      return `${target}: ${parts.join(' | ')}`;
    }

    function renderAgentAuditPanel() {
      const node = byId('agentAuditGrid');
      const decisions = activeAgentDecisions().slice(-8).reverse();
      const traces = activeAgentNodeTrace().slice(-6).reverse();
      const risks = activeRiskFlags();
      byId('agentAuditTitle').textContent = state.selectedTarget
        ? `${state.selectedTarget} agent decisions`
        : 'Study agent decisions';
      byId('agentAuditDetail').textContent = decisions.length
        ? `${decisions.length} recent decision(s), ${traces.length} node handoff(s).`
        : 'No agent decisions are recorded for the active view yet.';
      setPill('agentAuditStatus', decisions.length ? 'audited' : 'waiting');
      node.innerHTML = decisions.length
        ? decisions.map((decision) => agentDecisionCard(decision)).join('')
        : '<div class="muted">Run dependency planning, draft spec, code generation, execution, or compare to populate agent audit.</div>';
      byId('agentNodeTrace').innerHTML = traces.length
        ? traces.map((trace) => agentNodeTraceCard(trace)).join('')
        : '<div class="muted">Agent node handoffs will appear here after graph product steps run.</div>';
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

    function activeAgentNodeTrace() {
      const graph = state.graphState || {};
      const target = state.selectedTarget;
      const datasetState = target ? graph.datasets?.[target] : null;
      const datasetInputs = Array.isArray(datasetState?.agent_node_inputs) ? datasetState.agent_node_inputs : [];
      const datasetOutputs = Array.isArray(datasetState?.agent_node_outputs) ? datasetState.agent_node_outputs : [];
      const hasDatasetTrace = Boolean(datasetInputs.length || datasetOutputs.length);
      const inputs = hasDatasetTrace ? datasetInputs : graph.agent_node_inputs || [];
      const outputs = hasDatasetTrace ? datasetOutputs : graph.agent_node_outputs || [];
      const inputByKey = new Map((Array.isArray(inputs) ? inputs : []).map((item) => [agentNodeTraceKey(item), item]));
      return (Array.isArray(outputs) ? outputs : []).map((output) => ({
        output,
        input: inputByKey.get(agentNodeTraceKey(output)) || null
      }));
    }

    function agentNodeTraceKey(record) {
      return [
        record?.agent || '',
        record?.node || '',
        record?.dataset || '',
        record?.study_id || '',
        record?.run_id || ''
      ].join('|');
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

    function agentNodeTraceCard(trace) {
      const output = trace.output || {};
      const input = trace.input || {};
      const status = String(output.status || '').toLowerCase();
      const riskCount = (output.risk_flags || []).length + (input.risk_flags || []).length;
      const klass = status.includes('fail') || status.includes('terminal') ? 'fail' : riskCount || status.includes('warning') || status.includes('review') ? 'warn' : '';
      const task = input.task || readableNodeName(output.node);
      const result = readableDecisionName(output.decision || output.status || 'recorded');
      const artifactCount = (output.artifact_ids || []).length + (input.artifact_ids || []).length;
      const scope = output.dataset || input.dataset || 'Study';
      return `
        <div class="agent-trace-card ${klass}">
          <div>
            <strong>${escapeHtml(readableAgentName(output.agent || input.agent))}</strong>
            <span>${escapeHtml(readableNodeName(output.node || input.node))}</span>
          </div>
          <div>
            <strong>${escapeHtml(task)}</strong>
            <span>${escapeHtml(result)} for ${escapeHtml(scope)}${riskCount ? ` | ${riskCount} risk flag(s)` : ''}</span>
          </div>
          <div>
            <strong>${escapeHtml(output.status || 'recorded')}</strong>
            <span>${artifactCount ? `${artifactCount} artifact reference(s)` : 'No artifact reference'}</span>
          </div>
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
      if (waitingRuntimeDependenciesFor(target).length) return 'waiting upstream';
      const quality = datasetOutputQualityStatus(target);
      if (quality === 'structural_stub') return 'demo output';
      if (quality === 'not_real_derivation') return 'review only';
      if (progress?.status) return progress.status;
      if ((blocked || []).find((item) => item.dataset === target)) return 'blocked';
      if (state.runProgress) {
        if ((runnable || []).includes(target)) return 'ready';
        if (hasReferenceAdamEvidence(target)) return 'reference evidence';
        return state.plan ? 'waiting' : 'candidate';
      }
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

    function datasetOutputQualityStatus(target) {
      const progress = datasetProgressFor(target);
      const progressQuality = progress?.output_quality?.quality_status;
      if (progressQuality) return progressQuality;
      if (state.runProgress) return '';
      const reviewQuality = datasetReviewFor(target)?.output_quality?.quality_status;
      if (reviewQuality) return reviewQuality;
      return '';
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
      const activeProgress = datasetProgressFor(state.selectedTarget);
      const nextAction = String(activeProgress?.next_action || '');
      const revisingSpec = nextAction === 'revise_approved_spec';
      const graphOwnedGeneration = graphAllowsCodeGeneration(activeProgress);
      if (!graphOwnedGeneration && !targetSpecGateSatisfied(state.selectedTarget)) {
        byId('reviewPane').innerHTML = '<p class="note warn">No approved input spec is available. Click Finalize Inputs / Draft Spec, review the draft spec, then approve it before generating R code.</p>';
        byId('draftSpecPane').scrollIntoView({behavior: 'smooth', block: 'center'});
        return;
      }
      beginOperation(
        revisingSpec ? 'Generating revised draft spec' : 'Starting dataset generation',
        revisingSpec
          ? `Re-entering the graph-owned ${state.selectedTarget} flow. It will stop at draft-spec review before new R code is generated.`
          : `Starting the graph-owned ${state.selectedTarget} flow. It will stop at draft-spec review or code review before any R execution.`
      );
      setPill('codeStatus', 'running');
      try {
        const overrides = llmOverridePayload();
        const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(state.selectedTarget)}/native-full-run`, {
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
        await applyNativeFullRunStart(payload);
        state.selectedView = 'summary';
        setActiveTab();
        const interruptName = payload.current_interrupt?.name || payload.next_action || '';
        const waitingForDraft = interruptName === 'draft_spec_review';
        setPill('codeStatus', waitingForDraft ? 'draft review' : 'review');
        addEvent(
          waitingForDraft ? 'Draft spec generated' : 'R code generated',
          waitingForDraft
            ? `${payload.dataset} draft spec is ready for review. R has not been generated yet.`
            : `${payload.dataset} code is ready for review.`
        );
        completeOperation(
          waitingForDraft ? 'Draft spec ready' : 'R code generated',
          waitingForDraft
            ? `${payload.dataset} draft spec is ready for human review.`
            : `${payload.dataset} code is ready for review. R has not been executed yet.`
        );
        renderDraftSpecPane();
        renderGraphAwareDashboard();
        renderActionAvailability();
        setStep(waitingForDraft ? 4 : 5);
        renderPane();
      } catch (error) {
        setPill('codeStatus', 'failed');
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('R code generation failed', error);
      }
    }

    async function applyNativeFullRunStart(payload) {
      const target = String(payload.dataset || state.selectedTarget || '').toUpperCase();
      const previousGenerated = target ? state.generatedByDataset[target] || {} : {};
      const interruptName = String(payload.current_interrupt?.name || payload.next_action || '');
      await refreshGraphReadModels();
      if (target) state.selectedTarget = target;
      if (target && interruptName === 'draft_spec_review') {
        delete state.generatedByDataset[target];
      } else if (payload.code_path) {
        const existingGenerated = state.generatedByDataset[target] || {};
        const sameCodeArtifact = previousGenerated.code_path === payload.code_path;
        state.generatedByDataset[target] = {
          ...existingGenerated,
          study_id: payload.study_id,
          run_id: payload.run_id,
          dataset: target,
          status: payload.status === 'needs_review' ? 'generated' : payload.status,
          code_path: payload.code_path,
          draft_spec_path: payload.draft_spec_path || null,
          static_check_path: payload.static_check_path || null,
          generated_code: sameCodeArtifact ? existingGenerated.generated_code || '' : '',
          assumptions: sameCodeArtifact ? existingGenerated.assumptions || [] : [],
          risk_points: sameCodeArtifact ? existingGenerated.risk_points || [] : [],
          used_inputs: sameCodeArtifact ? existingGenerated.used_inputs || [] : [],
          expected_outputs: sameCodeArtifact ? existingGenerated.expected_outputs || [] : []
        };
      }
      await loadReviewSummary(payload.run_id || runId(), {detailLevel: 'summary', refreshGraph: false});
      syncActiveDatasetState();
    }

    async function approveCode(button = byId('approveButton')) {
      const generated = generatedFor(state.selectedTarget);
      if (!generated) return;
      const availability = actionAvailability().approveCode;
      if (!availability.ready) {
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
      state.generated = generated;
      const restorePending = setPendingButton(button, 'Recording...');
      beginOperation(
        'Saving code approval',
        `Recording human approval for ${generated.dataset}. This only updates the graph state; R will not run in this step.`
      );
      setPill('codeStatus', 'review');
      try {
        state.review = await api(`/runs/${encodeURIComponent(generated.run_id || runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            dataset: generated.dataset,
            interrupt: 'code_review',
            action: 'approve'
          })
        });
        const reviewedDataset = String(state.review.dataset || generated.dataset || '').toUpperCase();
        state.reviewByDataset[reviewedDataset] = {
          ...state.review,
          approved: state.review.approved !== false,
          graph_command: true
        };
        await refreshGraphReadModels();
        setPill('codeStatus', 'approved');
        addEvent('Code approved', `${reviewedDataset} code approval was recorded through the graph command gate. R has not been executed yet.`);
        completeOperation('Code approved', `${reviewedDataset} is ready for explicit local R execution.`);
        renderActionAvailability();
        renderPane();
        renderGraphAwareDashboard();
      } catch (error) {
        setPill('codeStatus', 'failed');
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(String(error))}</p>`;
        failOperation('Code approval failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    async function runApprovedCode() {
      const generated = generatedFor(state.selectedTarget);
      if (!generated) return;
      const availability = actionAvailability().runApproved;
      if (!availability.ready) {
        byId('reviewPane').innerHTML = `<p class="note warn">${escapeHtml(availability.reason)}</p>`;
        renderActionAvailability();
        return;
      }
      state.generated = generated;
      beginOperation(
        'Running approved R code',
        `Executing the graph-approved ${generated.dataset} R code with local Rscript.`
      );
      setPill('codeStatus', 'running');
      try {
        if (!hasNativeFullRunExecutionContract(generated.dataset)) {
          throw new Error('Product UI can execute only graph-owned native full-run code. Use the compatibility execute-approved-code API only for old split-flow runs.');
        }
        state.execution = await api(`/runs/${encodeURIComponent(generated.run_id)}/datasets/${encodeURIComponent(generated.dataset)}/native-full-run/execute`, {
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
        await loadReviewSummary(generated.run_id, {detailLevel: 'full', refreshGraph: false});
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
        failOperation('Approved-code execution failed', error);
      }
    }

    async function loadReviewSummary(id, {detailLevel = 'full', refreshGraph = false, render = true} = {}) {
      try {
        const normalizedDetail = String(detailLevel || 'full').toLowerCase() === 'summary' ? 'summary' : 'full';
        state.runReview = await api(`/runs/${encodeURIComponent(id)}/review-summary?study_dir=${encodeURIComponent(studyDir())}&detail_level=${encodeURIComponent(normalizedDetail)}`);
        state.runReviewDetailLevel = state.runReview?.detail_level || normalizedDetail;
        if (refreshGraph) await refreshGraphReadModels();
        if (!state.graphState && reviewSummaryHasGeneratedCode(state.runReview)) {
          await refreshGraphState();
        }
        for (const review of state.runReview?.dataset_reviews || []) {
          applyReviewSummaryDataset(review);
          if (review?.compare_summary && review.compare_summary.status !== 'not_run') {
            state.compareResults[review.dataset] = review.compare_summary;
          }
        }
        syncActiveDatasetState();
        setPill('codeStatus', codeStatusForActiveDataset());
        if (render) {
          renderAdvanced();
          renderGraphAwareDashboard();
          renderActionAvailability();
          renderPane();
        }
      } catch {
        state.runReview = null;
        state.runReviewDetailLevel = null;
        renderAdvanced();
        renderGraphAwareDashboard();
      }
    }

    function reviewSummaryHasGeneratedCode(summary) {
      return Boolean((summary?.dataset_reviews || []).some((review) => review?.generated_code_path || review?.generated_code));
    }

    async function ensureReviewSummaryDetail(detailLevel = 'full') {
      if (!runId() || !studyDir()) return null;
      const normalizedDetail = String(detailLevel || 'full').toLowerCase() === 'summary' ? 'summary' : 'full';
      if (
        state.runReview &&
        state.runReview.run_id === runId() &&
        (normalizedDetail === 'summary' || state.runReviewDetailLevel === 'full')
      ) {
        return state.runReview;
      }
      await loadReviewSummary(runId(), {detailLevel: normalizedDetail, refreshGraph: false, render: false});
      return state.runReview;
    }

    function applyReviewSummaryDataset(review) {
      const target = String(review?.dataset || '').toUpperCase();
      if (!target) return;
      if ((review.generated_code_path || review.generated_code) && graphAllowsReviewSummaryCodeRecovery(target)) {
        const existingGenerated = state.generatedByDataset[target] || {};
        state.generatedByDataset[target] = {
          ...existingGenerated,
          study_id: state.runReview?.study_id || state.studyId,
          run_id: state.runReview?.run_id || runId(),
          dataset: target,
          status: existingGenerated.status || 'generated',
          code_path: review.generated_code_path || existingGenerated.code_path || null,
          generated_code: review.generated_code || existingGenerated.generated_code || '',
          assumptions: review.assumptions || existingGenerated.assumptions || [],
          risk_points: review.risk_points || existingGenerated.risk_points || [],
          warnings: review.warnings || existingGenerated.warnings || [],
          used_inputs: existingGenerated.used_inputs || [],
          expected_outputs: existingGenerated.expected_outputs || []
        };
      }
      if (String(review.status || '').toLowerCase() === 'needs_review' && review.generated_code && graphRequiresCodeReview(target)) {
        delete state.reviewByDataset[target];
      }
    }

    function graphAllowsReviewSummaryCodeRecovery(target) {
      if (targetInDraftSpecReview(target)) return false;
      const progress = datasetProgressFor(target);
      const nextAction = String(progress?.next_action || '');
      if (['review_code', 'execute_approved_code', 'retry_approved_execution'].includes(nextAction)) return true;
      const graphDataset = state.graphState?.datasets?.[target] || {};
      const codeStatus = String(graphDataset.code_state?.status || '');
      return ['generated', 'approved', 'stale'].includes(codeStatus);
    }

    function graphRequiresCodeReview(target) {
      const progress = datasetProgressFor(target);
      if (String(progress?.next_action || '') === 'review_code') return true;
      const graphDataset = state.graphState?.datasets?.[target] || {};
      const interrupt = graphDataset.current_interrupt || {};
      if (String(interrupt.name || '') === 'code_review' && String(interrupt.status || 'open') === 'open') return true;
      return String(graphDataset.code_state?.status || '') === 'generated';
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
        const failurePanel = terminalFailurePanel(generated?.dataset || state.selectedTarget);
        if (targetInDraftSpecReview(state.selectedTarget)) {
          pane.innerHTML = `${failurePanel}<p class="note warn">Review the generated draft spec above before R code can be generated. Nothing has been sent to R yet.</p>`;
          attachTerminalFailureHandlers();
          return;
        }
        if (!generated) {
          pane.innerHTML = `${failurePanel}<p class="note">Generate code after choosing a target. Nothing has been sent to R yet.</p>`;
          attachTerminalFailureHandlers();
          return;
        }
        pane.innerHTML = `
          ${failurePanel}
          <p class="note strong">R code is ready for ${escapeHtml(generated.dataset)}. Review the assumptions, then approve to run locally.</p>
          ${draftSpecNotice(generated)}
          <div class="grid2">
            <div class="card"><h3>What will happen</h3><ul class="clean">${listItems(generated.expected_outputs, 'No output declared.')}</ul></div>
            <div class="card"><h3>Inputs used</h3><ul class="clean">${listItems(generated.used_inputs, 'No inputs declared.')}</ul></div>
          </div>
        `;
        attachTerminalFailureHandlers();
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

    function terminalFailurePanel(dataset) {
      const target = String(dataset || '').toUpperCase();
      const execution = executionFor(target);
      const datasetProgress = datasetProgressFor(target);
      const review = terminalFailureReviewFor(target);
      const graphActions = Array.isArray(datasetProgress?.available_actions) ? datasetProgress.available_actions : [];
      const graphGateOpen = Boolean(datasetProgress && datasetProgress.next_action === 'review_terminal_failure');
      const progressTerminalFailure = datasetProgress?.status === 'terminal_failure' || datasetProgress?.execution_status === 'terminal_failure';
      const executionTerminalFailure = execution?.status === 'terminal_failure';
      if (!target || (!executionTerminalFailure && !progressTerminalFailure) || !graphGateOpen) return '';
      const diagnostics = execution?.diagnostics_path
        ? ' Diagnostics were recorded in the run audit artifacts.'
        : ' Diagnostics were not linked in the current read model.';
      const reviewed = review?.action || review?.decision;
      const reviewedNote = reviewed
        ? `<p class="note strong">Last failure decision: ${escapeHtml(titleFromToken(reviewed))}. Continue with the matching next action from the graph.</p>`
        : '';
      const actionControls = graphActions.length
        ? graphActions.map((item) => `<button class="secondary" data-terminal-action="${escapeHtml(item.action)}" data-terminal-dataset="${escapeHtml(target)}">${escapeHtml(terminalFailureActionButtonLabel(item.action, item.label))}</button>`).join('')
        : '<p class="note">Waiting for graph-owned terminal-failure actions to load. Refresh progress before choosing a follow-up.</p>';
      return `
        <div class="card terminal-failure-panel">
          <h3>Terminal Failure Triage</h3>
          <p class="note warn">${escapeHtml(target)} failed during local R execution.${diagnostics} Choose one controlled next step; the graph will record the decision before any retry, repair, spec revision, new input request, or batch continuation.</p>
          <p class="note">Repair generated code creates a new code artifact for review. Rerun approved code repeats the already approved artifact. Go back to spec review when the failure suggests the derivation instruction is wrong.</p>
          ${reviewedNote}
          <div class="button-row">
            ${actionControls}
          </div>
        </div>
      `;
    }

    function terminalFailureActionButtonLabel(action, fallbackLabel) {
      const labels = {
        retry_execution: 'Rerun Approved Code',
        repair_code: 'Repair Generated Code',
        revise_spec: 'Back To Spec Review',
        request_inputs: 'Request More Inputs',
        skip_dataset: 'Skip Dataset'
      };
      return labels[action] || fallbackLabel || titleFromToken(action);
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
      if (!selectedPreview) {
        const available = downloadAvailable(review, kind === 'reference' ? 'reference' : 'generated');
        if (available) {
          return `
            <p class="note strong">${kind === 'reference' ? 'Reference ADaM' : 'Generated ADaM'} exists, but table preview has not been loaded yet.</p>
            <p class="note">Open this output tab to load detailed previews, or use Downloads for the file directly. SAS tables may take longer because the backend reads them through local R.</p>
          `;
        }
        return `<p class="note">${kind === 'reference' ? 'No reference ADaM found for this dataset.' : 'No generated ADaM output found yet.'}</p>`;
      }
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
      const canRunCompare = Boolean(
        (review?.output_preview && review?.reference_preview) ||
        (downloadAvailable(review, 'generated') && downloadAvailable(review, 'reference'))
      );
      const hasRecordedCompare = compare && String(compare.status || '') !== 'not_run';
      const compareButtonLabel = hasRecordedCompare ? 'Run Compare Again' : 'Run Compare';
      if (!hasRecordedCompare) {
        if (!canRunCompare) return '<p class="note">Compare is not available yet. A generated table and a reference ADaM table are both required.</p>';
        return `
          <p class="note">Compare has not been run yet. Reference ADaM is used only as comparison evidence.</p>
          <div class="button-row"><button class="secondary" id="refreshCompareButton">Run Compare</button></div>
        `;
      }
      const mismatchRows = (compare.mismatch_samples || []).map((item) => `
        <tr><td>${escapeHtml(item.key)}</td><td>${escapeHtml(item.column)}</td><td>${escapeHtml(item.generated)}</td><td>${escapeHtml(item.reference)}</td></tr>
      `).join('');
      const verdict = compareVerdict(compare);
      const rowDelta = compareRowDelta(compare);
      const generatedOnlyKeys = (compare.generated_only_keys || []).length;
      const referenceOnlyKeys = (compare.reference_only_keys || []).length;
      return `
        <div class="compare-verdict ${verdict.tone}">
          <strong>${escapeHtml(verdict.title)}</strong>
          <span>${escapeHtml(verdict.detail)}</span>
        </div>
        <div class="compare-mini-grid">
          <div class="compare-mini"><strong>${compare.row_count_generated ?? '-'}</strong><span>generated rows</span></div>
          <div class="compare-mini"><strong>${compare.row_count_reference ?? '-'}</strong><span>reference rows</span></div>
          <div class="compare-mini"><strong>${rowDelta}</strong><span>row delta</span></div>
          <div class="compare-mini"><strong>${compare.mismatch_count ?? 0}</strong><span>cell mismatches</span></div>
        </div>
        <div class="grid2" style="margin-top:12px;">
          <div class="card"><h3>Column Check</h3><ul class="clean">
            <li>Generated only: ${escapeHtml((compare.generated_only_columns || []).join(', ') || 'none')}</li>
            <li>Reference only: ${escapeHtml((compare.reference_only_columns || []).join(', ') || 'none')}</li>
            <li>Keys used: ${escapeHtml((compare.key_columns || []).join(', ') || 'row order')}</li>
          </ul></div>
          <div class="card"><h3>Row Check</h3><ul class="clean">
            <li>Generated-only keys: ${generatedOnlyKeys}${generatedOnlyKeys ? ` (${escapeHtml((compare.generated_only_keys || []).slice(0, 8).join(', '))}${generatedOnlyKeys > 8 ? ', ...' : ''})` : ''}</li>
            <li>Reference-only keys: ${referenceOnlyKeys}${referenceOnlyKeys ? ` (${escapeHtml((compare.reference_only_keys || []).slice(0, 8).join(', '))}${referenceOnlyKeys > 8 ? ', ...' : ''})` : ''}</li>
          </ul></div>
        </div>
        <div class="button-row"><button class="secondary" id="refreshCompareButton">${escapeHtml(compareButtonLabel)}</button></div>
        <h3 style="margin-top:12px;">Mismatch Samples</h3>
        <div class="table-wrap"><table><thead><tr><th>Key</th><th>Column</th><th>Generated</th><th>Reference</th></tr></thead><tbody>${mismatchRows || '<tr><td class="muted" colspan="4">No mismatch samples.</td></tr>'}</tbody></table></div>
      `;
    }

    function downloadAvailable(review, kind) {
      return Boolean((review?.downloads || []).find((item) => item.kind === kind && item.available));
    }

    function compareVerdict(compare) {
      const status = String(compare?.status || '').toLowerCase();
      if (status === 'match') {
        return {
          title: 'Reference compare matched',
          detail: 'The generated output matched the uploaded reference ADaM on the checked keys and columns. This is still comparison evidence, not clinical proof.',
          tone: ''
        };
      }
      if (status === 'differences') {
        return {
          title: 'Reference compare found differences',
          detail: 'This usually means the generated output and reference ADaM are not the same table. Review row counts, row keys, and mismatch samples. A reference mismatch does not automatically prove the generated code is wrong.',
          tone: 'warn'
        };
      }
      if (status === 'missing_reference') {
        return {
          title: 'No reference ADaM was available',
          detail: 'The generated output can be previewed and downloaded, but no reference comparison can be made.',
          tone: 'warn'
        };
      }
      if (status === 'not_supported') {
        return {
          title: 'Reference compare is not supported for this file',
          detail: compare?.note || 'The current backend could not compare this reference format.',
          tone: 'warn'
        };
      }
      return {
        title: `Compare status: ${status || 'unknown'}`,
        detail: compare?.note || 'Review the generated output and reference evidence manually.',
        tone: 'warn'
      };
    }

    function compareRowDelta(compare) {
      const generated = Number(compare?.row_count_generated);
      const reference = Number(compare?.row_count_reference);
      if (!Number.isFinite(generated) || !Number.isFinite(reference)) return '-';
      const delta = generated - reference;
      return delta > 0 ? `+${delta}` : String(delta);
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
      attachTerminalFailureHandlers();
    }

    function attachTerminalFailureHandlers() {
      for (const button of document.querySelectorAll('[data-terminal-action]')) {
        button.addEventListener('click', () => submitTerminalFailureReview(button.dataset.terminalDataset, button.dataset.terminalAction, button));
      }
    }

    function attachReviewQueueGraphCommandHandlers() {
      for (const button of document.querySelectorAll('[data-review-command-action]')) {
        if (button.dataset.reviewCommandBound === '1') continue;
        button.dataset.reviewCommandBound = '1';
        button.addEventListener('click', () => submitReviewQueueGraphCommand({
          dataset: button.dataset.reviewCommandDataset,
          interrupt: button.dataset.reviewCommandInterrupt,
          action: button.dataset.reviewCommandAction,
          button
        }));
      }
    }

    async function submitReviewQueueGraphCommand({dataset = '', interrupt = '', action = '', button = null} = {}) {
      const normalizedInterrupt = String(interrupt || '').trim();
      const normalizedDataset = String(dataset || '').toUpperCase();
      const normalizedAction = String(action || '').trim();
      if (!normalizedInterrupt || !normalizedAction) return;
      const item = humanReviewQueueItems().find((candidate) => (
        String(candidate.name || '') === normalizedInterrupt &&
        String(candidate.dataset || '').toUpperCase() === normalizedDataset
      ));
      if (!graphCommandReviewActionAllowed(item, normalizedInterrupt, normalizedAction)) return;
      const restorePending = setPendingButtonGroup(
        button,
        `[data-review-command-dataset="${normalizedDataset}"][data-review-command-interrupt="${normalizedInterrupt}"]`,
        'Recording...'
      );
      beginOperation(
        'Saving review decision',
        `${readableInterruptName(normalizedInterrupt)}: ${titleFromToken(normalizedAction)}. This records the decision only; R is not running.`
      );
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            dataset: normalizedInterrupt === 'dependency_review' ? null : normalizedDataset,
            interrupt: normalizedInterrupt,
            action: normalizedAction,
            notes: byId('reviewNotes').value.trim() || `Selected ${normalizedAction} from the graph review queue.`,
            payload: graphCommandPayloadForReview(normalizedInterrupt, normalizedAction)
          })
        });
        await refreshGraphReadModels();
        await loadReviewSummary(payload.run_id || runId(), {detailLevel: 'summary', refreshGraph: false});
        applyReviewQueueGraphCommandResponse(payload, normalizedInterrupt);
        addEvent(
          'Graph review decision recorded',
          `${readableInterruptName(normalizedInterrupt)}: ${titleFromToken(payload.action || normalizedAction)} -> ${titleFromToken(payload.next_action || payload.status)}.`
        );
        completeOperation(
          'Graph review decision recorded',
          `${readableInterruptName(normalizedInterrupt)} is now ${titleFromToken(payload.status || payload.action || normalizedAction)}.`
        );
        renderPlan(state.plan || {});
        renderDraftSpecPane();
        renderPane();
        renderGraphAwareDashboard();
        renderActionAvailability();
      } catch (error) {
        failOperation('Graph review decision failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    async function submitDependencyReviewDecision(action, button = null) {
      const normalizedAction = String(action || '').trim();
      const summary = activeDependencyReviewSummary();
      if (!summary || !['approve', 'reject'].includes(normalizedAction)) return;
      const restorePending = setPendingButtonGroup(
        button,
        '[data-primary-action="approveDependencyPlan"], [data-primary-action="rejectDependencyPlan"]',
        'Recording...'
      );
      beginOperation(
        normalizedAction === 'approve' ? 'Approving dependency plan' : 'Rejecting dependency plan',
        dependencyReviewSummaryText(summary)
      );
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            interrupt: 'dependency_review',
            action: normalizedAction,
            notes: byId('reviewNotes').value.trim() || `${normalizedAction === 'approve' ? 'Approved' : 'Rejected'} dependency plan from the main workflow step.`,
            payload: graphCommandPayloadForReview('dependency_review', normalizedAction)
          })
        });
        await refreshGraphReadModels();
        await loadReviewSummary(payload.run_id || runId(), {detailLevel: 'summary', refreshGraph: false});
        applyReviewQueueGraphCommandResponse(payload, 'dependency_review');
        const next = state.runProgress?.action_label || state.runProgress?.next_action || payload.next_action || payload.status;
        addEvent(
          'Dependency plan decision recorded',
          `${titleFromToken(normalizedAction)} -> ${titleFromToken(next)}.`
        );
        completeOperation(
          'Dependency plan decision recorded',
          normalizedAction === 'approve'
            ? `Dependency evidence is accepted for this run. Next: ${titleFromToken(next)}.`
            : 'Dependency plan was rejected. Update inputs or prepare a new plan before continuing.'
        );
        renderPlan(state.plan || {});
        renderDraftSpecPane();
        renderPane();
        renderGraphAwareDashboard();
        renderActionAvailability();
      } catch (error) {
        failOperation('Dependency plan decision failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    function graphCommandReviewActionAllowed(item, interrupt, action) {
      if (!item) return false;
      return graphCommandActionsForReviewItem(item).some((candidate) => candidate.action === action);
    }

    function graphCommandPayloadForReview(interrupt, action) {
      if (interrupt === 'dependency_review' && action === 'approve') {
        return {approved_dependency_datasets: []};
      }
      return {};
    }

    function applyReviewQueueGraphCommandResponse(payload, interrupt) {
      const target = String(payload.dataset || '').toUpperCase();
      if (interrupt === 'draft_spec_review' && target) {
        state.draftSpecReviewByDataset[target] = {
          ...payload,
          approved: payload.action === 'approve' || payload.approved === true,
          graph_command: true
        };
        if (payload.action === 'reject' || payload.approved === false) {
          delete state.generatedByDataset[target];
          delete state.reviewByDataset[target];
          delete state.executionByDataset[target];
        }
        return;
      }
      if (interrupt === 'code_review' && target) {
        state.reviewByDataset[target] = {
          ...payload,
          approved: payload.action === 'approve' || payload.approved === true,
          graph_command: true
        };
        if (payload.action === 'reject' || payload.approved === false) {
          delete state.executionByDataset[target];
        }
        return;
      }
      if (interrupt !== 'dependency_review') return;
      state.plan = {
        ...(state.plan || {}),
        dependency_review_status: payload.dependency_review_status || payload.status || payload.action || 'reviewed'
      };
      if (state.runProgress) {
        state.runProgress = {
          ...state.runProgress,
          dependency_review_status: state.plan.dependency_review_status,
          current_interrupt: payload.current_interrupt || null,
          review_queue: Array.isArray(state.runProgress.review_queue)
            ? state.runProgress.review_queue.filter((item) => String(item.name || item.interrupt || '') !== 'dependency_review')
            : state.runProgress.review_queue
        };
      }
    }

    function attachNativeResumeHandlers() {
      for (const button of document.querySelectorAll('[data-saved-graph-action]')) {
        if (button.dataset.savedGraphBound === '1') continue;
        button.dataset.savedGraphBound = '1';
        button.addEventListener('click', () => submitNativeResumeReview(
          button.dataset.savedGraphDataset,
          button.dataset.savedGraphAction,
          button.dataset.savedGraphInterrupt,
          button
        ));
      }
    }

    async function submitNativeResumeReview(dataset, decision, interruptName, button = null) {
      const target = String(dataset || state.selectedTarget || '').toUpperCase();
      const item = nativeResumeQueueItem(target, interruptName);
      const interrupt = normalizeNativeResumeInterruptName(interruptName);
      const allowedNow = nativeResumeItemAllowsDecision(item, interrupt, decision);
      if (!target || !decision || !item || !allowedNow) return;
      const restorePending = setPendingButtonGroup(
        button,
        `[data-saved-graph-dataset="${target}"][data-saved-graph-interrupt="${interrupt}"]`,
        'Recording...'
      );
      beginOperation('Saving saved-graph decision', `Recording ${titleFromToken(decision)} for ${target}. This updates durable LangGraph state before the next step.`);
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/datasets/${encodeURIComponent(target)}/native-resume`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({
            study_dir: studyDir(),
            reviewer: byId('reviewer').value.trim() || 'local_user',
            decision,
            notes: byId('reviewNotes').value.trim() || `Selected ${decision} from the saved graph resume gate.`,
            execute_after_approval: false,
            config_path: byId('configPath').value.trim() || null,
            rscript_path: byId('rscriptPath').value.trim() || null,
            ...llmOverridePayload()
          })
        });
        if (normalizeNativeResumeInterruptName(payload.interrupt) === 'code_review') {
          state.reviewByDataset[target] = {
            ...payload,
            approved: payload.decision === 'approve',
            native_resume: true
          };
          if (payload.decision === 'reject') {
            delete state.executionByDataset[target];
          }
        }
        if (normalizeNativeResumeInterruptName(payload.interrupt) === 'draft_spec_review') {
          state.draftSpecReviewByDataset[target] = {
            ...payload,
            approved: payload.decision === 'approve',
            native_resume: true
          };
          if (payload.decision === 'reject') {
            delete state.generatedByDataset[target];
            delete state.reviewByDataset[target];
            delete state.executionByDataset[target];
          }
        }
        if (normalizeNativeResumeInterruptName(payload.interrupt) === 'terminal_failure') {
          state.terminalFailureReviewByDataset[target] = {
            action: payload.decision,
            next_action: payload.next_action,
            current_interrupt: payload.current_interrupt,
            native_resume: true
          };
        }
        await refreshGraphReadModels();
        await loadReviewSummary(payload.run_id || runId(), {detailLevel: 'summary', refreshGraph: false});
        addEvent('Saved graph gate resumed', `${target}: ${titleFromToken(payload.decision)} -> ${titleFromToken(payload.next_action || payload.status)}.`);
        completeOperation('Saved graph gate resumed', `${target} next action: ${titleFromToken(payload.next_action || payload.status)}.`);
        renderDraftSpecPane();
        renderPane();
        renderGraphAwareDashboard();
        renderActionAvailability();
      } catch (error) {
        failOperation('Saved graph resume failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
    }

    function nativeResumeItemAllowsDecision(item, interruptName, decision) {
      if (!item || !Array.isArray(item.available_actions)) return false;
      const action = String(decision || '').trim();
      if (!nativeResumeActionAllowed(interruptName, action)) return false;
      return item.available_actions.some((available) => String(available.action || '').trim() === action);
    }

    async function submitTerminalFailureReview(dataset, action, button = null) {
      const target = String(dataset || state.selectedTarget || '').toUpperCase();
      if (!target || !action) return;
      const restorePending = setPendingButtonGroup(
        button,
        `[data-terminal-dataset="${target}"]`,
        'Recording...'
      );
      beginOperation('Saving failure decision', `Recording ${titleFromToken(action)} for ${target}. No repair or rerun starts until the graph accepts this decision.`);
      try {
        const payload = await api(`/runs/${encodeURIComponent(runId())}/graph-command`, {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: graphCommandRequestBody({
            dataset: target,
            interrupt: 'terminal_failure',
            action,
            notes: byId('reviewNotes').value.trim() || `Selected ${action} from the local UI terminal-failure triage.`
          })
        });
        state.terminalFailureReviewByDataset[target] = {
          action: payload.action,
          next_action: payload.next_action,
          current_interrupt: payload.current_interrupt,
          graph_command: true
        };
        await refreshGraphReadModels();
        addEvent('Failure decision recorded', `${target}: ${titleFromToken(payload.action)} -> ${titleFromToken(payload.next_action)}.`);
        completeOperation('Failure decision recorded', `${target} next action: ${titleFromToken(payload.next_action)}.`);
        renderPane();
        renderActionAvailability();
      } catch (error) {
        failOperation('Failure decision failed', error);
      } finally {
        restorePending();
        renderActionAvailability();
      }
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
    byId('startStudyLoopButton').addEventListener('click', startNativeStudyLoop);
    byId('approveDraftSpecButton').addEventListener('click', (event) => approveDraftSpec(event.currentTarget));
    byId('generateCodeButton').addEventListener('click', generateCode);
    byId('approveButton').addEventListener('click', (event) => approveCode(event.currentTarget));
    byId('runApprovedButton').addEventListener('click', runApprovedCode);
    byId('addTargetButton').addEventListener('click', addManualTarget);
    byId('openLlmSettingsButton').addEventListener('click', openLlmSettings);
    byId('closeLlmSettingsButton').addEventListener('click', closeLlmSettings);
    byId('llmSettingsModal').addEventListener('click', (event) => {
      if (event.target === byId('llmSettingsModal')) closeLlmSettings();
    });
    if (document.addEventListener) {
      document.addEventListener('keydown', (event) => {
        if (event.key === 'Escape' && !byId('llmSettingsModal').classList.contains('hidden')) {
          closeLlmSettings();
        }
      });
    }
    byId('modelMode').addEventListener('change', updateLlmModeControls);
    byId('testLlmButton').addEventListener('click', testLlmConnection);
    for (const button of document.querySelectorAll('[data-lang-option]')) {
      button.addEventListener('click', () => setLanguage(button.dataset.langOption));
    }
    for (const button of document.querySelectorAll('[data-upload-role]')) {
      button.addEventListener('click', () => uploadRole(button.dataset.uploadRole));
    }
    for (const button of document.querySelectorAll('[data-view]')) {
      button.addEventListener('click', async () => {
        state.selectedView = button.dataset.view;
        setActiveTab();
        if (state.selectedView === 'output') {
          beginOperation('Loading output review', 'Reading generated/reference table previews and downloadable artifact details.');
          await ensureReviewSummaryDetail('full');
          completeOperation('Output review loaded', 'Generated ADaM review details are ready.');
        } else if (state.selectedView === 'code' || state.selectedView === 'risk') {
          await ensureReviewSummaryDetail('summary');
        }
        renderPane();
      });
    }
    async function initializePage() {
      updateLlmModeControls();
      applyI18n();
      renderActionAvailability();
      updateHeaderStatusOverview();
      renderGraphAwareDashboard();
      renderPrimaryNextAction();
      await checkHealth();
      const restored = await restoreBrowserSession();
      if (!restored) {
        renderActionAvailability();
        updateHeaderStatusOverview();
        renderGraphAwareDashboard();
        renderPrimaryNextAction();
      }
    }

    initializePage();
  </script>
</body>
</html>
"""
