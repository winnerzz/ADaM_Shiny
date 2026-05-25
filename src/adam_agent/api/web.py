"""Static local web UI for Phase 8.2."""

from __future__ import annotations


INDEX_HTML = """<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>ADaM Agent Studio</title>
  <style>
    :root {
      color-scheme: light;
      --bg: #f6f8fb;
      --panel: #ffffff;
      --line: #d9e2ec;
      --text: #14212f;
      --muted: #5c6c7d;
      --accent: #0f766e;
      --accent-dark: #115e59;
      --danger: #b42318;
      --warn: #a15c07;
      --ok: #067647;
      --code: #0b1220;
    }
    * {
      box-sizing: border-box;
    }
    body {
      margin: 0;
      min-height: 100vh;
      font-family: Arial, Helvetica, sans-serif;
      color: var(--text);
      background: var(--bg);
    }
    header {
      border-bottom: 1px solid var(--line);
      background: var(--panel);
      padding: 14px 22px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
    }
    h1 {
      margin: 0;
      font-size: 20px;
      font-weight: 700;
    }
    .status-line {
      color: var(--muted);
      font-size: 13px;
    }
    main {
      display: grid;
      grid-template-columns: minmax(280px, 360px) minmax(360px, 1fr) minmax(360px, 1fr);
      gap: 14px;
      padding: 14px;
      height: calc(100vh - 58px);
    }
    section {
      background: var(--panel);
      border: 1px solid var(--line);
      border-radius: 8px;
      min-height: 0;
      display: flex;
      flex-direction: column;
    }
    .section-head {
      border-bottom: 1px solid var(--line);
      padding: 12px 14px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 10px;
    }
    h2 {
      margin: 0;
      font-size: 15px;
    }
    .section-body {
      padding: 14px;
      overflow: auto;
      min-height: 0;
    }
    label {
      display: block;
      margin: 0 0 6px;
      color: var(--muted);
      font-size: 12px;
      font-weight: 700;
    }
    input,
    select {
      width: 100%;
      height: 36px;
      border: 1px solid var(--line);
      border-radius: 6px;
      padding: 7px 9px;
      color: var(--text);
      background: #fff;
      font-size: 13px;
    }
    .field {
      margin-bottom: 12px;
    }
    .row {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 10px;
    }
    button {
      height: 36px;
      border: 1px solid var(--accent-dark);
      border-radius: 6px;
      padding: 0 12px;
      color: #fff;
      background: var(--accent);
      font-weight: 700;
      cursor: pointer;
    }
    button:hover {
      background: var(--accent-dark);
    }
    button.secondary {
      color: var(--text);
      border-color: var(--line);
      background: #fff;
    }
    button.secondary:hover {
      background: #edf3f8;
    }
    .button-row {
      display: flex;
      flex-wrap: wrap;
      gap: 8px;
      margin-top: 10px;
    }
    .summary-grid {
      display: grid;
      grid-template-columns: repeat(2, minmax(0, 1fr));
      gap: 10px;
      margin-bottom: 14px;
    }
    .metric {
      border: 1px solid var(--line);
      border-radius: 6px;
      padding: 10px;
      background: #fbfdff;
    }
    .metric span {
      display: block;
      color: var(--muted);
      font-size: 12px;
      margin-bottom: 5px;
    }
    .metric strong {
      display: block;
      overflow-wrap: anywhere;
      font-size: 14px;
    }
    .pill {
      display: inline-flex;
      align-items: center;
      min-height: 24px;
      border-radius: 999px;
      padding: 3px 9px;
      font-size: 12px;
      font-weight: 700;
      background: #e8f5f1;
      color: var(--ok);
    }
    .pill.fail {
      background: #fcebea;
      color: var(--danger);
    }
    .pill.warn {
      background: #fff4df;
      color: var(--warn);
    }
    table {
      width: 100%;
      border-collapse: collapse;
      font-size: 13px;
    }
    th,
    td {
      border-bottom: 1px solid var(--line);
      padding: 8px 6px;
      text-align: left;
      vertical-align: top;
      overflow-wrap: anywhere;
    }
    th {
      color: var(--muted);
      font-size: 12px;
    }
    pre {
      margin: 0;
      min-height: 260px;
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      border-radius: 6px;
      padding: 12px;
      color: #d8e2f0;
      background: var(--code);
      font-size: 12px;
      line-height: 1.45;
    }
    .muted {
      color: var(--muted);
      font-size: 13px;
    }
    @media (max-width: 1100px) {
      main {
        grid-template-columns: 1fr;
        height: auto;
      }
      section {
        min-height: 360px;
      }
    }
  </style>
</head>
<body>
  <header>
    <h1>ADaM Agent Studio</h1>
    <div class="status-line" id="health">Checking API...</div>
  </header>
  <main>
    <section>
      <div class="section-head">
        <h2>Run Setup</h2>
      </div>
      <div class="section-body">
        <div class="field">
          <label for="studyDir">Study Folder</label>
          <input id="studyDir" placeholder="D:\\path\\to\\PSY201">
        </div>
        <div class="row">
          <div class="field">
            <label for="runId">Run ID</label>
            <input id="runId">
          </div>
          <div class="field">
            <label for="targets">Targets</label>
            <input id="targets" value="ADAE">
          </div>
        </div>
        <div class="field">
          <label for="configPath">Config Path</label>
          <input id="configPath" value="studies\\_template\\configs\\mock_downstream.json">
        </div>
        <div class="field">
          <label for="executionMode">Execution Mode</label>
          <select id="executionMode">
            <option value="llm_downstream_provider">llm_downstream_provider</option>
            <option value="llm_downstream_r_sandbox">llm_downstream_r_sandbox</option>
            <option value="real_adsl_minimal">real_adsl_minimal</option>
            <option value="stub">stub</option>
          </select>
        </div>
        <div class="field">
          <label for="rscriptPath">Rscript Path</label>
          <input id="rscriptPath" placeholder="C:\\Dev\\R-4.5.2\\bin\\Rscript.exe">
        </div>
        <div class="field">
          <label for="approvedDeps">Approved Dependencies</label>
          <input id="approvedDeps" placeholder="ADSL, ADLB">
        </div>
        <button id="runButton">Run Study</button>
        <p class="muted">Runs are synchronous in Phase 8.2. The browser waits until the graph finishes.</p>
      </div>
    </section>
    <section>
      <div class="section-head">
        <h2>Run Summary</h2>
        <span id="runStatus" class="pill warn">idle</span>
      </div>
      <div class="section-body">
        <div class="summary-grid">
          <div class="metric"><span>Study</span><strong id="studyValue">-</strong></div>
          <div class="metric"><span>Run</span><strong id="runValue">-</strong></div>
          <div class="metric"><span>Mode</span><strong id="modeValue">-</strong></div>
          <div class="metric"><span>Dependency Review</span><strong id="reviewValue">-</strong></div>
        </div>
        <table>
          <thead>
            <tr>
              <th>Dataset</th>
              <th>Status</th>
              <th>Validation</th>
              <th>Route</th>
            </tr>
          </thead>
          <tbody id="datasetRows">
            <tr><td colspan="4" class="muted">No run yet.</td></tr>
          </tbody>
        </table>
        <div class="button-row">
          <button class="secondary" data-artifact="dependency">Dependency</button>
          <button class="secondary" data-artifact="validation">Validation</button>
          <button class="secondary" data-artifact="diagnostics">Diagnostics</button>
          <button class="secondary" data-artifact="audit">Audit</button>
          <button class="secondary" data-artifact="context">Context</button>
        </div>
      </div>
    </section>
    <section>
      <div class="section-head">
        <h2>Artifact JSON</h2>
      </div>
      <div class="section-body">
        <pre id="artifactView">Select a run artifact.</pre>
      </div>
    </section>
  </main>
  <script>
    const state = {
      lastRun: null,
      selectedDataset: null
    };

    const byId = (id) => document.getElementById(id);

    function defaultRunId() {
      const stamp = new Date().toISOString().replace(/[-:T.Z]/g, '').slice(0, 14);
      return `run_ui_${stamp}`;
    }

    byId('runId').value = defaultRunId();

    async function checkHealth() {
      try {
        const response = await fetch('/health');
        const payload = await response.json();
        byId('health').textContent = payload.status === 'ok' ? 'API ready' : 'API unavailable';
      } catch (error) {
        byId('health').textContent = 'API unavailable';
      }
    }

    function splitValues(value) {
      return value.split(',').map((item) => item.trim().toUpperCase()).filter(Boolean);
    }

    function payloadFromForm() {
      const rscriptPath = byId('rscriptPath').value.trim();
      const configPath = byId('configPath').value.trim();
      return {
        study_dir: byId('studyDir').value.trim(),
        run_id: byId('runId').value.trim(),
        target_datasets: splitValues(byId('targets').value),
        config_path: configPath || null,
        execution_mode: byId('executionMode').value,
        approved_dependency_datasets: splitValues(byId('approvedDeps').value),
        rscript_path: rscriptPath || null
      };
    }

    function setStatus(status) {
      const pill = byId('runStatus');
      pill.textContent = status;
      pill.className = 'pill';
      if (status === 'failed') {
        pill.classList.add('fail');
      } else if (status === 'idle' || status === 'running' || status === 'warning') {
        pill.classList.add('warn');
      }
    }

    function renderRun(payload) {
      state.lastRun = payload;
      state.selectedDataset = payload.dataset_results?.[0]?.dataset || null;
      byId('studyValue').textContent = payload.study_id || '-';
      byId('runValue').textContent = payload.run_id || '-';
      byId('modeValue').textContent = payload.execution_mode || '-';
      byId('reviewValue').textContent = payload.dependency_review_status || '-';
      setStatus(payload.status || 'unknown');

      const rows = byId('datasetRows');
      rows.innerHTML = '';
      const results = payload.dataset_results || [];
      if (!results.length) {
        rows.innerHTML = '<tr><td colspan="4" class="muted">No dataset result.</td></tr>';
        return;
      }
      for (const result of results) {
        const route = result.metadata?.recommended_route || result.metadata?.summary_status_note || '-';
        const tr = document.createElement('tr');
        tr.innerHTML = `
          <td>${result.dataset}</td>
          <td>${result.status}</td>
          <td>${result.validation_status || '-'}</td>
          <td>${route}</td>
        `;
        tr.addEventListener('click', () => {
          state.selectedDataset = result.dataset;
          for (const row of rows.querySelectorAll('tr')) {
            row.style.background = '';
          }
          tr.style.background = '#edf8f6';
        });
        rows.appendChild(tr);
      }
    }

    async function runStudy() {
      const payload = payloadFromForm();
      if (!payload.study_dir || !payload.run_id || !payload.target_datasets.length) {
        byId('artifactView').textContent = 'Study folder, run id, and target dataset are required.';
        return;
      }
      setStatus('running');
      byId('artifactView').textContent = 'Running...';
      try {
        const response = await fetch('/runs', {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify(payload)
        });
        const result = await response.json();
        if (!response.ok) {
          setStatus('failed');
          byId('artifactView').textContent = JSON.stringify(result, null, 2);
          return;
        }
        renderRun(result);
        byId('artifactView').textContent = JSON.stringify(result, null, 2);
      } catch (error) {
        setStatus('failed');
        byId('artifactView').textContent = String(error);
      }
    }

    async function loadArtifact(kind) {
      const run = state.lastRun;
      if (!run) {
        byId('artifactView').textContent = 'Run a study first.';
        return;
      }
      const studyDir = encodeURIComponent(byId('studyDir').value.trim());
      const runId = encodeURIComponent(run.run_id);
      const dataset = encodeURIComponent(state.selectedDataset || run.dataset_results?.[0]?.dataset || 'ADAE');
      let url = '';
      let options = {};
      if (kind === 'dependency') {
        url = `/runs/${runId}/dependency-plan?study_dir=${studyDir}`;
      } else if (kind === 'validation') {
        url = `/runs/${runId}/datasets/${dataset}/validation?study_dir=${studyDir}`;
      } else if (kind === 'diagnostics') {
        url = `/runs/${runId}/datasets/${dataset}/diagnostics?study_dir=${studyDir}`;
      } else if (kind === 'audit') {
        url = `/runs/${runId}/audit-manifest?study_dir=${studyDir}`;
      } else {
        url = `/runs/${runId}/artifacts/read?study_dir=${studyDir}`;
        options = {
          method: 'POST',
          headers: {'Content-Type': 'application/json'},
          body: JSON.stringify({relative_path: `llm/${String(dataset).toLowerCase()}_context.json`})
        };
      }
      const response = await fetch(url, options);
      const payload = await response.json();
      byId('artifactView').textContent = JSON.stringify(payload, null, 2);
    }

    byId('runButton').addEventListener('click', runStudy);
    for (const button of document.querySelectorAll('[data-artifact]')) {
      button.addEventListener('click', () => loadArtifact(button.dataset.artifact));
    }
    checkHealth();
  </script>
</body>
</html>
"""
