# LangGraph-v2 最近三天交接工作日志

生成时间：2026-06-04  
仓库：`D:\Archive\Research\Projects\ADaM_Shiny_LangGraph`  
分支：`LangGraph-v2`  
统计范围：最近三天 git commits，约从 `2026-06-01 12:27 +08:00` 到 `2026-06-04 02:43 +08:00`，并追加了后续提交前整理记录  
当前远程状态：以 `git status --short --branch` 为准；生成本文时本地分支仍有未提交整理改动  
当前未跟踪文件：`docs/assets/adam-agent-studio-global-architecture.png`，尚未被文档引用，暂不建议纳入产品代码提交

## 一句话交接结论

这三天的主要工作不是简单做 UI 或补几个接口，而是把原来由 FastAPI service 分步骤控制的流程，逐步收拢到 LangGraph 原生流程里：图负责保存状态、触发人工审核、恢复中断、处理依赖、读取产物，UI/API 变成图流程的入口和展示层。

现在系统已经有可运行的本地 FastAPI + HTML UI，支持 study 文件上传/扫描、目标 ADaM 选择、无 spec 时 draft spec 审核、LLM 生成 R code、人工审核、R 沙盒执行、结果预览/下载、reference ADaM compare，以及 `.sas7bdat` 参考数据读取支持。当前仍要重点收尾的是：compare 差异后的质量闸门、UI 产品化、静态规则层、真实系统级 R 沙盒隔离。

补充：后续 UI 收敛工作已经把产品主路径进一步改成 native-first。浏览器主按钮优先进入 `/runs/native-study-loop`；单 dataset 生成和 `revise_approved_spec` 都进入 `/native-full-run`；本地 R 执行只允许 `/native-full-run/execute`。旧 split-flow endpoints 仍保留为兼容/API 回归路径，但不再是普通 UI 主路径。

## 当前架构快照

用户看到的是本地网页 UI。UI 不直接决定临床生成逻辑，而是通过 FastAPI 调接口。

当前主路径是：

```text
HTML UI
  -> FastAPI endpoints
  -> service 层
  -> graph gateway
  -> LangGraph study/dataset flow
  -> LLM 代码生成 / R sandbox / compare 工具
  -> run artifacts + graph checkpoint
  -> UI 读取进度、审核点、输出、比较结果
```

核心原则：

- 用户提供 input spec 时，系统优先使用 input spec；没有 spec 时才生成 draft spec。
- draft spec 和 generated R code 都要经过人工审核闸门。
- reference ADaM 只能用于输出形态参考和最终 compare，不能反推或决定衍生逻辑。
- 图状态和 run artifact 是主事实来源，service/UI 不应该私自复用过期路径。
- 缺少 graph progress、graph state 损坏、checkpoint 不可用、终端失败等情况都尽量 fail closed，也就是明确失败，不静默继续。

## 最近三天完成的主线

### 1. LangGraph 原生中断与恢复

完成了 native resume 的基本能力：图运行到人工审核点时暂停，UI/API 可以看到可执行动作，再把用户动作送回图继续运行。

相关工作包括：

- 新增 native resume endpoint 和 progress 字段。
- 将 resume 和 active checkpoint runtime 绑定，避免拿旧 checkpoint 继续跑。
- 暴露 native resume queue 和队列数量。
- 区分 full-run 兼容恢复与 native interrupt 恢复。
- 修复 draft spec review、code review、repair follow-up、terminal failure 后的继续运行契约。

### 2. UI/API 从 service gate 向 graph command boundary 收拢

此前 UI 很多按钮是在 service 层直接做业务动作。现在逐步改成：UI 只发命令，命令经过 service/gateway 进入图，由图决定状态变化。

相关工作包括：

- 增加 graph command boundary。
- dependency review 改为 graph command。
- UI review actions 改为 graph command。
- full-run code review / draft review 改为 native resume。
- LG3 full-run execution 改为 native endpoint。
- service 每次操作后关闭 graph gateway，降低状态串扰风险。
- UI 主行动面板改成 native-first：优先 `Start Runnable Datasets`，`finalize-inputs` 只作为手动兼容检查。
- UI 的 `revise_approved_spec` 重新进入 `/native-full-run`，不再直连 `/draft-spec`。
- UI 的 `Run Approved Code` 必须有 LG3/native full-run execution contract，否则 fail closed，不再自动回退 `/execute-approved-code`。

### 3. 多 dataset / 依赖 / run artifact 合同

系统开始把“生成某个 ADaM”看作 study 级流程中的一个 dataset 子任务，而不是孤立按钮。

相关工作包括：

- study loop 通过 LG3 full-run dispatch 进入图流程。
- native study loop 会检查运行时依赖输出。
- 显式传递 run directory，规范依赖产物路径。
- 保护 graph-owned run artifacts 和 dataset artifact reads。
- generated output reads 以 graph state 为准。
- 对缺失或损坏 graph progress / graph state 的 UI 操作 fail closed。

### 4. UI 可用性和产品流程改进

最近一个本地 commit 主要改善 UI 对用户的解释能力。

已做：

- 增加明显的 `Next Step` 面板，让用户知道下一步该点什么。
- demo 默认不再直接跳到容易触发依赖困惑的 ADAE，而是展示 `ADSL + ADAE`，默认 ADSL。
- dependency map 改为普通语言解释。
- compare 面板改为可读的 verdict 和指标。
- demo load 后不直接刷新成混乱进度，而是让用户明确继续动作。
- input 文件摘要更偏向用户语言，不只是 JSON。

### 5. `.sas7bdat` 与 reference ADaM compare

针对 PSY201 和真实 demo 文件形态，补强了 `.sas7bdat` 读取与比较：

- reference ADaM `.sas7bdat` 可以通过本地 R/haven reader 读取。
- compare 支持 `.sas7bdat` reference。
- compare 列名大小写不一致时可匹配。
- R sandbox 环境 allowlist 增加 `PROCESSOR_ARCHITECTURE`，避免某些 R/haven 启动失败。

注意：`.sas7bdat` 的读取依赖本地 R 和 `haven` 包。没有 `haven` 时，系统能扫描文件存在，但不能完整 profile。

### 6. LLM 代码生成和容错

补强了 LLM 输出解析：

- generated-code metadata 中非 list / 非 string 字段不会直接硬失败，而是规整为 warnings。
- 这能减少 LLM 返回格式轻微偏差导致整步中断。

这不代表生成代码质量已经合格。真实质量仍由 code review、R 执行、compare、后续静态规则共同判断。

## 关键文件和职责

### `src/adam_agent/api/web.py`

本地 HTML UI 的主要实现文件。最近大量修改集中在这里。

当前职责：

- 展示 study 文件状态、target 选择、下一步提示。
- 发起 demo load、scan inputs、graph run、resume、review、compare。
- 展示 dependency map、execution cards、compare summary、输出预览。

风险：

- 目前 UI 仍是单文件 HTML/JS 字符串，不是成熟前端工程。
- 可以继续改，但长期应拆成真正的前端模块。

### `src/adam_agent/api/app.py`

FastAPI 路由入口。

当前职责：

- 暴露 native full-run API。
- 暴露 native resume / graph command 相关 endpoint。
- 连接 service 层。

### `src/adam_agent/api/service.py`

API 背后的应用服务层。

当前职责：

- study workspace 读写。
- 文件扫描和输入摘要。
- 调用 graph gateway。
- 审核摘要、产物读取、compare、输出下载等服务入口。

这三天的方向是：service 逐步变薄，避免它绕过图直接改变业务状态。

### `src/adam_agent/api/models.py`

FastAPI 请求/响应模型。

当前职责：

- 定义 native resume、full-run、graph command、progress/read model 的数据结构。
- 给 UI/API 一份稳定合同。

### `src/adam_agent/graph/gateway.py`

LangGraph 与 service/API 的关键边界。

当前职责：

- 创建和关闭 graph runtime。
- 管理 checkpointer。
- 启动 full-run。
- 处理 native interrupt resume。
- 从 graph progress 恢复 UI 所需状态。
- 判断 run artifact 是否属于图拥有。
- 对损坏状态 fail closed。

这是当前最关键的工程文件之一。

### `src/adam_agent/graph/dataset_graph.py`

dataset 级图流程。

当前职责：

- draft spec、code generation、review、sandbox、repair、terminal failure 等 dataset 节点。
- 最近补了 native dataset-loop interrupt 相关状态处理。

### `src/adam_agent/llm/generated_code.py`

LLM 生成 R code 的解析和结构化包装。

当前职责：

- 从模型响应中提取 code、metadata、warnings。
- 对轻微结构不规范的 metadata 做容错。

### `src/adam_agent/tools/sandbox.py`

R 执行沙盒。

当前职责：

- 调用本地 Rscript 执行生成的 R code。
- 管理运行目录和环境变量 allowlist。

注意：当前是应用层路径约束，不是容器级沙盒。

### `src/adam_agent/tools/compare.py`

输出数据与 reference ADaM 的比较工具。

当前职责：

- 读取 generated output。
- 读取 reference ADaM，包括 `.sas7bdat` reference reader。
- 做列名、行数、变量差异等比较。

下一步要加强的是 compare mismatch 后的质量闸门，而不仅是展示差异。

### `tests/test_api_phase8.py`

Phase 8 API/UI 行为测试。

覆盖：

- UI 默认 target 行为。
- demo load 行为。
- scan inputs read model。
- native flow endpoint。
- graph command / review action 行为。
- compare endpoint 和 `.sas7bdat` reference 支持。

### `tests/test_graph_gateway.py`

Graph gateway 的核心契约测试。

覆盖：

- checkpoint/runtime 绑定。
- native resume。
- full-run dispatch。
- dependency gate。
- graph-owned artifact 读取。
- corrupt graph state fail closed。
- terminal failure boundary。

### `tests/test_llm_generated_code.py`

LLM generated code 解析容错测试。

### `tests/test_tools_phase4.py`

工具层测试，包括 compare `.sas7bdat` reference reader。

## 最近三天 commit 明细

| 日期时间 | Commit | 说明 |
|---|---:|---|
| 2026-06-04 02:43 | `087b934` | Improve graph-driven UI workflow |
| 2026-06-03 21:09 | `234cbfe` | Fix native UI flow checkpoint dependency and execution contract |
| 2026-06-03 20:35 | `3f9469f` | Clarify native product flow entrypoints |
| 2026-06-03 18:14 | `556cfe0` | Route LG3 full-run execution through native endpoint |
| 2026-06-03 17:37 | `86e1c69` | Clarify graph review endpoint boundaries |
| 2026-06-03 16:22 | `e327aca` | Route dependency review through graph command |
| 2026-06-03 15:01 | `0b1fa2e` | Route UI review actions through graph command |
| 2026-06-03 13:18 | `752e7c9` | Route full-run draft review through native resume |
| 2026-06-03 12:53 | `2e5fbd6` | Route full-run code review through native resume |
| 2026-06-03 12:15 | `9fe4416` | Add graph command boundary and local sqlite default |
| 2026-06-03 00:59 | `5325b8b` | Separate native interrupt resume from full-run compatibility |
| 2026-06-02 23:33 | `3ad2534` | Expose gated native interrupt resume actions |
| 2026-06-02 18:46 | `3cb4a89` | Explain native resume reasons in UI |
| 2026-06-02 18:40 | `c349487` | Explain native resume binding state |
| 2026-06-02 18:34 | `dd77598` | Fail closed native resume at service boundary |
| 2026-06-02 18:28 | `6d82182` | Clarify LG3 full-run resume actionability |
| 2026-06-02 18:03 | `70d2f49` | Cover bound native interrupt full-run metadata |
| 2026-06-02 17:56 | `f62a1fa` | Clarify LG3 full-run resume capability |
| 2026-06-02 14:45 | `0341754` | Normalize native resume checkpoint paths |
| 2026-06-02 14:34 | `ee491bc` | Bind native resume to active checkpoint runtime |
| 2026-06-02 14:09 | `3c9d489` | Recover LG3 contract from progress read model |
| 2026-06-02 13:47 | `742d960` | Recover UI requested targets from graph progress |
| 2026-06-02 13:30 | `d635f82` | Preserve LG3 contract through explicit execution |
| 2026-06-02 13:16 | `d87e39a` | Route LG3 review approvals through full-run resume |
| 2026-06-02 12:44 | `3dae802` | Route study loop through LG3 full-run dispatch |
| 2026-06-02 11:01 | `142cc58` | Route UI generation through LG3 native full-run |
| 2026-06-01 23:18 | `6ca9655` | Expose LG3 native full-run API entry |
| 2026-06-01 22:59 | `14f20bf` | Preserve LG3 follow-up context through resume |
| 2026-06-01 22:29 | `3657137` | Preserve LG3 repair follow-up contract |
| 2026-06-01 22:10 | `96644d7` | Record LG3 terminal failure boundary |
| 2026-06-01 21:55 | `066e24f` | Continue LG3 full-run after draft spec review |
| 2026-06-01 21:33 | `5525857` | Preserve LG3 full-run contract through native resume |
| 2026-06-01 21:21 | `202ac48` | Harden LG3 native full-run contract coverage |
| 2026-06-01 21:14 | `772a78f` | Add LG3 native dataset full-run contract |
| 2026-06-01 20:54 | `7ba5c37` | Document LangGraph v2 closeout audit |
| 2026-06-01 20:46 | `0957294` | Expose native resume queue status in study loop response |
| 2026-06-01 20:29 | `6825897` | Show native resume queue status in study loop UI |
| 2026-06-01 20:23 | `f38de81` | Mirror native resume queue counts in study loop result |
| 2026-06-01 20:12 | `fd41e7f` | Expose native resume queue item counts |
| 2026-06-01 20:05 | `b70ffc4` | Clarify durable native resume gate messaging |
| 2026-06-01 19:55 | `51f5c07` | Align native resume queue with graph gates |
| 2026-06-01 19:43 | `7406179` | Expose native resume interrupt queue |
| 2026-06-01 19:07 | `52e0d6d` | Harden run artifact path resolution |
| 2026-06-01 18:51 | `3e2fb94` | Pass explicit run directory to dependency gate |
| 2026-06-01 18:38 | `66c051f` | Normalize runtime dependency artifact paths |
| 2026-06-01 18:29 | `528328d` | Gate native study loop on runtime dependency outputs |
| 2026-06-01 18:04 | `98daf37` | Guard graph-owned run artifacts |
| 2026-06-01 17:42 | `671e148` | Guard graph-owned dataset artifact reads |
| 2026-06-01 17:16 | `ba664fb` | Align generated output reads with graph state |
| 2026-06-01 16:43 | `1457ab1` | Fail closed compare on corrupt graph state |
| 2026-06-01 16:31 | `cd6d8e1` | Fail closed review summary on corrupt graph state |
| 2026-06-01 16:24 | `e51e601` | Fail closed UI actions on missing graph progress |
| 2026-06-01 16:10 | `5ebc598` | Gate terminal failure UI actions on graph state |
| 2026-06-01 15:41 | `1fb458c` | Centralize primary action availability UI |
| 2026-06-01 15:10 | `162c155` | Cover graph-state-only code review recovery |
| 2026-06-01 14:51 | `c160201` | Recover code review UI from graph-gated summary |
| 2026-06-01 14:23 | `53c3f11` | Explain preserved native study loop progress |
| 2026-06-01 13:49 | `53418f0` | Centralize UI graph state application |
| 2026-06-01 13:43 | `3963a29` | Centralize UI progress application |
| 2026-06-01 13:37 | `967fc61` | Guard native resume UI boundary |
| 2026-06-01 13:29 | `2e520da` | Align native resume progress fields |
| 2026-06-01 13:18 | `9a4e613` | Expose native resume status in progress |
| 2026-06-01 13:09 | `b36aa29` | Add fail-closed native resume endpoint |
| 2026-06-01 12:52 | `6d659cd` | Expose native study loop resume boundary |
| 2026-06-01 12:37 | `19894d5` | Guard unavailable service checkpointer backends |
| 2026-06-01 12:27 | `ebcc27c` | Close service graph gateways per operation |

## 已验证过的测试

最近一次已知验证包括：

```powershell
python -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae tests.test_api_phase8.Phase8ApiTests.test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected tests.test_api_phase8.Phase8ApiTests.test_index_demo_load_does_not_refresh_progress_directly tests.test_api_phase8.Phase8ApiTests.test_index_scan_inputs_refreshes_graph_read_models tests.test_api_phase8.Phase8ApiTests.test_demo_study_endpoint_prepares_shiny_demo_shape tests.test_api_phase8.Phase8ApiTests.test_study_inputs_endpoint_returns_human_oriented_file_summary -v
```

```powershell
python -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_supports_sas7bdat_reference_when_graph_state_exists tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_reference_output_supports_sas7bdat_reference_reader tests.test_tools_phase4.Phase4ToolTests.test_compare_supports_sas7bdat_reference_through_injected_reader -v
```

```powershell
python -m py_compile src\adam_agent\api\web.py src\adam_agent\api\service.py
```

提交前整理阶段新增验证：

```powershell
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_generate_button_handles_native_full_run_draft_gate tests.test_api_phase8.Phase8ApiTests.test_index_generate_revise_spec_uses_native_full_run_not_draft_spec_endpoint tests.test_api_phase8.Phase8ApiTests.test_index_native_full_run_draft_gate_clears_stale_generated_code -v
```

```powershell
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway tests.test_llm_generated_code tests.test_tools_phase4 -v
```

结果：396 tests OK。

浏览器层面也做过本地 UI 核查：

- `http://127.0.0.1:8000/health` 返回 `{"status":"ok"}`。
- 初始页面提示加载 demo 或上传 study 文件。
- `Load Demo` 后默认 target 是 `ADSL`。
- 下一步提示显示为确认 ADSL input spec。
- 未观察到明显 JS console error。

## 当前未完成事项

### 1. Compare 差异还没有成为强质量闸门

现在系统能展示 compare verdict 和指标，但如果 generated output 与 reference ADaM 明显不一致，流程还不够严格。

下一步应做：

- compare mismatch 后不要简单显示 done。
- UI 提供明确动作：接受差异、请求 code repair、退回 draft spec、记录 warning 后继续。
- 下游依赖如果使用了 compare mismatch 的上游输出，要被标记为有风险，不能静默当作合格输入。

### 2. UI 还不是成熟前端产品

当前 UI 逻辑集中在 `web.py` 中，已经比之前清楚，但仍不适合长期维护。

下一步应做：

- 把 UI 状态、API client、组件渲染拆开。
- 继续减少 JSON 暴露。
- 把用户最关心的三件事放清楚：我现在在哪一步、为什么卡住、下一步能做什么。

### 3. 静态规则层仍是基础版

现在静态规则还不是完整 CDISC/P21 规则引擎。

下一步应做：

- 保留通用规则接口。
- 规则应按 dataset/variable/spec/output 的抽象结构写，不要写 demo 专用判断。
- 先做最低限度规则：必须变量、变量类型、关键日期、重复 key、明显空输出、输出列与 spec 不一致。

### 4. R sandbox 还不是生产级隔离

当前 R sandbox 主要靠 run directory 和环境变量控制。

下一步应做：

- 生产环境应使用容器、权限隔离、网络禁用、CPU/内存/时间限制。
- 当前本地 demo 可以继续用应用层沙盒，但文档和 UI 不能把它说成生产级安全沙盒。

### 5. 多智能体还处在图节点协作阶段

现在 LangGraph 的价值主要体现在状态、暂停、恢复、依赖和审计上。它还不是完整的多个专家 agent 自主协作系统。

下一步应做：

- 把角色明确拆成 spec drafter、code generator、static reviewer、runtime debugger、compare reviewer。
- 每个 agent 要有独立输入、输出、置信度、证据和可审核记录。
- 不要让一个大 prompt 同时做所有判断。

## 接手者建议阅读顺序

1. `docs/langgraph_2_construction_plan_zh.md`
2. `docs/langgraph_v2_closeout_audit.md`
3. `docs/phase8_1_api_contract.md`
4. `src/adam_agent/graph/gateway.py`
5. `src/adam_agent/api/service.py`
6. `src/adam_agent/api/web.py`
7. `src/adam_agent/graph/dataset_graph.py`
8. `tests/test_graph_gateway.py`
9. `tests/test_api_phase8.py`

## 接手前检查命令

```powershell
git status --short --branch
git log --oneline --decorate origin/LangGraph-v2..HEAD
python -m unittest tests.test_api_phase8 tests.test_graph_gateway tests.test_llm_generated_code tests.test_tools_phase4 -v
```

启动本地 UI：

```powershell
python -m uvicorn adam_agent.api.app:create_app --factory --host 127.0.0.1 --port 8000
```

如果本地不是从项目根目录启动，需要先进入：

```powershell
cd D:\Archive\Research\Projects\ADaM_Shiny_LangGraph
```

## 最重要的交接提醒

- 不要再把 UI 按钮做成绕过 LangGraph 的独立 service 流。
- 不要让 reference ADaM 参与推导逻辑，它只能做比较和输出形态证据。
- 不要把 mock run 和真实 LLM run 混在一起当作同等质量结果。
- 不要让缺少 graph state 的路径静默继续。
- 不要把 `.sas7bdat` 支持等同于完整 SAS 项目理解；SAS 程序解析和 spec draft 仍要经过审核。
- 不要把“R code 跑通”说成“临床逻辑正确”；正确性至少还需要 spec 审核、静态规则、reference compare 和人工确认。
