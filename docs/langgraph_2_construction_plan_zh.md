# LangGraph-2 第二阶段施工方案

这份文档是 ADaM Agent Studio 在 Phase 0-8 MVP 之后的第二阶段施工方案。它把目前已经发现的架构偏差和未完成事项，整理成一套系统性工程计划。

LangGraph-2 的核心原则：

> 不要在现有 FastAPI service flow 旁边再叠一层补丁。要把真实业务流程迁入一个由 LangGraph 控制的状态机，让 FastAPI/UI 变成这个状态机的入口和展示层。

## 1. 当前基础

当前已经有价值的资产：

- `src/adam_agent/api/service.py` 里有目前最完整的真实流程：依赖计划、确认输入、draft spec、draft spec 审核、生成代码、代码审核、执行已审核 R、验证、比较、下载。
- `src/adam_agent/graph/study_graph.py` 已有 study 级依赖计划、dataset 批次、分发、汇总和 study audit manifest。
- `src/adam_agent/graph/dataset_graph.py` 已有 dataset 图骨架、LLM downstream 执行模式、失败路由和结果汇总。
- `src/adam_agent/graph/workflow_state.py` 目前会持久化 `workflow_state.json` 和 SQLite sidecar checkpoint history，供当前 UI/API 流程使用。
- `src/adam_agent/llm/`、`src/adam_agent/downstream/`、`src/adam_agent/tools/` 已经形成了可复用的工具边界。
- ADSL 已修正为和其他 AD target 一样走统一 ADaM 产品流程。旧的 `src/adam_agent/adsl/` 只作为 legacy/regression 保留。

当前架构偏差：

- 真实 human-in-the-loop 流程主要还在 FastAPI service 函数里，不在 LangGraph `interrupt` 节点里。
- `workflow_state.json` 仍是当前产品状态源；LangGraph checkpointer 还不是单一事实来源。
- `DatasetGraph` 里仍有早期 `*_stub` 节点，图还不是完整产品工作流。
- 当前产品更像“受控流水线 + LLM 调用”，还不是明确专家角色分工的多智能体图。
- UI 的 dataset card 和 target 切换仍更像单 target 控制器，不像多个持久 dataset run 的图状态视图。
- Static ADaM/CDISC 检查还是 placeholder，不是规则引擎。
- R 执行仍是本地 `Rscript` 加应用层路径约束，不是强化沙盒。

未完成的产品能力：

- 进程重启后的 graph-native checkpoint/resume。
- 依赖审核、draft spec 审核、code review、terminal failure triage 的 graph-native interrupt。
- UI 中可见的多 dataset 编排和持久 dataset card。
- Evidence、Spec、Code、Validation、Repair、Reference、Audit 等 agent 角色分离。
- CDISC/P21/company standard 检索工具。
- 生产级验证、安全和部署控制。

## 2. 目标架构

LangGraph-2 应该收敛到下面这个形态：

```text
FastAPI/UI
  -> GraphGateway
    -> StudyGraph
      -> scan_inputs
      -> plan_dependencies
      -> 必要时 interrupt dependency_review
      -> 按 runnable target 分发 DatasetGraph
      -> 汇总结果
      -> 写 study audit

DatasetGraph(target)
  -> prepare_context
  -> 如果 input_spec 存在：use_input_spec
     否则：draft_spec_agent -> interrupt draft_spec_review
  -> code_agent
  -> static_rule_check
  -> interrupt code_review
  -> r_sandbox_runner
  -> validate_output
  -> compare_reference_when_available
  -> diagnose_failure
  -> route repair_code / revise_spec / terminal_failure
  -> dataset_audit
```

FastAPI 不应该拥有业务工作流状态。它应该只负责：

- 创建 graph run。
- 把用户命令发送给 graph interrupt。
- 读取 graph state 和 artifacts 给 UI 展示。
- 暴露 artifact preview/download/compare 端点。
- 在迁移期间保留 legacy endpoint 作为兼容 shim。

Graph state 应该成为 durable source of truth。`workflow_state.json` 可以继续保留为 UI 友好的只读投影，但必须由 graph state 生成，不能继续作为独立状态机维护。

## 3. 核心设计原则

1. 保留已经可用的工具边界。

   继续复用已有 scanner、LLM client、prompt compaction、generated-code parser、downstream runner、R runner、validation、diagnostics 和 artifact writing。不要为了改编排而重写底层工具。

2. 把编排迁入 LangGraph，而不是把所有工具重写成 LangGraph。

   `api/service.py` 里的 service 函数应该被拆成 graph nodes 或 node-callable tools。FastAPI 应该变薄。

3. 所有 ADaM target 一视同仁。

   ADSL、ADAE、ADCM、ADLB 以及未来 ADxx target 必须使用同一份图契约：spec -> code -> review -> execute -> validate。Dataset-specific 逻辑属于 spec、standards tools 或 generated code，不属于 graph 特殊分支。

4. 人工审核必须是显式 interrupt，而不是 UI 约定。

   图必须在审核点停止，并且只能用已记录的人类决策 payload 恢复。

5. 多智能体意味着有审计状态的专家节点。

   不要创建没有 artifact 的自由对话 agent。每个 agent node 必须有输入、输出、risk flags 和 artifact 记录。

6. Reference ADaM 是证据，不是权威。

   Reference ADaM 可以支持输出形态比较和依赖可用性判断，但不能静默决定推导逻辑。

7. 在验证能力完成前禁止生产级宣称。

   CDISC/P21 检查、强化沙盒、submission-grade ADaM 正确性都是 Phase 9+ hardening 目标，不应由 LangGraph-2 暗示已经完成。

## 4. Phase LG2.0 - 架构锁定与状态契约

目标：

在迁移代码之前，先定义 graph-native state contract。

任务：

- 创建 canonical `StudyRunState` 和 `DatasetRunState` schema 或 TypedDict，覆盖当前 `workflow_state.json` 字段：
  - study/run id
  - target datasets
  - dependency plan 和 decisions
  - current interrupt
  - input fingerprint
  - 每个 dataset 的 spec/code/review/execution 状态
  - artifact references
  - failure records
  - compare 和 validation summaries
- 定义允许的 interrupt 名称：
  - `dependency_review`
  - `draft_spec_review`
  - `code_review`
  - `terminal_failure`
  - `dependency_user_action_required`
- 定义用户命令 payload：
  - approve/reject dependency plan
  - approve/reject draft spec
  - approve/reject generated code
  - terminal failure 后 retry/revise
  - approve system generation of missing dependency dataset
- 定义状态投影规则：
  - graph state -> `workflow_state.json`
  - graph state -> UI review summary
  - graph state -> audit manifest
- 为后续 multi-agent 预留字段，避免之后又做破坏性状态迁移：
  - `agent_decisions`
  - `risk_flags`
  - `evidence_bundle_id`
  - `reference_queries`

可能涉及文件：

- `src/adam_agent/graph/state.py`
- `src/adam_agent/schemas/states.py`
- `src/adam_agent/schemas/approval.py`
- `src/adam_agent/graph/workflow_state.py`
- `docs/state_model.md`
- `tests/test_state_schemas.py`

退出标准：

- 一个测试可以序列化/反序列化包含两个 datasets 和两个不同 interrupts 的 graph state。
- `workflow_state.json` 被文档明确为投影，而不是独立产品状态源。
- 状态投影测试验证 graph checkpoint state 和 `workflow_state.json` 在 dataset status、current interrupt、artifact references 上一致。

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - 新增 canonical `StudyRunState`、`DatasetRunState`、`InterruptState` 和 `HumanCommand` schema。
  - 新增 graph state 到 `workflow_state.json` 的投影和一致性检查。
  - 为 risk flags、agent decisions、evidence bundle、reference queries 预留字段。
- 仍待完成：
  - 把剩余由 service 直接维护的状态跳转全部迁到 graph-owned state update。

## 5. Phase LG2.1 - GraphGateway 与原生 Checkpointing

目标：

引入一个统一 gateway，负责 graph invocation、resume 和 state read。

任务：

- 新增 `GraphGateway` 模块，负责：
  - 使用 SQLite checkpointer 编译本地 StudyGraph
  - 启动 run
  - 用用户命令恢复 interrupt
  - 读取当前 graph state
  - 为 UI 兼容写入 `workflow_state.json` 投影
- 先增加 graph-native 内部入口，再逐个迁移现有 FastAPI endpoints 调用 gateway。不要一次性翻转所有 endpoint。
- 停止在 `api/service.py` 里新增独立状态跳转。
- 现有 SQLite sidecar 只作为兼容/debug projection，直到 LangGraph checkpointer 替代它。
- 添加 restart tests：
  - start run -> 命中 draft spec interrupt
  - 重新创建 app/gateway
  - 用 approval resume
  - 继续到 code generation

可能涉及文件：

- `src/adam_agent/graph/gateway.py` 新增
- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/graph/workflow_state.py`
- `src/adam_agent/api/service.py`
- `src/adam_agent/api/app.py`
- `tests/test_graph_gateway.py` 新增
- `tests/test_api_phase8.py`

退出标准：

- 用户审核决策通过 LangGraph checkpoint state 持久化。
- 进程重启不会丢失当前 interrupt。
- 对新的 graph-native 路径，FastAPI 不再手动决定下一个 workflow node。
- endpoint 逐步迁移期间，现有 UI flow 仍能正常工作。

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - 新增 `GraphGateway`，用于 graph-native dependency planning。
  - `/runs/prepare` 已通过 gateway 启动，并写入 `graph_state.json`、`graph_checkpoints.sqlite` 和 workflow projection。
  - 新增 graph state 读取 endpoint 和 dependency review endpoint。
  - dependency review 决策会写入 canonical graph state，再投影给 UI。
- 仍待完成：
  - draft spec review、code review、R execution、repair、validation、compare endpoints 还需要从 service-owned transitions 迁到 graph interrupts。

## 6. Phase LG2.2 - 用产品节点替换 DatasetGraph Stub Path

目标：

让 `DatasetGraph` 成为真实的单 dataset 产品工作流。

任务：

- 替换产品路径中这些 skeleton nodes：
  - `draft_lineage_stub`
  - `draft_spec_stub`
  - `generate_code_stub`
  - `run_sandbox_stub`
  - `repair_code_stub`
  - `revise_spec_stub`
- 新增真实节点，封装现有 service/tool 行为：
  - `prepare_dataset_context`
  - `select_or_request_spec`
  - `draft_spec_agent`
  - `run_spec_static_check`
  - `wait_for_draft_spec_review`
  - `generate_r_code_agent`
  - `run_static_rule_check`
  - `wait_for_code_review`
  - `execute_r_sandbox`
  - `validate_dataset_output`
  - `compare_reference_output`
  - `diagnose_dataset_failure`
  - `route_after_diagnosis`
  - `write_dataset_audit`
- 旧 stub 行为只保留在显式 test/demo mode，不作为默认产品路径。
- 在任何 graph-native R 执行路径成为 product-default 之前，至少要对 generated R code 执行现有最小 forbidden-call 检查。
- 通过提取或 wrapper 复用现有函数：
  - `build_target_llm_context`
  - `generate_draft_spec_from_evidence`
  - `parse_generated_code_response`
  - `write_generated_code_artifacts`
  - `LocalRRunner`
  - `diagnose_downstream_failure`
  - static-check placeholder
  - compare/download helpers

可能涉及文件：

- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/routing.py`
- `src/adam_agent/api/service.py`
- `src/adam_agent/downstream/runner.py`
- `src/adam_agent/llm/draft_spec.py`
- `src/adam_agent/llm/context.py`
- `tests/test_graph_smoke.py`
- `tests/test_api_phase8.py`

退出标准：

- ADSL 和 ADAE 的 graph invocation 可以停在 code review，并 resume 到 R execution。
- 没有 product test 依赖 `*_stub` 节点。
- `real_adsl_minimal` 保持 retired。
- 包含明显 forbidden calls 的 generated R 会在 R execution 前被拦截。

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - 新增显式 `graph_product_prepare` DatasetGraph mode。
  - `graph_product_prepare` 会构建目标 LLM context；如果用户提供了 `input_spec`，直接使用；如果没有，则生成需要人工审核的 draft spec。
  - draft spec artifact 绑定当前 input fingerprint，并写入 reference ADaM policy。
  - 新增显式 `graph_product_generate_code` DatasetGraph mode。
  - `graph_product_generate_code` 可以使用用户 `input_spec`，也可以使用同一 run 内、已人工批准、且 input fingerprint 匹配的 draft spec。
  - stale 或缺少 fingerprint 的 approved draft spec 会在 code generation 前 fail closed。
  - generated code、LLM 原始响应、parsed response、compact prompt、static-check placeholder 都会作为 audit artifacts 写出。
  - graph 会停在 `code_review`，这个 mode 不执行 R。
  - 旧 stub chain 只通过显式 legacy/test mode 保留；graph-product modes 会跳过 stub code generation 和 sandbox execution。
  - FastAPI `/datasets/{dataset}/finalize-inputs` 现在委托给 `graph_product_prepare`。
  - FastAPI `/datasets/{dataset}/generate-code` 现在委托给 `graph_product_generate_code`。
  - service wrapper 仍会把 graph state 映射回现有 UI response model 和 `workflow_state.json` projection，以保持 UI 兼容。
- 仍待完成：
  - 从 `code_review` graph-native resume 到 R execution。
  - graph-native validation、compare、terminal failure routing 和 repair。
  - 等所有 product tests 都改用 graph-product modes 后，再移除或进一步隔离旧 stub nodes。

## 7. Phase LG2.3 - StudyGraph 多 Dataset 产品编排

目标：

让多 target ADaM 生成成为 graph 的一等能力，而不是 UI 的 target 选择循环。

任务：

- 保留 `plan_dataset_dependencies()` 作为确定性 planner，但把其结果纳入 `StudyRunState`。
- dependency plans 和用户 dependency decisions 必须绑定：
  - 当前 input fingerprint
  - dependency decision version
  - target dataset set
- 使用 dependency batches 分发独立 DatasetGraphs。
- 对每个 target dataset 维护独立 dataset state：
  - current interrupt
  - approvals
  - generated code
  - validation status
  - failure diagnosis
  - output artifact
- 依赖缺失时：
  - 判断用户是否已通过 reference/output 提供
  - 如果没有提供，触发 `dependency_user_action_required`
  - 允许用户选择是否由系统生成依赖 dataset
  - 不静默自动运行上游 ADaM
- UI 切换 active target 时保留所有结果。
- 从 dataset statuses 汇总 study status：
  - completed
  - waiting_for_user
  - running
  - terminal_failure
  - partially_completed

可能涉及文件：

- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/graph/dependencies.py`
- `src/adam_agent/graph/dependency_resolution.py`
- `src/adam_agent/api/web.py`
- `src/adam_agent/api/models.py`
- `tests/test_graph_smoke.py`
- `tests/test_api_phase8.py`

退出标准：

- 请求 `ADAE, ADCM` 的 run 会保留两个 dataset cards 和各自状态。
- 如果二者都依赖 `ADSL`，且用户选择由系统生成 ADSL，ADSL 只运行一次，两个下游 dataset 都使用它的输出。
- 如果用户拒绝系统生成依赖，下游 datasets 保持 blocked，并显示明确 action message。
- 上传新文件导致 input fingerprint 变化时，旧 dependency decisions 会失效。

## 8. Phase LG2.4 - 多智能体节点模型

目标：

引入多智能体架构，但不引入失控自治。

Agent 角色：

- Evidence Agent：
  - 扫描 SDTM、define、legacy SAS、input spec、reference ADaM
  - 产出 evidence bundle 和 risk flags
- Dependency Agent：
  - 审查确定性 dependency plan，并标记不确定依赖
  - 没有证据时不发明依赖
- Spec Agent：
  - 仅在没有用户 input spec 时生成 draft spec
  - 记录 assumptions 和 risk points
- Code Agent：
  - 根据 approved spec 和 runtime contract 生成 R
  - 输出严格 JSON，包含 R code 和 expected outputs
- Static Review Agent：
  - 先运行确定性检查
  - 当没有 input spec 时，在 code generation 前检查 draft spec 结构
  - 必要时调用 reference/rules tool 查询具体规则
- Execution Agent：
  - 在配置好的 sandbox 中执行 approved code
  - 不修改代码
- Diagnosis/Repair Agent：
  - 分类失败
  - 路由到 repair code、revise spec、request input 或 terminal failure
- Audit Agent：
  - 写 run-level 和 dataset-level audit summaries

重要边界：

本项目里的“agent”是一个有明确 tool contract 的 LangGraph node，不是可以任意改文件的自由进程。

可能涉及文件：

- `src/adam_agent/agents/` 新包
- `src/adam_agent/graph/dataset_graph.py`
- `src/adam_agent/graph/study_graph.py`
- `src/adam_agent/llm/`
- `src/adam_agent/tools/`
- `tests/test_agents_contract.py` 新增

退出标准：

- 每个 agent node 有 typed input/output。
- 每个 agent 写 artifacts 和 risk flags。
- Dataset run audit 能显示哪个 agent 做了哪个决策。

## 9. Phase LG2.5 - Reference 与 Static Rule 层

目标：

加入 standards-aware 检查，但不伪装成生产完整。

任务：

- 添加 reference tool interfaces：
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- 先使用小型本地 fixtures 或 indexed markdown/PDF snippets。
- 在人工 code review 前加入确定性 static checks：
  - required output file path
  - expected dataset name
  - 适用时检查 `USUBJID` 等关键变量
  - 禁止危险 R calls
  - 禁止 network/system command calls
  - 在便宜可做时检查 spec variable 与 generated code output 是否不一致
- 所有检查都标记置信等级：
  - blocking error
  - warning
  - informational
- 不宣称 full CDISC compliance。

可能涉及文件：

- `src/adam_agent/tools/reference_store.py` 新增
- `src/adam_agent/tools/static_rules.py` 新增
- `references/CDISC/`
- `references/P21_Rules/`
- `references/company_standards/`
- `src/adam_agent/api/service.py`
- `tests/test_static_rules.py` 新增

退出标准：

- 包含 `system()` 或 `download.file()` 的 generated code 会在 review 前被 blocked。
- 缺失 expected output path 会被 blocked。
- 警告明确说明：当前不能证明 full CDISC/P21 compliance。

## 10. Phase LG2.6 - R Sandbox 强化边界

目标：

把当前 R execution boundary 变成可替换 sandbox interface。

任务：

- 定义 `SandboxRunner` protocol：
  - local Rscript runner
  - future Docker runner
  - future Windows-isolated runner
- 保留 local `Rscript` 作为默认 developer runner。
- 强制约束：
  - working directory = run directory
  - timeout
  - output whitelist
  - environment variable control
  - 使用真实 sandbox backend 时禁用网络
- 添加 preflight checks：
  - script path 必须在 run dir 下
  - output path 必须在 run dir 下
  - 无明显 forbidden calls
- 文档明确 local Rscript 不是生产隔离。

可能涉及文件：

- `src/adam_agent/tools/r_runner.py`
- `src/adam_agent/tools/sandbox.py` 新增
- `src/adam_agent/downstream/runner.py`
- `src/adam_agent/api/service.py`
- `tests/test_r_sandbox.py`

退出标准：

- Graph 调用 sandbox interface，而不是 hard-coded local runner。
- Local runner 仍可用于开发。
- 生产文档清楚标注 local runner 不是 hardened sandbox。

## 11. Phase LG2.7 - UI 作为 Graph State Viewer

目标：

让 UI 变得可理解，因为它反映 graph state，而不是自己重新实现 workflow logic。

任务：

- 替换单 active-target 心智模型，改成：
  - study-level progress
  - dependency batches
  - persistent dataset cards
  - 每个 dataset 的 current interrupt/action
  - 每个 dataset 的 artifacts
- 默认隐藏技术路径。
- 路径/config 详情只放在 Advanced。
- Dependency map 先做成文本清晰表达：
  - “ADAE needs ADSL”
  - “ADSL available from user reference”
  - “ADLB waiting for user decision”
- 任何 reference ADaM panel 必须说明：reference ADaM 只用于 comparison/output-shape/dependency availability evidence，不是 derivation authority。
- 切换 target 时保留 generated/review/execution state。
- 显示按钮为什么 disabled。
- UI 数据来源使用 graph state projection endpoint。

可能涉及文件：

- `src/adam_agent/api/web.py`
- `src/adam_agent/api/models.py`
- `src/adam_agent/api/app.py`
- `src/adam_agent/api/service.py`
- `tests/test_api_phase8.py`

退出标准：

- 用户可以请求多个 outputs，并看到每个 dataset 的独立状态。
- 切换 target 不会隐藏之前进度。
- UI 从 graph state 显示下一步需要的人类动作。
- UI 清楚区分 input spec、approved draft spec、reference ADaM、generated output 的角色。

## 12. Phase LG2.8 - 兼容与废弃

目标：

在迁移到 graph-native 控制时，不破坏当前 MVP。

任务：

- 初期保留当前 endpoints：
  - `/runs/prepare`
  - `/finalize-inputs`
  - `/draft-spec-review`
  - `/generate-code`
  - `/code-review`
  - `/execute-approved-code`
- 逐个 endpoint 改接 GraphGateway。
- 把旧 direct service transitions 标记为 compatibility shims。
- 添加测试，确保 service shims 和 graph-native endpoints 产生相同 state projection。
- 保持 `src/adam_agent/adsl/` legacy code 不进入 product graph。
- 旧 `DatasetGraph` stub tests 在仍有回归价值时可以保留，但必须标记为 explicit demo/test mode，使 product tests 不依赖它们。

退出标准：

- 用户可见 workflow 不回退。
- 新 graph-native flow 和旧 endpoint sequence 收敛到相同 audit artifacts 和 UI state。

## 13. 推荐施工顺序

不要从 UI polish 或 CDISC rules 开始。推荐顺序：

1. LG2.0 State contract。
2. LG2.1 GraphGateway/checkpointer/resume。
3. LG2.2 DatasetGraph real product nodes。
4. LG2.3 StudyGraph multi-dataset orchestration。
5. LG2.7 UI graph-state viewer。
6. LG2.4 Multi-agent role packaging。
7. LG2.5 Reference/static rule layer。
8. LG2.6 Sandbox hardening boundary。
9. LG2.8 Compatibility cleanup 贯穿整个过程。

原因：

当前最大架构偏差是状态归属。如果 UI、service、graph 继续各自维护一部分 workflow state，后面每加一个功能都会变成补丁。必须先做 graph state contract 和 gateway。

## 14. LangGraph-2 非目标

- 不做 full production CDISC/P21 compliance。
- 不执行 legacy SAS programs。
- 不从 reference ADaM 单独反推 derivation rules。
- 在 graph-native replacement 通过测试前，不移除已有可用 APIs。
- 不让 ADSL 再次变成特殊路径。
- 不引入可以任意写文件或运行代码的 broad autonomous agent。所有 agent 必须有 typed state、artifacts 和 review gates。

## 15. 主要风险与控制

风险：Graph migration 破坏当前 UI flow。

控制：保留 compatibility endpoints，并添加 equivalence tests。

风险：LangGraph interrupt state 与 `workflow_state.json` 分叉。

控制：让 `workflow_state.json` 成为 graph state 生成的 projection。

风险：Multi-agent design 变成空泛概念。

控制：每个 agent 必须是有 typed input/output 和 artifact writes 的 node。

风险：Standards layer 过度宣称 compliance。

控制：检查结果标记 blocking/warning/info，full compliance 留到 Phase 9 hardening。

风险：R sandbox 看起来比实际更安全。

控制：local Rscript 标记为 developer mode，并在宣称隔离前加入可替换 SandboxRunner interface。

风险：Terminal failure 后用户不知道下一步能做什么。

控制：实现前先定义 terminal failure resume commands：
`retry_execution`、`repair_code`、`revise_spec`、`request_new_input`、`skip_dataset`、`continue_other_datasets`。

## 16. 第一个具体实施票

只从 LG2.0 + LG2.1 开始：

1. 添加 graph-native state schemas。
2. 添加 `GraphGateway`。
3. 添加 SQLite checkpointer-backed start/resume/read operations。
4. 添加 graph state 与 `workflow_state.json` 的 projection consistency tests。
5. 只把 `prepare_run_plan` 和一个 review interrupt path 接到新路径，作为 proof。
6. 添加 restart/resume tests。

这会为后续所有工作打地基。现在直接做 UI redesign、CDISC rules 或更多 provider options，都不能解决核心架构偏差。

## 17. 实施记录

### 2026-05-29 - LG2.0/LG2.1 起步

已完成：

- 从 `LangGraph` 创建第二阶段开发分支 `LangGraph-v2`。
- 新增 graph-native canonical state：
  - `StudyRunState`
  - `DatasetRunState`
  - `InterruptState`
  - `HumanCommand`
- 为未来多智能体节点预留状态字段：
  - `agent_decisions`
  - `risk_flags`
  - `evidence_bundle_id`
  - `reference_queries`
- `StudyGraph` 增加 `graph_gateway_mode=plan_only`，允许图在 dependency planning 后停住，而不是继续执行 dataset。
- 新增 `GraphGateway`：
  - 启动 graph-native dependency plan。
  - 把 StudyGraph 输出转换成 canonical `StudyRunState`。
  - 写出 `runs/{run_id}/graph_state.json`。
  - 写出 `runs/{run_id}/graph_checkpoints.sqlite` 作为当前本地持久 ledger。
  - 写出 `workflow_state.json` UI projection。
  - 提供 `load_graph_state()` 供进程重启后读取 canonical state。
- `/runs/prepare` 已开始通过 `GraphGateway` 生成 dependency plan 和 workflow projection。
- 新增 graph-native 依赖审核入口：
  - `POST /runs/{run_id}/dependency-review`
  - 目前支持 approve/reject dependency review command，并写入 canonical graph state。
- 新增只读 API：
  - `GET /runs/{run_id}/graph-state`
- 新增 projection consistency helper，验证 graph state 与 `workflow_state.json` 的关键字段一致。

当前边界：

- 这一步只迁移了 dependency planning 入口。
- dependency review 已有最小 graph command/resume 记录，但还不会触发 dataset 产品节点执行。
- draft spec、code review、R execution 仍主要由现有 FastAPI service compatibility path 执行。
- 目前使用本地 `graph_state.json` + `graph_checkpoints.sqlite` 作为 canonical state 持久层；后续可替换为真正 LangGraph SQLite/Postgres checkpointer。
- `DatasetGraph` 产品节点仍未完成替换，LG2.2 才处理。

已验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_state_schemas -v
```

结果：51 tests passed。

### 2026-05-29 - LG2.2 最小切片

已完成：

- `DatasetGraph` 新增显式执行模式 `graph_product_prepare`。
- 该模式只做产品路径第一段：
  - 构建 target LLM context。
  - 写出 `runs/{run_id}/llm/{dataset}_context.json`。
  - 判断是否存在 target input_spec。
  - 有 input_spec 时停在 `code_generation_ready`。
  - 没有 input_spec 时调用 draft spec agent 生成 `runs/{run_id}/specs/{dataset}_draft_spec.json`，然后停在 `draft_spec_review`。
- 新模式不会进入旧 `generate_code_stub`、`run_sandbox_stub` 假执行链。
- 新增测试覆盖：
  - input_spec 存在时不生成 stub code。
  - input_spec 缺失时生成带 input fingerprint 的 draft spec，并进入 draft spec review gate。

当前边界：

- 这不是完整 DatasetGraph 产品替换。
- draft spec LLM 生成已并入 `graph_product_prepare` 最小路径。
- code generation、code review、R execution 还没有并入 DatasetGraph。
- 当前 UI/API 仍主要使用 compatibility service path。

已验证：

```text
python -B -m unittest tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_context tests.test_downstream_runner -v
```

结果：113 tests passed。

### 2026-05-29 - LG2.2 审查修复

已完成：

- 根据子 agent 对第一段 LG2.2 产品节点迁移的审查意见，修复了几个原则性漏洞。
- 在 `finalize-inputs` 和 `generate-code` 前加入 dependency gate：
  - blocked 或不可运行 target 现在会 fail closed，不能绕过 dependency review。
  - legacy SAS / define / spec conflict 这类需要人工确认的依赖证据，必须先解决才能继续产品步骤。
  - `no_dependency_evidence` 不会被硬拦截，这样缺 spec 的 target 仍能进入 draft spec 生成环节。
- 收紧 Reference ADaM 边界：
  - Reference ADaM 仍可作为 comparison / output-shape evidence。
  - Reference ADaM 不再满足 runtime dependency availability。
  - LLM context 不再把 `reference_adam/*` 暴露为 `resolved_dependencies`。
  - mock ADAE code 不再读取 `reference_adam/adsl.csv`。
  - 只有当前 run 生成的 output ADaM 才能作为运行时上游依赖输入。
- approved draft spec 现在同时绑定：
  - 当前 study input fingerprint。
  - 人工审批当时记录的 approved spec artifact sha256。
- 新增回归测试覆盖：
  - dependency gate 不能被 API 绕过。
  - dependency warning 不能被 API 绕过。
  - approved draft spec 审批后被篡改会被拒绝。
  - Reference ADaM 不进入 runtime LLM context。
  - run-output ADaM 仍可进入 runtime LLM context。
  - 同名 Reference ADaM 不会抢在同 run 生成的 output ADaM 前面。

当前边界：

- dataset actions 后，compatibility service wrapper 仍会直接更新 `workflow_state.json`。
- `code-review` 和 `execute-approved-code` 还没有完全迁移为 graph-native interrupt。
- product modes 仍经过一些 legacy stub node 名称再 summary，不过这些 stub node 在 product mode 下是 no-op。

已验证：

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

第二轮子 agent 复查又发现两个问题，已修复：

- dependency-plan warning 现在也会阻止 `finalize-inputs` / `generate-code`，直到用户审核。
- dependency artifact 查找现在优先使用同 run 生成的 output，再考虑同名 Reference ADaM。

结果：126 tests passed。

### 2026-05-29 - LG2.2 Draft Spec Gateway 切片

已完成：

- 把 generated draft spec 状态迁入 `GraphGateway` canonical graph state：
  - `record_draft_spec_generation()` 记录 draft spec 路径/hash、LLM
    prompt/response artifact、变量列表、warning、当前 input fingerprint。
  - dataset 进入 graph-owned `draft_spec_review` interrupt，不再只依赖
    service 层自己写 `workflow_state.json`。
- 把 draft spec 审核决策迁入 `GraphGateway`：
  - `record_draft_spec_review()` 把 approve/reject 写入 dataset 和 study 两级
    human command history。
  - approved spec 路径/hash 和 review artifact 写入
    `DatasetRunState.spec_state`。
- 在信任 draft spec review 前加入 fail-closed 校验：
  - graph state 中没有 draft spec 记录时拒绝审核。
  - 审核的 draft spec 路径和 graph state 不一致时拒绝。
  - draft spec 在生成后被篡改时拒绝。
  - draft spec artifact 自身缺少 input fingerprint 或 fingerprint 已过期时拒绝。
  - approve 决策必须带 approved spec artifact 和 hash。
  - approved spec hash 必须和 review payload 匹配。
- 收紧 approved draft spec 的消费边界：
  - DatasetGraph 代码生成现在拒绝只存在于文件系统里的 approved draft
    spec，必须同时在 canonical graph state 里有 `spec_state.status ==
    approved`。
  - `GraphGateway.record_code_generation()` 会在记录 generated code 前再次校验
    graph-approved draft spec 的路径/hash/fingerprint。
  - 审核通过一个 dataset 时，不再清空另一个 dataset 仍然打开的 run-level
    interrupt。
- 更新 FastAPI compatibility service，使 `finalize-inputs`、`draft-spec`、
  `draft-spec-review` 都通过 `GraphGateway` 写入状态。
- 新增回归测试覆盖：
  - draft spec 生成后 graph-state 显示 `draft_spec_review`。
  - draft spec 审核通过后 graph-state 显示 approved spec state。
  - draft spec 审核前被篡改会被 graph gateway 拦截。

当前边界：

- `workflow_state.json` 仍作为 UI 兼容投影保留，但本切片已经让 draft spec
  生成/审核以及 approved draft spec 的消费成为 graph-owned 状态。
- code review 和 execution 已经部分 graph-owned，但 terminal
  validation/compare/repair 仍在 service 或 compatibility path 中。
- DatasetGraph 仍保留 legacy stub node，供非产品测试模式使用。

已验证：

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：子 agent 审查修复后，核心测试 144 tests passed。

### 2026-05-29 - LG2.2 Compare Gateway 切片

已完成：

- 把 reference compare 结果记录迁入 `GraphGateway` canonical graph state：
  - `record_compare()` 写入 `DatasetRunState.compare_summary`。
  - `DatasetResultSummary.compare_status` 现在反映 graph 中最新 compare
    status。
  - 如果 compare report 文件存在，会作为 dataset artifact ref 记录。
- 更新 FastAPI compatibility service，使 `/compare` 仍返回原来的
  `DatasetCompareResponse`，但同时把 compare 结果从 graph state 投影回
  `workflow_state.json`。
- compare 算法边界不变：
  - 仍只是 CSV-to-CSV 的结构和抽样 cell comparison。
  - 仍不是临床规则合规验证器。
  - Reference ADaM 仍只是比较证据，不是推导逻辑来源。
- 记录 compare 结果时保留打开的 graph interrupt：
  - dataset-level interrupt 不被清掉。
  - study-level dependency review 不会因为 compare call 被误清。
- 根据子 agent 审查意见修复：
  - `graph_state.json` 不存在时，compare recording 不再创建 canonical graph run。
  - compare recording 不再刷新 dataset product `input_fingerprint`；当前
    fingerprint 只写入 `compare_summary`。
  - study-level interrupt 和 dataset-level interrupt 同时存在时，保留
    study-level interrupt 的优先显示。
  - missing generated/reference 状态也会写回已有 graph state，避免 canonical
    compare state 停留在旧的 match/differences。

当前边界：

- compare 状态已经 graph-owned，但 compare 计算本身仍是 service/tool helper。
- 静态 ADaM/CDISC 检查和 repair routing 仍是后续 LG2.x / Phase 9 工作。

已验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

结果：子 agent 审查修复后 focused tests 65 tests passed。

核心验证：

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：151 tests passed。

### 2026-05-29 - LG2.2 Terminal Failure Review 切片

已完成：

- 新增 graph-owned terminal-failure triage 记录：
  - `GraphGateway.record_terminal_failure_review()` 记录执行进入
    `terminal_failure` 后的人类处置决策。
  - 允许的处置动作包括 `retry_execution`、`repair_code`、`revise_spec`、
    `request_new_input`、`skip_dataset`、`continue_other_datasets`。
  - 决策写入 dataset 和 study 两级 `human_commands`，并写入
    `DatasetRunState.execution_state.terminal_failure_review`。
  - gateway 会记录确定性的 `next_action`，但不会伪装 repair 或 spec
    revision 已经发生。
- 新增 FastAPI compatibility endpoint：
  - `POST /runs/{run_id}/datasets/{dataset}/terminal-failure-review`
  - 只有 dataset 当前处于 open graph-owned `terminal_failure` interrupt 时才允许写入。
- 保留当前 MVP 边界：
  - `repair_code`、`revise_spec`、`request_new_input` 会保留
    `terminal_failure` interrupt，只是把下一步动作说清楚。
  - `retry_execution` 会关闭当前 interrupt，把 dataset 放回 `pending`，但用户仍需显式再次执行。
  - `skip_dataset` / `continue_other_datasets` 只记录决策，不会静默修复或产出数据。

当前边界：

- 本切片只把 terminal-failure triage 纳入 canonical graph state；还没有实现
  graph-native repair-code 或 revise-spec 子流程。
- UI 按钮仍是后续工作；后台/API 契约已经具备。

已验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

结果：子 agent 审查前 focused tests 66 tests passed。

子 agent 审查后的修复：

- 在 approved-code execution 前增加 graph-owned 硬闸门：
  - dataset 如果还有 open `terminal_failure` interrupt，不能直接再次调用执行接口。
  - 必须先记录 terminal-failure review decision。
  - `retry_execution` 会关闭 interrupt，并把 dataset 放回 `pending`；但再次执行仍必须由用户显式触发。
- 防止失败后的 partial run output 被下游误当成可用依赖：
  - dependency resolution 在信任 `runs/{run_id}/outputs/{dataset}.csv` 前，会检查
    `runs/{run_id}/graph_state.json`。
  - 如果生产该 output 的 dataset 是 `terminal_failure` 或 `failed`，或者
    execution state 标记 `partial_output_usable=false`，该 artifact 会被标记为
    `found_but_unusable`。
  - 即使磁盘上存在 CSV，只要没有 canonical graph `output_adam` artifact 背书，
    也不能作为可用 run output 依赖。
  - 这可以防止失败 ADSL 写出的 partial `adsl.csv` 被 ADAE 或其他下游 ADaM 当作运行输入。
- 收紧 terminal-failure triage 的状态语义：
  - `skip_dataset` 后 dataset 保持 `failed`。
  - 如果已经没有 open interrupt，但任一 dataset 是 `failed`，study-level graph
    status 也会是 `failed`，不会伪装成 `running`。

修复后最终验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke -v
```

结果：118 tests passed。

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：155 tests passed。

### 2026-05-29 - LG2.2 Terminal Failure 后续闸门切片

已完成：

- 在 terminal-failure triage 后增加 graph-owned 后续闸门：
  - dataset 如果仍处于 `terminal_failure`，且没有记录 terminal-failure review
    decision，后续产品步骤会 fail closed。
  - 只有用户选择 `repair_code` 后，才允许重新 `generate-code`。
  - 只有用户选择 `revise_spec` 或 `request_new_input` 后，才允许重新
    `finalize-inputs` / `draft-spec`。
  - `execute-approved-code` 仍然只有在 `retry_execution` 后才允许。
- 复用现有产品流作为第一版 repair/revise 实现：
  - `repair_code` 表示用户允许从当前 approved spec 重新生成 R code，然后重新进入
    code review 和 execution。
  - `revise_spec` 表示用户必须先重新走 input/spec finalization，再重新生成 code。
  - 不伪造 repair 结果，也不绕过人工审核。
- 新增 `GraphGateway.record_input_spec_ready()`，让基于 input_spec 的
  finalization 写入 canonical graph state，而不是只更新 legacy workflow
  projection。
- 在 `spec_state` 和 `code_state` 中记录 terminal-failure follow-up provenance，
  审计时可以看到是哪一个人工处置动作解锁了后续产品步骤。

当前边界：

- 实际 LLM code repair prompt 仍沿用普通 code-generation prompt；专门的 repair
  prompt 留给后续 LG2.2/LG2.3。
- `request_new_input` 在用户补充/修正输入后按 `revise_spec` 处理；上传后自动
  re-plan 属于另一个 workflow 任务。

已验证：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：70 tests passed。

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：子 agent 审查前 158 tests passed。

子 agent 复审后的补充：

- 新的 `gpt-5.5` 子 agent 复审未发现 major blocker。
- 已修复一个 medium 问题：`generate-code`、`finalize-inputs`、`draft-spec`
  现在会先通过 canonical graph terminal-failure gate，再调用
  `mark_workflow_inputs_current()`。因此被 graph gate 拒绝的产品步骤不会先把
  legacy `workflow_state.json` read model 写脏。
- 已简化 terminal-failure gate helper，去掉重复状态判断。
- 新增回归断言：`retry_execution` 后如果错误调用 `generate-code` 被拒绝，
  `workflow_state.json` 不会被覆盖成 `generate_code_start`。

最终验证：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：73 tests passed。

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：161 tests passed。

### 2026-05-29 - LG2.3 多 Target 状态汇总与 UI 恢复切片

已完成：

- 在 `GraphGateway` 中新增 graph-owned study status rollup，让 study-level
  `status` 和 `current_interrupt` 从每个 dataset 的 durable state 汇总出来，
  不再由各个 endpoint 分散手写。
- 保留 public `/runs/prepare` 之后的多 target dataset 进度：
  - 同一个 run 里，之后为另一个 target 重新 prepare，不会丢掉之前 dataset
    已生成的 code state。
  - 既有 product progress 会继续保存在 canonical `StudyRunState.datasets`
    map 中。
  - target list 会把已有进度的 target 与新计划 target 合并。
- 根据子 agent 审查修复 interrupt 优先级：
  - 新的 study-level `dependency_review` 优先于旧的 dataset interrupt。
  - dataset `terminal_failure` 优先于普通 dataset review gate，比如
    `code_review`。
  - approve study-level dependency review 后，会清掉该 study interrupt。
  - terminal-failure triage 会在重新汇总 study status 前清理过期的
    dependency-review projection。
- 更新本地 UI：dependency planning 后从 `/graph-state` 恢复 per-dataset 状态：
  - draft spec、approved spec、generated-code metadata、code review、
    execution、compare summary 都会恢复到 UI 的 per-dataset map。
  - 切换 target 或重新 prepare 后，之前生成过的 dataset card 不会看起来像空状态。
  - stale generated code 会显示为 `stale`，不会伪装成正常待审核 code。
  - 如果 graph recovery 只有 code path、没有浏览器中可见的 code text，
    approve/run 会被禁用，避免绕过真实人工 code review。
- 新增回归测试覆盖：
  - 同一个 run 中为新 target prepare 时，保留另一个 target 的 generated-code state。
  - 记录 compare 时保留另一个 dataset 的 interrupt。
  - terminal failure 优先于普通 code review。
  - re-plan 后 dependency review 优先于旧 dataset interrupt。

当前边界：

- UI 的 `selectedTargets()` 仍只返回当前 active target。本切片完成的是多 target
  状态持久化和可见性；完整批量选择与 graph-dispatched 多 dataset execution
  仍留给后续 LG2.3。
- UI 从 graph state 恢复的是 code metadata，不是 code text。浏览器刷新后必须先
  加载 run review/code text，才允许 approve 本地执行。

子 agent 审查：

- `gpt-5.5` 子 agent 发现两个 major 和两个 medium UI 问题。
- 已在提交前修复：
  - terminal failure 不会再被字典序更靠前的普通 dataset interrupt 掩盖。
  - re-plan 产生的新 dependency-review interrupt 不会被旧 dataset product
    interrupt 覆盖。
  - stale code 不再显示成普通 generated code。
  - graph recovery 只有 code metadata、没有可审核 code text 时，approve/run
    会被禁用。

最终验证：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：77 tests passed。

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：165 tests passed。

### 2026-05-30 - LG2.3 多 Target 规划选择切片

已完成：

- 把本地 UI 里的 target 拆成两个概念：
  - `selectedTargetsForPlan`：一起发给 `/runs/prepare` 做依赖规划的
    ADaM datasets。
  - `selectedTarget`：当前在 draft spec、code review、execution、results
    面板里查看和操作的单个 dataset。
- 将 target 控件从单个 active button 改成“规划勾选框 + 查看按钮”：
  - 被勾选的 datasets 会一起参与 dependency plan。
  - active dataset 仍然控制单 dataset 的生成、审核、执行动作。
  - UI 至少保留一个 target 参与规划，避免空计划。
- 更新 dependency plan 的说明和事件文案，明确区分“planned targets”和
  “active detail target”。
- 从 `/graph-state` 里的 `requested_datasets` 恢复多 target 规划选择。
  `target_datasets` 仍作为更宽的 run inventory，用于卡片和进度展示，避免历史
  dataset 进度被静默变成当前 checkbox selection。
- plan/event 显示也改为使用 `requested_datasets` 作为 “Planned targets”，
  避免把更宽的 run inventory 展示成本次规划选择。
- 增加回归测试，确认 `/runs/prepare` 一次接收 `["ADAE", "ADCM"]`，并且
  canonical graph state 里持久化两个 dataset state。
- 增加 UI contract 覆盖，确认 View/card 交互不会修改规划选择，也不会触发新的
  dependency plan。

当前边界：

- 本切片仍是“规划与状态保存”，不是自动批量生成/自动批量执行。draft spec
  approval、code generation、code review、本地 R execution 仍然一次操作一个
  active dataset。
- 查看某个 dataset card 不会修改规划选择，也不会触发新的 dependency plan。
  只有 checkbox/manual-target 这类“规划选择”动作才会重新调用 `/runs/prepare`。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：78 tests passed。

根据子 agent 复审修复后：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：79 tests passed。

```text
python -B -m unittest tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：167 tests passed。

### 2026-05-30 - LG2.4 Agent Decision Contract 切片

已完成：

- 新增 `src/adam_agent/agents/` 契约包。这里的 agent 是“有明确输入输出和审计记录的
  LangGraph 节点”，不是可以任意操作文件的自由自治进程。
- 定义了明确 agent 角色：
  - `evidence_agent`
  - `dependency_agent`
  - `spec_agent`
  - `code_agent`
  - `static_review_agent`
  - `execution_agent`
  - `diagnosis_repair_agent`
  - `audit_agent`
- 给运行态 `DatasetGraphState` 和 `StudyGraphState` 增加
  `agent_decisions`、`risk_flags` reducer。
- 让已有 graph-owned 产品节点写入 agent decision：
  - dependency planning 写入 `dependency_agent` 决策。
  - product context preparation 写入 `evidence_agent` 决策。
  - draft spec generation 写入 `spec_agent` 决策。
  - R code generation 写入 `code_agent` 决策。
  - placeholder static checking 写入 `static_review_agent` warning。
  - approved R execution 写入 `execution_agent` 决策。
- 通过 `GraphGateway` 把这些 decision 持久化到 canonical
  `graph_state.json`，并投影到当前 UI 使用的 `workflow_state.json`。
- FastAPI service 仍只是 graph transition 的调用方。它把 DatasetGraph 产生的
  decisions 传给 `GraphGateway`，不成为 agent truth 的新来源。
- 对旧状态做兼容：历史里比较松散的 `agent_decisions` dict 不会阻塞 graph-state
  rollup；新写入的 decision 仍按严格 `AgentDecision` schema 校验。
- 根据子 agent 审查意见，补上 StudyGraph batch execution path 对 dataset
  subgraph `agent_decisions` 和 `risk_flags` 的收集，避免 LG2.4 只在 FastAPI
  split-flow endpoint 中生效。
- Gateway 生成的兼容默认 decision 现在会写入
  `record_source: graph_gateway_default`，方便审计时区分“DatasetGraph 节点真实
  产出的决策”和“Gateway 为兼容旧入口补记的默认记录”。
- 增加测试覆盖 agent contract、dependency-plan agent decision、spec agent
  decision、code/static-review agent decision、execution agent decision。

当前边界：

- 本切片只完成“agent 角色和审计状态记录”的结构化落地；还没有实现
  tool-calling reference agent、完整 ADaM/CDISC 静态规则检查、或自主多步 repair
  planning。
- `static_review_agent` 当前明确是 placeholder warning，不声称已经完成 CDISC
  compliance 检查。
- ADSL 仍走统一 ADaM split flow，没有重新引入 deterministic ADSL template
  特殊路径。
- Reference ADaM 仍然只是 compare/output-shape evidence，本切片没有把它记录为
  derivation authority。
- 本切片里 study-level `agent_decisions` 是 append-only audit history，不是
  dataset rollback/replacement 后重建出来的 “current-only view”。后续 audit-view
  切片需要把 immutable history 和当前有效决策分开。

验证：

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway -v
```

结果：27 tests passed。

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_state_schemas -v
```

结果：41 tests passed。

根据子 agent 审查修复后：

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_graph_smoke -v
```

结果：79 tests passed。

```text
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：171 tests passed。
