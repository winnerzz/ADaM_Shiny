# LangGraph-2 第二阶段施工方案

这份文档是 ADaM Agent Studio 在 Phase 0-8 MVP 之后的第二阶段施工方案。它把目前已经发现的架构偏差和未完成事项，整理成一套系统性工程计划。

LangGraph-2 的核心原则：

> 不要在现有 FastAPI service flow 旁边再叠一层补丁。要把真实业务流程迁入一个由 LangGraph 控制的状态机，让 FastAPI/UI 变成这个状态机的入口和展示层。

## 1. 当前基础

当前已经有价值的资产：

- `src/adam_agent/graph/gateway.py` 现在是主要产品状态转换边界。
  FastAPI service helpers 已经通过 `GraphGateway` 委托 dependency planning、
  input finalization、draft-spec generation/review、code generation/review、
  approved R execution、terminal-failure review、compare recording、upload
  invalidation，以及 legacy `/runs` compatibility writes。
- `src/adam_agent/api/service.py` 现在主要负责 request validation、
  config/provider resolution、response shaping、artifact preview/download 和
  read-model helpers。它不应该拥有 workflow state transitions。
- `src/adam_agent/graph/study_graph.py` 已有 study 级依赖计划、dataset 批次、分发、汇总和 study audit manifest。
- `src/adam_agent/graph/dataset_graph.py` 已有 dataset 图骨架、
  product-mode prepare/generate/execute nodes、LLM downstream 执行模式、失败路由和结果汇总。
- `src/adam_agent/graph/workflow_state.py` 目前仍会持久化兼容用的
  `workflow_state.json` read model 和 SQLite sidecar history，供当前 UI/API
  展示层使用。
- `src/adam_agent/llm/`、`src/adam_agent/downstream/`、`src/adam_agent/tools/` 已经形成了可复用的工具边界。
- ADSL 已修正为和其他 AD target 一样走统一 ADaM 产品流程。旧的 `src/adam_agent/adsl/` 只作为 legacy/regression 保留。

当前架构偏差：

- `workflow_state.json` 仍作为 UI/API 兼容 read model 存在，但产品事实来源正在
  canonical `graph_state.json`；compatibility projection writes 现在应位于
  `GraphGateway` 后面，而不是 FastAPI service helpers 里。
- 产品流已经由 graph-gateway 拥有，但 UI 仍通过一步一步的 FastAPI 调用驱动。
  它还不是一个由 StudyGraph 统一启动、分发所有 dataset subgraph、并通过
  LangGraph 原生 interrupt/checkpointer resume 完成的完整运行。
- `DatasetGraph` 里仍有早期 `*_stub` 节点供显式 legacy/test modes 使用。Product
  modes 已防止回落到 legacy stub chain，但旧节点还没有删除。
- 当前产品更像“受控流水线 + LLM 调用”，还不是每个角色都由专家节点承担的
  多智能体图。
- UI 的 dataset card 和 target 切换仍更像单 target 控制器，不像多个持久 dataset run 的图状态视图。
- Static ADaM/CDISC 检查现在已有有限范围的 policy-driven gate，但还不是完整规则引擎。
- R 执行仍是本地 `Rscript` 加应用层路径约束，不是强化沙盒。

未完成的产品能力：

- 完整产品流的 graph-native checkpointer resume，而不仅是当前持久化
  `graph_state.json` 后的恢复读取。
- 依赖审核、draft spec 审核、code review、terminal failure triage 的 LangGraph
  原生 interrupt 执行，而不是 API methods 将 review commands 写入 canonical state。
- 由 StudyGraph 驱动的多 dataset 产品编排，并在 UI 中体现为持久 dataset card。
- 更深入的 Evidence、Spec、Code、Validation、Repair、Reference、Audit 等 agent
  角色分离。当前 agent decisions 是可审计记录，但还不是完整专家节点图。
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
  - 产品 split-flow 的状态跳转现在已经归 `GraphGateway` 所有，但仍通过一步一步的
    gateway calls 暴露。后续仍需把完整产品运行做成 LangGraph 原生
    interrupt/checkpointer execution，而不是一串 API 触发的 graph-state update。

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
  - 产品 split-flow endpoints 现在已经通过 `GraphGateway` 委托状态变更，包括
    input finalization、draft-spec generation/review、code generation/review、
    approved R execution、terminal-failure triage、compare recording、upload
    invalidation 和 legacy `/runs` compatibility projection writes。
  - API 回归测试会防止 service helpers 直接写 `workflow_state.json`，或在受保护的
    product actions 中调用 low-level recorders。
  - 新增 checkpointer 边界：默认仍支持 in-memory saver；当安装可选
    `langgraph-checkpoint-sqlite` 包时，可以显式启用本地 SQLite saver。
  - 本地 SQLite saver 写入 `runs/{run_id}/langgraph_checkpoints.sqlite`，刻意和
    现有产品审计 ledger `graph_checkpoints.sqlite` 分开。
  - 增加 restart-style 覆盖：当可选包可用时，重新创建一个 `GraphGateway` 可以从同一个
    LangGraph SQLite checkpoint 读回 native dependency-review interrupt。
- 仍待完成：
  - 审核决策和产品动作已经持久化到 canonical graph state；native pilot interrupt
    在安装可选依赖后可以使用本地 SQLite checkpointer。但完整产品循环还不是一个
    可在进程重启后恢复所有 gate 的单次 native LangGraph run。公开 FastAPI/UI 默认
    仍使用 in-memory checkpointer，除非后续切片接入明确产品配置。
  - repair/spec-revision 闭环仍需要更多原生 graph routing；当前 terminal-failure
    triage 会记录受控 next action，但不会自动执行 repair cycle。

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
  - limited static-check report
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
  - generated code、LLM 原始响应、parsed response、compact prompt、limited static-check report 都会作为 audit artifacts 写出。
  - graph 会停在 `code_review`，这个 mode 不执行 R。
  - product agent nodes 现在会直接进入 `summarize_dataset`，不再经过 no-op 的
    legacy `*_stub` 节点。
  - `graph_product_execute` 不再在 `prepare_dataset` 阶段执行 R；R execution
    只发生在显式的 `execute_approved_code` graph node。
  - 旧 stub chain 只通过显式 legacy/test mode 保留；graph-product modes 会跳过 stub code generation 和 sandbox execution。
  - FastAPI `/datasets/{dataset}/finalize-inputs` 现在委托给 `graph_product_prepare`。
  - FastAPI `/datasets/{dataset}/generate-code` 现在委托给 `graph_product_generate_code`。
  - FastAPI code-review、approved execution、terminal-failure review、compare 和
    upload invalidation 路径现在会调用 high-level `GraphGateway` methods，然后把
    graph-owned result 映射回现有 response models。
  - `GraphGateway` 负责写 `workflow_state.json` compatibility projection；service
    wrapper 只根据 gateway result 整理 response model。
- 仍待完成：
  - dataset 产品步骤已经由 graph-gateway 拥有，但还不是从 code review 到 R
    execution 和后续路由的一次连续 native LangGraph interrupt/resume execution。
  - automatic repair 和 spec-revision loops 尚未完成。当前 terminal-failure
    handling 会记录人工 triage 和受控 next actions。
  - 等 legacy/test-mode coverage 不再有用后，最终移除旧 stub nodes。

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

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - Dependency plan 已写入 canonical `StudyRunState`，并绑定 requested targets
    和当前 input fingerprint。
  - Gateway 会从持久化的 per-dataset state 汇总 study status 和
    `current_interrupt`，不再让每个 API 路径各自写一套 study progress。
  - 支持 multi-target planning：`/runs/prepare` 可以一次接收多个 ADaM target，
    持久化所有 requested dataset state，并在后续 plan/view 其他 target 时保留
    已有 dataset progress。
  - 缺失或不确定的上游 ADaM evidence 会 fail closed。Reference ADaM 可以作为
    availability 或 comparison evidence，但不能作为 runtime dependency artifact
    或 derivation authority。
  - 上传文件变更后会标记受影响的 canonical graph runs 为 stale，并在 input
    fingerprint 变化时强制重新规划。
  - UI target 选择已拆成 planning targets 和单个 active detail target，所以切换
    当前查看对象不会抹掉其他 dataset state。
- 边界：
  - 当前已完成 graph-owned multi-target planning、状态保留、依赖闸门和状态汇总。
  - 还没有完成“对所有勾选 target 自动批量生成/审核/执行”。Draft-spec review、
    code review 和 local execution 仍是一次处理一个 active dataset，通过
    graph-gateway gate 控制。
  - 当前设计仍然不会静默自动运行上游 ADaM 依赖；用户依赖决策必须显式记录。

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
- Validation Agent：
  - 记录 validation 和 generated-vs-reference comparison evidence
  - 标记 scope limits，不声称临床推导正确性
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

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - 在 `src/adam_agent/agents/` 中新增 bounded agent 合同：
    `AgentDecision`、`AgentNodeInput`、`AgentNodeOutput`。
  - DatasetGraph 产品节点现在会为 evidence/spec/code、static review、
    execution、validation/compare、diagnosis/repair、audit handoff 写出
    typed node IO。
  - GraphGateway 会保留 DatasetGraph 的 node IO；直接 recorder 路径没有
    上游 node IO 时，也会补充兼容的 fallback node IO；dataset-level node
    IO 会汇总到 canonical study state。
  - Dependency planning 现在会写出 study-level `dependency_agent` node IO。
  - 派生的 agent audit summary 现在展示 study 级和 dataset 级 node IO
    计数、无效 node IO 计数、按 agent 统计的计数，以及最新 node output 摘要。
  - 测试覆盖 node IO 合同校验、GraphGateway 持久化、StudyGraph audit
    汇总、异常 node IO 可见性，并确认 compare、terminal failure、
    dependency planning、execution、fallback recorder 等产品行为没有改变。
- 边界：
  - 当前已经完成的是“受控多智能体节点包装和审计可见性”。
  - 这里的 agent 仍是受图和工具约束的节点，不是自由自治进程；不会绕过人工闸门，
    也不会改变 Reference ADaM 不能作为推导权威的原则。
  - 专门的 LLM repair、spec-revision 闭环，以及更深的 standards retrieval
    仍属于后续 hardening 工作。

## 9. Phase LG2.5 - Reference 与 Static Rule 层

目标：

加入 standards-aware 检查边界，但不伪装成生产完整。静态检查必须是通用
policy 检查，不能写成针对 demo 或某个 ADaM 数据集的补丁规则。

设计原则：

- 静态检查是 policy/rule-pack 层，不是不断追加 PSY201、ADAE、ADSL 或某个
  demo 变量特例的补丁清单。
- 静态规则开发必须先抽象出可复用的 rule shape，不能从某个失败样例直接写
  补丁。每次新增规则时先问“违反了哪个通用 contract”，而不是“怎么让这个
  文件不失败”。
- 一条规则只有在不命名某个 demo study、legacy program、reference ADaM 文件
  或手挑临床变量的情况下也能讲清楚适用范围，才允许进入实现。否则它只能是
  reviewer note 或 candidate rule-pack backlog item。
- 静态检查要从底层原则设计成可复用的 contract check。demo 中出现的失败只能
  说明“可能缺一个更通用的 contract”，它本身不能直接变成生产规则。
- 这是硬架构边界：通用 static-check engine 不能按 dataset 名、study 名、demo
  文件夹或某个临床变量个案分支。demo 中观察到的问题只能变成通用 contract 的
  测试，或进入带来源的 rule-pack item。
- 静态检查只验证已经声明的 contract，不负责决定应该有什么临床推导 contract。
  例如 engine 可以检查 generated code 是否明显写出了 approved output path，
  或是否引用了调用方传入的 spec 变量；但它不能自己判断某个数据集必须有某个
  ADaM 变量或某种推导，除非这个要求来自 approved spec 或带来源的 rule pack。
- 实现必须分层：
  - `StaticRuleEngine`：领域中立的 evaluator，只负责 artifact 完整性、执行安
    全、声明契约和 rule-pack 执行。
  - `StaticRulePolicy`：当前 run 的配置，例如 required output path、当前 code
    hash、声明 target dataset、approved spec 传入的 identifiers。
  - `StaticRulePack`：可选的 standards/company rules，必须带 authority_type、
    source、version、scope、severity、evidence。临床/领域知识只能从这里进入，
    不能写死在 engine。
- 静态规则可以检查通用 artifacts 和 contracts：
  - generated code 的 path/hash 绑定
  - expected output file contract
  - declared target dataset
  - approved spec 中的变量、label、type
  - R 执行边界中允许/禁止的 primitives
  - standards pack 提供的 reference rule id
- Blocking rule 只能来自：
  - execution safety violation
  - artifact integrity 或 hash/path mismatch
  - 用户已经 approved 的显式 contract
  - 带 evidence 和 declared severity 的 versioned rule-pack rule
- 新增 blocking clinical rule 不能写进 generic engine。它必须通过带明确
  `authority_type`、source、scope、severity 和 evidence 的 versioned rule pack
  进入，并作为 rule-pack 变更接受审核。允许的 authority class 只限 CDISC
  standard、P21 rule、company standard 或 user policy。demo 观察或实现备注不是
  规则权威来源。
- 静态规则不能从 demo 数据观察中发明临床推导逻辑。例如它可以说“approved
  spec 中列出的变量在 generated code 里不可见”；但不能说“这个数据集必须按
  某种固定方式推导 TRTEMFL”，除非这条规则来自明确的 approved spec、company
  standard 或带来源的 CDISC/P21 rule pack。
- 数据集相关 standards 应该作为带 authority_type、版本、来源、适用范围、
  severity 和 evidence 的 rule pack 输入。规则引擎保持通用，领域知识由 rule
  pack 提供。
- demo 中发现的问题只能先形成 candidate rule。只有当它被转换成带来源的
  rule-pack item 后，才能成为生产静态规则；在此之前只能作为实现备注或通用
  contract 的测试，不应写成生产逻辑。
- rule-pack 准入是产品/治理决策，不是随手改代码。任何 clinical/static
  standards rule 要能 block 一个 run，必须先有 authority_type、source、
  version、scope、severity 和 evidence。
- `source` 也必须是真正的规则来源，不能把 `demo-observation`、
  `implementation-note`、`candidate-rule` 或 `reviewer-note` 伪装成
  `user_policy` 或 company standard。观察记录只能进入 candidate backlog 或
  reviewer note，不能直接进入 binding rule pack。
- generic engine 里不能有“例外登记表”。如果未来某个问题看起来需要例外处理，
  工程上的处理只能是：修正 approved spec contract，加入带来源的 rule-pack
  item，或在具备正式来源前保留为不 blocking 的 reviewer note。
- 所以 LG2.5 的第一优先级是 rule-pack 准入和 provenance，而不是继续增加临床
  检查项。rule-pack contract 没有建立前就新增 ADaM/CDISC 规则，应视为设计错
  误。
- 任何启发式或不完整检查都只能作为 warning/informational，并且必须记录“不能
  证明临床推导正确”。
- “通用”不是“看见 demo 里某个问题就写一条 if 规则”。通用规则必须检查当前
  run 中已经声明的 contract，而不是记住某个临床个案。未来在 PSY201 或其他
  study 中发现的问题，只能先被表达成以下三种形态之一：
  - 不依赖 study 名或 dataset 名的 artifact/execution/spec contract
  - 带 authority_type、source、scope、severity、evidence 的 versioned
    rule-pack item
  - 不 blocking 的 reviewer note 或 candidate-rule backlog
- 如果一个 static check 需要靠“某个 demo study、某个上传文件、某个单独变量
  特例”才能解释清楚，它就不能进入 generic static-rule engine。
- 用户审查意见重申：静态规则不能写成一串具体 demo 修补。未来如果某个失败
  暗示需要新增检查，必须先把它改写成通用 contract，或改写成带治理信息的
  rule-pack item；如果无法完成这个改写，就只能作为 reviewer note 暴露给人，
  不能作为 blocking static rule。
- static-rule 设计在实现前必须通过一层 abstraction gate：把触发问题的 demo
  名、文件名、dataset 名和单个变量个案都删掉后，仍然能说清楚它检查的是哪个
  通用 declared contract，或它属于哪个带来源治理信息的 rule-pack item。否则
  只能进入 reviewer note 或 backlog candidate。

静态规则架构调整：

- 每个候选规则必须拆成两个对象来看：
  - `RuleObservation`：问题是在哪里被观察到的，例如 demo 失败、用户审核意见、
    或一段糟糕的 generated script。它只能作为调查证据，不能直接 block run。
  - `RuleAuthority`：系统为什么有权强制执行这条规则，例如 system execution
    boundary、approved spec、user policy，或已经准入的 rule-pack item。
- generic engine 里只能放 evaluator function。Evaluator 只能回答这类问题：
  - artifact 是否绑定当前 code hash？
  - code 是否调用了禁止的 execution primitive？
  - code 是否可见地满足调用方声明的 output contract？
  - 带来源的 rule-pack item 是否有足够 metadata 可以被执行？
- 所有领域词都必须作为 policy/rule-pack 参数传入 evaluator。Engine 代码里不能
  携带 clinical variable、dataset 名、study 名、legacy program 名或 reference
  output pattern。
- 所以后续每条 static rule definition 必须包含：
  - 稳定的 `rule_id`
  - `rule_family`：artifact contract、execution boundary、spec contract 或
    standards pack
  - authority source：system contract、approved spec、user policy 或 rule pack
  - 当前 run 或 rule pack 传入的 parameter payload
  - severity 和 confidence
  - evidence pointer
  - 面向 reviewer 的 limitation text，说明这条规则不能证明什么
- 一个具体 failure 只有完成这条转换链后，才能成为 blocking rule：
  observation -> generic rule shape -> authority binding -> deterministic
  evaluator -> audit-visible report。任何一步缺失时，产品只能把它显示成
  reviewer note，不能给 engine 打补丁。
- 新增 static check 的回归测试必须证明通用性。每条新的 blocking static rule
  至少需要一个中性的 non-demo fixture，并且要有 source-level guard，防止规则
  依赖最初的 demo/study/dataset/variable 名称。

任务：

- 添加 reference tool interfaces：
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- 先使用小型本地 fixtures 或 indexed markdown/PDF snippets。
- 在人工 code review 前加入确定性 static checks：
  - contract rules：调用方传入的 output path、dataset name、code path/hash
    绑定，以及 LLM parser 保证的 generated-code dataset contract
  - execution-boundary rules：禁止危险 R calls，禁止 network/system command calls
  - spec/code consistency rules：从 approved spec variables 传入的 identifier
    可见性检查；当无法低成本证明 generated code 产出 expected variables 时，只
    给 warning
  - rule-pack loader contract：只加载带
    authority_type/source/version/scope/severity/evidence metadata 的显式
    standards/company rules；缺失 rule pack 必须显示成限制，不能用静默
    heuristic 顶替
  - rule-pack admission checks：没有 allowed authority_type、source、version、
    scope、declared severity 和 evidence pointer 的 rule-pack item，不能影响
    code review 或 execution
  - non-authority source checks：candidate/demo/reviewer/implementation note 不能
    被当成正式 source 进入 binding rule pack
  - future standards-pack rules：从显式 references 加载 CDISC/P21/company
    standard 检查，不把 demo 观察硬编码进引擎
  - 后续：在便宜可做时检查 spec variable 与 generated code output 是否不一致
- 增加回归保护：如果新增 static check 被实现成 dataset/study/demo 特例，而不
  是通用 contract 或 rule-pack rule，应被测试拦截或显式标记。
- 增加 source-level 回归保护：dataset 名、demo study 名、demo 中观察到的临床
  变量个案可以出现在测试或 rule-pack fixture 中，但不能作为 generic engine 的
  分支逻辑。
- 每条静态规则进入实现前必须走 rule lifecycle checklist：
  - candidate observation：在 demo/test/real run 中发现的问题，不能直接 block
    生产流程
  - generic contract：把问题改写成不依赖 dataset/study/file 特例的通用契约
  - authority binding：明确来源是 system contract、approved spec、user
    policy，还是 versioned rule pack
  - implemented check：写成确定性 evaluator，并带完整 audit metadata
  - reviewer visibility：报告里说明检查范围，以及它不能证明什么
- 给 static-check artifact 和未来 review 增加 rule-abstraction gate：
  - 具体 failure 可以触发调查，但不能直接作为规则文本
  - 实现后的规则必须能在不依赖 demo/study/file 名称的情况下说明清楚，除非这些
    名称属于受治理 rule pack 的 scope
  - audit report 必须提醒 reviewer：新的 blocking rule 需要通用 declared
    contract，或已经准入的 source-backed rule-pack authority
- 给未来每个 static-check PR 增加 rule-design review checklist：
  - 这条规则检查的是哪个已经声明的 contract？
  - 规则权威来自哪里：system contract、approved spec、user policy，还是
    versioned rule pack？
  - 这条规则是否不依赖 demo-study 名和某个文件观察？
  - 如果它能 block run，authority_type、source、version、scope、severity、
    evidence 记录在哪？
- 给未来每个 static-rule change 增加 implementation acceptance checklist：
  - generic engine code 里没有 study-name、dataset-name、demo-folder、
    legacy-program 或 reference-output 的特殊分支。
  - 任何 dataset/domain-specific knowledge 都必须通过 governed rule pack 加载，
    或来自当前 run 的 approved spec。
  - 触发这条规则的测试至少包含一个 generic/non-demo fixture，证明它检查的是
    可复用 contract，而不是记住当前 demo。
  - report text 必须说明这条规则检查什么，以及它不能证明什么。
- 所有检查都标记置信等级：
  - blocking error
  - warning
  - informational
- 不宣称 full CDISC compliance。

LG2.5 当前 slice 已实现：

- `StaticRulePolicy` 驱动 generated R checks。规则引擎本身不硬编码
  ADAE、ADSL、PSY201 或 `USUBJID`。
- `DatasetGraph` 和 downstream runner 都会在 code review 或 sandbox execution
  前写出 `runs/{run_id}/static_checks/{dataset}_static_check.json`。
- 当前 blocking checks 覆盖 forbidden R calls 与缺失调用方指定 output path。
- Identifier checks 来自 approved spec context，只作为 warning 级“代码里是否可见”
  检查，不能解释成推导正确性证明。
- `LocalReferenceStore` 提供一个小型本地文件检索边界，后续可接 CDISC/P21/company
  standards。
- active service 里的旧空实现 static-check 路径已删除，GraphGateway 的措辞
  改为 limited-scope static check。
- 根据子 agent 审查意见，`GraphGateway.record_code_generation`、
  `validate_code_review` 和 approved-code execution 现在都会 fail closed：
  static-check artifact 缺失、hash 改变、schema 不完整、存在 blocking findings，
  或没有绑定当前 generated R code 的 path/hash 时，不能进入审核/执行。
- StudyGraph downstream audit manifest 现在保留 downstream runner 写出的
  `static_check` artifact，不再在 dataset-result rollup 时丢失。
- 测试使用 `CUSTOM`/`ANY` 这类通用数据集名，防止规则退化成 demo-shaped patch。
- 测试还覆盖残缺 static report，以及拿另一份 R script 的 passing static report
  冒充当前代码检查结果的绕过场景。
- `StaticRulePack` 准入现在要求 pack 级显式 `authority_type`
  (`cdisc_standard`, `p21_rule`, `company_standard`, `user_policy`)。rule item
  继承该 authority，不能用不同 authority class 偷渡规则。
- `StaticRulePack` 准入会拒绝 `demo-observation`、`implementation-note`、
  `candidate-rule`、`reviewer-note` 这类非权威 source，防止把测试观察或实现备
  注包装成正式规则。

LG2.5 当前 slice 验证：

- `python -B -m unittest tests.test_downstream_runner -v`
- `python -B -m unittest tests.test_graph_smoke -v`
- `python -B -m unittest tests.test_static_rules tests.test_llm_generated_code tests.test_api_phase8 tests.test_graph_gateway tests.test_agents_contract -v`
- `python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules -v`

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

LG2.6 当前 slice 已实现：

- 新增 `src/adam_agent/tools/sandbox.py`，包含 `SandboxRunner` protocol 和
  `LocalRscriptSandboxRunner`。
- graph-owned approved-code execution 现在通过 sandbox interface 调用执行边界，
  不再直接构造 `LocalRRunner`。
- `llm_downstream_r_sandbox` 路径在需要本地 R 执行时也改用
  `LocalRscriptSandboxRunner`。
- validation report 现在会记录 sandbox boundary metadata：backend name、
  是否 hardened、run directory、是否禁用网络，以及明确说明 local Rscript 只是
  developer mode，不是生产级隔离。
- local sandbox preflight 会在启动 Rscript 前拦截 run dir 外的 script path 和
  不匹配的 working directory。
- `LocalRRunner` 现在会把相对 `script_path` 解析到 `working_dir` 下，避免按调用
  shell 的当前目录误解析。
- local sandbox 现在只把 allowlist 中的环境变量传给 Rscript。API key、
  `R_PROFILE_USER` 和 `R_ENVIRON_USER` 默认不会传入。
- local sandbox 执行 Rscript 时会加入 `--vanilla`，避免 generated-code run 读取
  用户级 R startup files 或 saved workspace。
- local sandbox 会拒绝调用方传入的 Rscript arguments，防止可执行命令行参数绕过
  generated-script preflight。
- `LocalRRunner` 保留显式 environment/argument injection，供 sandbox boundary
  控制执行；这不改变其他 profiling 或 legacy helper caller 的默认行为。

LG2.6 当前 slice 验证：

- `python -B -m unittest tests.test_sandbox tests.test_downstream_runner -v`
- `python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit -v`
- `python -B -m unittest tests.test_tools_phase4 tests.test_phase5_adsl_loop tests.test_sandbox -v`
- `python -B -m unittest tests.test_sandbox tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_static_rules tests.test_llm_generated_code tests.test_tools_phase4 tests.test_phase5_adsl_loop -v`

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

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - UI 通过 `GET /runs/{run_id}/progress` 读取 graph-owned progress，并在
    state-changing actions 后刷新 graph read models。
  - 顶部状态区显示当前操作、已加载 study、active detail target、graph-owned
    next action，以及可见的操作进度。
  - Dataset cards 会保留并展示 per-dataset graph state，包括 spec/code/
    execution/validation/compare 状态和 agent-node trace context。
  - Planning targets 与 active detail target 在界面上分开显示。
  - Dependency map 改成面向用户的逐 target 说明卡，解释 evidence、graph
    decision、runtime meaning 和 next action。
  - 主流程默认隐藏技术路径；path 和 source metadata 放在 Advanced/audit 表面。
  - Reference ADaM 被明确标注为 comparison/output-shape/dependency-availability
    evidence，不是 derivation authority。
  - 按钮可用性和 disabled reason 由 graph progress 驱动，明显 blocked 的状态不会只等
    后端报错。
- 边界：
  - UI 已经更接近 graph-state viewer，但仍是运行在 compatibility endpoints 和
    read models 之上的本地浏览器 UI。
  - 它还没有变成一个完全 native LangGraph run + interrupt resume 的前端。
  - Code generation、review、execution 仍是 active-dataset 动作；multi-target
    selection 目前控制 planning 和 dashboard context。

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

当前实现状态：

- 已在 `LangGraph-v2` 完成：
  - split-flow 产品 endpoints 已把状态跳转委托给 `GraphGateway`，覆盖 dependency
    review、input finalization、draft-spec review、code generation、code review、
    approved-code execution、terminal-failure review、compare recording、upload
    invalidation 和 read-model projection。
  - `workflow_state.json` 被作为 compatibility projection/read model；canonical
    product truth 是 `graph_state.json`。
  - compatibility responses 会携带显式 `workflow_control` metadata，方便区分旧路由
    与 graph-native product state。
  - legacy `/runs` LLM run-to-completion 已被拦截，并指向 split-flow review gates。
    只有显式 `execution_mode="stub"` 才保留 legacy compatibility/test 路径。
  - Product DatasetGraph topology 已不包含 legacy stub nodes；旧 stub chain 只存在于
    显式 legacy stub graph。
  - ADSL 保持在统一 ADaM LLM flow 中，没有重新接回旧 deterministic R-template 产品路径。
  - API/CLI 现在要求显式 execution mode，不再因为 mock provider 请求自动选择 stub 行为。
  - review summary 和 compare read paths 现在会在 canonical `graph_state.json`
    已存在但不可读时 fail closed。只有 run 从未创建 canonical graph state 时，
    才允许使用 artifact 或 `workflow_state.json` fallback。
  - generated table preview、generated downloads、review-summary 里的 generated
    output preview，以及显式 compare 读取现在只要存在 `graph_state.json`，就使用
    canonical graph output artifact；不会为 graph-owned run 回退到陈旧 CSV 或
    validation artifact。
- 边界：
  - 当前已经基本完成 compatibility ownership 清理，并把 legacy behavior 显式化。
  - 但完整产品流程仍没有完全替换为 native LangGraph interrupt/checkpointer resume。
  - `workflow_state.json` 仍为 UI/API 兼容保留；新的 workflow logic 仍应继续通过
    GraphGateway 和 canonical graph state 进入。

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
- 在这个切片当时，code generation、code review、R execution 还没有成为
  graph-owned product entry points。
- 后续 LG2.2/LG2.8 切片已经把 finalize inputs、draft spec generation、
  code generation、code review、dependency review、approved execution、
  terminal failure review 和 compare recording 收进 `GraphGateway`，同时保留
  兼容 route path。

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
- 后续 LG2.2 工作已经移除了 product mode 经过 no-op legacy stub node 再
  summary 的路径；这里是当时切片的历史边界。

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
- 在这个切片当时，code review、execution、validation、compare 和 repair
  routing 仍有 service/compatibility responsibilities。
- 后续 LG2.2/LG2.8 切片已经把 code review、approved execution、
  terminal failure review、dependency review 和 compare recording 移入
  `GraphGateway`。当前仍保留的边界是 compatibility route paths 会按单步调用
  graph-owned product steps。
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

### 2026-06-01 - LG2.8 Compare Corrupt Graph-State Guard

已完成：

- 收紧 compare compatibility path：如果 `runs/{run_id}/graph_state.json`
  已存在但无法解析或无法通过 schema validation，则直接 fail closed。
- 保留 legacy run 的 artifact-only compare fallback；也就是只有没有 canonical
  graph state 的旧 run 才能走这个兼容路径。
- 新增回归测试，证明损坏的 canonical graph state 不会回退到 artifact compare，
  也不会写出 compare report。

已验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_compare_fails_closed_when_existing_graph_state_is_corrupt tests.test_api_phase8.Phase8ApiTests.test_review_summary_fails_closed_when_existing_graph_state_is_corrupt -v
```

结果：3 tests passed。

### 2026-06-01 - LG2.8 Generated Output Read-Model Guard

已完成：

- generated table preview、generated downloads、review-summary output preview
  和显式 compare 读取现在都走 graph-aware output resolver。
- 只要 `graph_state.json` 存在，generated output 只有在 canonical dataset
  graph state 记录了 run 目录下可用的 output path 或 `output_adam` artifact 时才可见。
- 如果请求的 dataset 不存在于 canonical graph state，generated 读取会 fail closed，
  不会使用 legacy output discovery。
- 显式 compare 不能再把陈旧的 `outputs/{dataset}.csv` bootstrap 成 graph state
  里的 output artifact；它只能比较 graph 已经记录的 generated output。
- 只为没有 canonical graph state 的 legacy run 保留 artifact fallback。
- 收紧已有 path-escape 回归：如果 graph state 指向 run 目录外，UI read model
  会隐藏 generated output，而不是回退到 `outputs/{dataset}.csv`。

已验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_graph_state_blocks_generated_table_download_without_output_artifact tests.test_api_phase8.Phase8ApiTests.test_graph_state_generated_artifact_controls_table_download_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_output_is_not_previewed_or_downloadable tests.test_api_phase8.Phase8ApiTests.test_review_summary_ignores_graph_state_output_path_outside_run_dir tests.test_api_phase8.Phase8ApiTests.test_review_summary_fails_closed_when_existing_graph_state_is_corrupt -v
```

结果：5 tests passed。

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
  - limited static checking 写入 `static_review_agent` warning。
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
- `static_review_agent` 当前明确是 limited-scope policy check，不声称已经完成
  CDISC compliance 检查。
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

### 2026-05-30 - LG2.4 Audit Agent Summary 切片

已完成：

- 新增 `src/adam_agent/agents/audit.py`，作为受控的 audit agent summary
  层。
- 新的 audit summary 是从 canonical graph state 派生出来的只读摘要：
  `agent_decisions`、`risk_flags`、dataset 状态、当前 interrupt、artifact id。
- `GraphGateway` 每次持久化 canonical `graph_state.json` 时，会同步写
  `runs/{run_id}/audit/agent_summary.json`。
- `StudyGraph` batch execution 路径也会在写 study-level
  `audit/manifest.json` 前，写同样结构的 `audit/agent_summary.json` artifact。
- `StudyRunState` 和 `DatasetRunState` 增加 `agent_audit_summary`，让 UI/API
  projection 可以直接显示“哪个 agent 做了什么”，不需要自己解析原始 decision
  list。
- `workflow_state.json` 现在包含：
  - study-level `agent_audit_summary`
  - dataset-level `agent_audit_summary`
  - study-level audit `artifacts`，其中包含 `agent_summary_*`
- audit manifest metadata 现在同时包含原始 `agent_decisions`、`risk_flags`
  和派生出的 `agent_audit_summary`。
- 根据子 agent 审查，修复了一个 medium traceability 问题：直接走
  `StudyGraph` batch path 时，写 `agent_summary.json` 会按 dataset 汇总
  `state.audit_artifacts`，并增加回归断言，确认 direct StudyGraph summary
  里能看到 dataset artifact id。

当前边界：

- `agent_summary.json` 不是 workflow state source。系统真相仍然是
  `graph_state.json`。
- 这个 summary 只面向人工阅读和审计，不解锁 workflow gate，不改变 dependency
  plan，也不改变 dataset execution。
- summary 仍然反映 append-only decision history；还没有解决 rollback/replacement
  后区分 immutable audit history 和 current-only view 的未来问题。
- `static_review_agent` 相关内容现在是 limited-scope policy check，不证明完整
  CDISC/P21/company-standard compliance。

Focused verification：

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_graph_smoke -v
```

结果：80 tests passed。

完整核心验证：

```text
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas -v
```

结果：172 tests passed。

### 2026-05-30 - LG2.7 Study Progress Viewer 切片

已完成：

- 在本地 UI 的 Study Dashboard 增加 study-level progress panel：
  - study-level title/detail
  - 下一步动作 pill
  - 五个紧凑阶段：Inputs、Plan、Spec、Code Review、Run
- 这个 panel 来自现有 graph state/UI projection：
  - `state.graphState.status`
  - `state.graphState.current_interrupt`
  - dependency plan 状态
  - 每个 dataset 的 spec/code/review/execution map
- 增加前端契约测试，确保 progress panel 绑定 graph state 和 active dataset
  state，而不是重新实现一套独立 workflow。
- 本切片只做展示层，不改变 dependency planning、generation gate、code
  review、execution 或 repair routing。

当前边界：

- 这个 panel 是 browser-side projection。它改善用户理解，但 workflow 真相仍然
  是 `graph_state.json`。
- 这不是完整批量执行 UI。generation、review、本地 execution 仍然是 active
  dataset 动作。
- 本轮工具环境没有暴露 Browser plugin 需要的控制接口，因此没有做浏览器截图验
  证；已用 FastAPI HTTP response 和 UI contract tests 验证页面可访问和元素存在。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：56 tests passed。

### 2026-05-30 - LG2.7 Disabled Action Reason 切片

已完成：

- 在主要审核按钮旁边增加可见 action hints：
  - Finalize Inputs / Draft Spec
  - Approve Draft Spec
  - Generate R Code
  - Approve And Run Locally
- 将按钮可用性说明集中到 `actionAvailability()`，并且只从现有 graph/run UI
  projection 推导：
  - 当前选择的 target
  - dependency plan 和 active blocked dependency
  - input spec / approved draft spec gate
  - generated code state
  - review/execution state
- 给按钮增加 `title`、`aria-disabled-reason` 和 `data-action-ready`，让 UI 能解释
  缺什么，但不拥有 workflow gating。
- 本切片只做 UI projection，不改变 API 行为、graph transition、button gating、
  generation gate、approval gate 或 sandbox execution。

当前边界：

- action hints 是浏览器侧解释层。workflow 真相仍然是 graph state 和已有 run
  artifacts。
- hint renderer 不能设置 `button.disabled`。实际 transition 仍由已有 handler
  和 backend/graph checks 负责，包括现有的 auto-prepare 行为。
- 当前 compatibility buttons 仍然是 one-dataset-at-a-time action。
- 一些旧的直接 `button.disabled = ...` 赋值仍在 legacy UI path 中，但
  `renderActionAvailability()` 已从 dashboard 和 active target refresh path 调用，
  作为可见原因层的统一覆盖。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：57 tests passed。

### 2026-05-30 - LG2.7 Graph-State Plan Recovery 切片

已完成：

- 在本地 UI 中新增 `planFromGraphState()`，从 canonical graph state 字段恢复
  dependency-plan read model：
  - requested datasets
  - target datasets
  - runnable datasets
  - blocked datasets
  - dependency review status
  - dependency decisions
  - dependency plan / resolution payloads
- `applyGraphState()` 现在会从 `graph_state.json` 恢复 `state.plan`，并在 active
  target 恢复后重新渲染 dependency plan 面板。
- 调整 action hint wording：当浏览器侧还没有 plan cache 时，提示点击动作会先
  auto-prepare，和现有 handler 行为一致。

当前边界：

- 这是 UI read-model recovery 切片。canonical truth 仍是 `graph_state.json`；
  浏览器侧 `state.plan` 只是 projection。
- 不改变 dependency planning、dependency review、generation gate、approval gate、
  DatasetGraph execution 或 sandbox behavior。
- 它减少刷新/恢复后对临时浏览器 cache 的依赖，但还不是完整 pure graph-state UI。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：58 tests passed。

### 2026-05-30 - LG2.4 Validation Agent Compare Audit 切片

已完成：

- 在受控 agent role contract 中增加 `validation_agent`。
- `GraphGateway.record_compare()` 每次持久化 generated-vs-reference ADaM
  compare evidence 时，都会记录一条 `validation_agent` decision。
- 该 decision 记录：
  - compare status
  - compare report artifact id（如果存在）
  - 当前 input fingerprint digest
  - `reference_compare_limited_scope` risk flag
- study-level 和 dataset-level audit summary 现在能看到 compare evidence 是
  一个可审计的后处理 agent decision，而不是无归属的 service-side state mutation。

当前边界：

- `validation_agent` 是受控 graph/audit role，不是自主审稿人。
- Reference ADaM 仍然只是 comparison/output-shape evidence。本切片不允许
  reference ADaM 决定 derivation logic。
- compare algorithm 仍然是现有结构/样本比较，不声称证明临床推导正确性，也不
  声称 CDISC/P21 compliance。

验证：

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state -v
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.7 Agent Audit Viewer 切片

已完成：

- 在本地 UI 的 study dashboard 中增加 `Agent Audit` panel。
- 该 panel 读取浏览器里已经加载的 canonical `/graph-state` 数据：
  - active dataset 的 `agent_decisions`
  - fallback study-level 的 `agent_decisions`
  - graph 和 active-dataset 的 `risk_flags`
- Agent record 以简短、可读的卡片显示，不把原始 JSON 直接丢给用户。
- 面板包含目前 LG2.4 已加入的角色，包括 `validation_agent` 和
  `diagnosis_repair_agent`。

当前边界：

- 这是只读 graph-state viewer，不创建也不修改 graph state。
- 不新增 audit source。canonical truth 仍然是 `graph_state.json`；UI 只渲染已有
  projection payload。
- 不声称 agent decisions 证明临床正确性。risk flags 只是给人工 review 的可见性信号。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -m unittest tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.7 Human Review Queue Viewer 切片

已完成：

- 在本地 UI 的 study dashboard 中增加 `Human Review Queue` panel。
- 该 panel 读取浏览器里已经加载的 canonical `/graph-state` payload：
  - study-level `current_interrupt`
  - per-dataset `current_interrupt`
  - per-dataset `status` fallback，用于显示 `needs_review` 和
    `terminal_failure`
- 以可读卡片显示 dependency review、draft-spec review、code review 和
  terminal-failure triage 等人工审核门。
- 增加 UI contract 测试，确认 queue 来自 graph state，且不把 raw JSON 直接展示给用户。

当前边界：

- 这是只读 graph-state viewer，不创建、不关闭、不 approve/reject、不修改任何
  graph interrupt。
- 不新增第二套 human-review state machine。canonical truth 仍然是
  `graph_state.json`。
- 不改变 static-rule 行为。静态检查仍然是 generic contract/rule-pack
  governance，不是 demo/study/dataset-specific 补丁。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_state_progress_panel -v
python -m compileall -q src\adam_agent
```

### 2026-05-30 - LG2.7 Hide Technical Paths In Main UI 切片

已完成：

- 从主流程里的 draft-spec 和 code-review pane 中移除直接 spec/draft-spec artifact
  path 展示。
- 改成面向用户的 artifact-recorded 文案。
- 技术路径继续保留在已有的 Advanced setup and audit files 表格中。
- 增加 UI contract 测试，防止普通 review pane 重新展示 `input_spec_path`、
  `approved_spec_path`、`draft.spec_path` 或 `generated.draft_spec_path`。

当前边界：

- 这是 UI presentation 层改动，不改变 artifact storage、download behavior、
  graph state、workflow projection 或 audit files。
- 技术路径仍可在 Advanced 中用于 audit/debug。
- 不改变 static-rule 行为。静态检查仍然是 generic contract/rule-pack
  governance，不是 demo/study/dataset-specific 补丁。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -m compileall -q src\adam_agent
```

### 2026-05-30 - LG2.4 Diagnosis/Repair Agent Triage Audit 切片

已完成：

- `GraphGateway.record_terminal_failure_review()` 在人工处置 terminal failure
  时，记录一条 `diagnosis_repair_agent` decision。
- 该 decision 捕获：
  - failure ids 和之前的 recommended routes
  - `retry_execution`、`repair_code`、`revise_spec` 等 human action
  - 下一步受控 product action
  - terminal-failure interrupt 是否仍然 open
- 给 dataset 和 study risk flags 增加
  `terminal_failure_triage_limited_scope`，让 audit reader 能区分 triage
  记录与真正 repair / re-execution。

当前边界：

- 本切片不实现 autonomous repair，也不执行 retry。
- 既有 gate 不变：
  - `repair_code` 只解锁 code regeneration
  - `revise_spec` / `request_new_input` 只解锁 input/spec finalization
  - `retry_execution` 只解锁后续显式 execution request
- diagnosis/repair agent record 是基于现有 graph-owned human decision 的 audit
  metadata，不是第二套 workflow state machine。

验证：

```text
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage -v
python -B -m unittest tests.test_agents_contract tests.test_graph_gateway tests.test_api_phase8 -v
```

### 2026-05-30 - LG2.6 Local R Environment-Control 切片

已完成：

- 给 `RRunRequest` 增加可选 `environment` 和 `arguments` 字段，并让
  `LocalRRunner` 传给底层 `subprocess.run()`。
- 给 local sandbox 增加环境变量 allowlist。默认情况下，generated-code R
  execution 只收到一小组运行时环境变量，不会收到 API key、`R_PROFILE_USER`
  或 `R_ENVIRON_USER`。
- 保留显式空 allowlist 的语义。这样“完全不继承环境变量”和“使用默认
  allowlist”不会混在一起。
- local sandbox 调用 Rscript 时加入 `--vanilla`，避免 generated-code run 读取
  用户级 R startup files 或 saved workspace。
- 在 sandbox boundary metadata 中记录 environment control 和 Rscript arguments，
  方便 validation/audit report 展示本地 runner 实际做了什么。
- 按用户审查意见再次重申 static-rule governance：静态检查必须保持 generic
  contract/rule-pack checks，不能退化成 demo-specific patch rules。

当前边界：

- 这仍然是 local developer runner，不是系统级隔离。它减少环境变量泄露和 R
  startup file 影响，但不能阻止 generated R code 读写任意文件或访问网络。
- 本切片不新增任何 clinical、dataset-specific、study-specific 或 demo-specific
  static ADaM rule。

验证：

```text
python -B -m unittest tests.test_sandbox -v
python -B -m unittest tests.test_sandbox tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway -v
python -m compileall -q src\adam_agent
git diff --check -- src/adam_agent/tools/r_runner.py src/adam_agent/tools/sandbox.py tests/test_sandbox.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

结果：12 个 sandbox tests 通过；207 个 sandbox/downstream/graph/API 相关测试
通过；compileall 通过；diff check 无 whitespace error。

### 2026-05-30 - LG2.6 Sandbox Forbidden-Call Preflight 切片

已完成：

- 在 `LocalRscriptSandboxRunner` 内新增第二层 fail-closed 执行边界。
- local sandbox 会在启动 Rscript 前扫描即将执行的 R code，并阻止 `system()`
  这类 forbidden R calls。
- 新增中立的 `r_safety` helper，并由 static rules 与 sandbox preflight 共同
  复用，避免 execution-boundary checks 变成两份会漂移的补丁清单。
- detector 会忽略注释和字符串字面量，所以 `"system('not-a-call')"` 这种提示文
  本不会误触发 sandbox block。

当前边界：

- 这仍然不是生产级隔离。Local Rscript 仍是 developer runner，validation
  report 必须继续标记为 not hardened。
- 本切片只增加通用 execution-boundary guard，不新增 clinical、ADaM、CDISC、
  dataset-specific、study-specific 或 demo-specific rule。
- Static code review 仍是 review 前的主要 gate。Sandbox preflight 是最后一
  层执行保护，用于防止 approved code artifact 被篡改或绕过正常 review path
  后仍直接执行。

Focused verification：

```text
python -B -m unittest tests.test_r_safety tests.test_sandbox tests.test_static_rules -v
```

结果：33 个 shared R-safety、sandbox、static-rule tests passed。

### 2026-05-30 - LG2.7 Reference Evidence Dependency Map 切片

已完成：

- 收紧 local UI 的 dependency map 文案，避免把 Reference ADaM 显示成可单独满
  足的 runtime dependency。
- 将浏览器侧投影拆成两类含义：
  - runtime availability：本 run 中已选择、已计划或 runnable；
  - reference evidence：上传的 Reference ADaM，只用于 compare/output-shape
    evidence。
- dataset status 现在把纯 Reference ADaM 存在标为 `reference evidence`，而
  不是 ready runtime input。
- 增加 UI contract regression，防止 dependency map 把
  `hasReferenceAdamEvidence()` 当成 runtime availability。

当前边界：

- 这是 UI graph-state viewer 的语义修正，不改变 backend dependency planning、
  graph execution、LLM prompts 或 sandbox behavior。
- Reference ADaM 仍可作为 comparison/output-shape/dependency evidence，但它
  不是 derivation authority，也不能单独满足 runtime dependency availability。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
```

结果：2 个 focused UI dependency-map tests passed；完整
`tests.test_api_phase8` passed 73 tests。

### 2026-05-30 - LG2.5 Static Rule 非权威 Source Guard 切片

已完成：

- 收紧 static-rule governance：rule-pack 不只检查 `authority_type`，也检查
  `source` 是否是真正规则来源。
- `StaticRulePack` 准入现在会拒绝以下 note-like source，防止它们成为 binding
  rule：
  - `demo-observation`
  - `implementation-note`
  - `candidate-rule`
  - `reviewer-note`
- 拒绝范围覆盖下划线/空格变体和带后缀形式，例如
  `demo-observation-2026`，避免把实现备注改个名字伪装成 user policy。
- 更新 `StaticRulePolicy` governance metadata，让 static-check report 明确说明
  note-like source 不能作为正式 rule-pack source。
- 同步更新中英文 LG2 施工方案，把这条作为硬设计边界。

当前边界：

- 本切片不新增任何 clinical、ADaM、CDISC、P21、study-specific、
  dataset-specific 或 variable-specific rule。
- 合理的 `user_policy`、`company_standard`、`p21_rule` 和 `cdisc_standard`
  rule pack 仍然可用，前提是它们提供真实 source、version、scope、severity 和
  evidence metadata。
- 静态检查仍然是 generic contract/rule-pack governance。Demo 观察可以进入
  candidate backlog，但不能直接 block run。

验证：

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_static_rules tests.test_graph_gateway tests.test_api_phase8 -v
```

后缀加固前结果：26 个 static-rule tests passed；151 个 static/gateway/API
相关测试通过。

子 agent 审查：

- 只读子 agent review 返回 GO。
- 未发现 blocking issue。
- 审查建议补上带后缀的 note-like source guard；已在最终验证前落实。

### 2026-05-30 - LG2.8 Legacy Workflow Write Boundary 切片

已完成：

- 将 service 层剩余的 `workflow_state.json` 直接写入收口到两个明确命名的旧
  `/runs` 兼容 helper：
  - `_write_legacy_run_blocked_workflow_state`
  - `_write_legacy_run_completion_workflow_state`
- 更新 `run_study_from_request()`，让主体流程委托这两个 helper，而不是内联调用
  `update_workflow_state()`。
- 增加 AST 回归保护：扫描 `api/service.py`，如果未来有任何新的 service function
  在这两个 legacy helper 之外直接写 workflow state，测试会失败。
- 增加第二条 AST guard：这两个 legacy helper 只能由 `run_study_from_request()`
  调用，防止它们变成可被其他 service function 复用的状态写入口。
- 产品 endpoint 边界保持不变：dataset product actions 仍必须进入
  `GraphGateway` product methods，不能调用低层 recorder 或 workflow-state 写入
  helper。

当前边界：

- 本切片不删除旧 `/runs` shim；只是把它的 direct projection writes 明确化并加
  guard。
- 上传失效仍通过 `GraphGateway.mark_all_inputs_changed()` 加现有 legacy projection
  invalidation helper 处理。
- 静态规则行为不变。Static checks 仍然是 generic contract/rule-pack checks，不是
  demo/study/dataset-specific 补丁。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_legacy_run_endpoint_owns_only_remaining_service_workflow_writes tests.test_api_phase8.Phase8ApiTests.test_run_study_from_request_delegates_legacy_workflow_writes tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
```

结果：新增 helper-caller guard 前 4 tests passed；补充 helper-caller guard 后，
focused helper guards 重新运行通过。

### 2026-05-30 - LG2.2 DatasetGraph Non-Legacy Route Guard 切片

已完成：

- 给 `DatasetGraph` 增加 route-level 回归保护，确保非 legacy modes 不会落回旧
  stub chain：
  - `graph_product_prepare`
  - `graph_product_generate_code`
  - `graph_product_execute`
  - `llm_downstream_stubbed`
  - `llm_downstream_provider`
  - `llm_downstream_r_sandbox`
  - 已退休的 `real_adsl_minimal`
- 保留已有 graph-shape guard：product agent nodes 直接进入
  `summarize_dataset`，不进入 `draft_lineage_stub`。
- 不删除 legacy stub nodes。它们仍只通过显式 `stub_chain` 分支供旧测试/兼容使用。

当前边界：

- 这是 regression guard，不改变 `DatasetGraph` runtime behavior、product steps、
  LLM prompts、R execution 或 sandbox handling。
- Stub modes 仍存在，但 product 和 LLM downstream 路径现在有测试保护，避免意外
  回流到旧 stub chain。
- 静态规则行为不变，仍然是 generic contract/rule-pack layer。

Focused verification：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_nodes_do_not_flow_through_legacy_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_non_legacy_modes_never_route_to_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_uses_input_spec_without_stub_code tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

结果：4 tests passed。

### 2026-05-30 - LG2.5 Static Rule Authority Admission 切片

已完成：

- 收紧静态规则设计，确保 generic engine 仍是 contract/rule-pack 层，而不是
  demo 或 dataset-specific 补丁清单。
- 给 `StaticRulePack` 和 `StaticRulePackItem` 增加 `authority_type`。
- 允许的 authority class 被刻意限制为：
  - `cdisc_standard`
  - `p21_rule`
  - `company_standard`
  - `user_policy`
- rule-pack item 继承 pack authority。如果 item 声明不同 authority type，
  准入会 fail closed。
- Static-rule policy 输出现在记录 rule-pack admission 与 candidate-rule 治理
  说明，让报告明确：demo observations 在通过受治理的 rule-pack 流程前，只能是
  reviewer notes。
- 更新施工方案措辞：未来任何 standards rule 要影响 review 或 execution，必须先
  具备 authority_type/source/version/scope/severity/evidence。

当前边界：

- 本切片不新增任何 clinical、ADaM、CDISC、P21、study-specific 或
  dataset-specific static rule。
- 当前 generated-R checks 仍只覆盖 generic artifact contracts、
  execution-boundary checks，以及 caller-approved spec identifier visibility。
- 没有 `authority_type` 的旧 rule-pack payload 会被有意拒绝。当前产品路径没有
  依赖加载这类 legacy rule-pack payload。

子 agent 审查：

- 子 agent 复审返回 GO。
- 未发现 blocking issue，并确认本改动保持了“静态规则通用化、非补丁式”的方向。
- 非阻断后续项：未来 audit hardening 可以校验 authority type 与 source/version
  的语义匹配，而不只是字段存在。

Focused verification：

```text
python -B -m unittest tests.test_static_rules -v
```

结果：23 tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

结果：116 tests passed。

```text
python -B -m unittest tests.test_static_rules tests.test_state_schemas tests.test_graph_smoke -v
```

结果：89 tests passed。

```text
python -m compileall -q src\adam_agent
```

结果：passed。

### 2026-05-30 - LG2.8 Service Preflight Ownership 切片

已完成：

- 从 FastAPI compatibility service wrappers 中移除对
  `GraphGateway.validate_product_step_start()` 的直接调用，覆盖：
  - finalize inputs
  - draft spec generation
  - code generation
- fail-closed 行为继续保留在 GraphGateway product methods 内部。service 层仍负责
  request shape 和 API response，但不再拥有这些 product steps 的
  terminal-failure preflight routing。
- 新增 AST 回归测试，确认这些 service wrappers 不再直接调用 gateway preflight
  method。
- 新增运行时回归测试，确认 `/draft-spec` 会先被 GraphGateway terminal-failure
  preflight 拦截，不会绕过进入 DatasetGraph。

当前边界：

- 这是责任边界清理，不改变 route path、response schema、dependency planning
  semantic、terminal-failure routing、LLM provider、static checks 或 R execution。
- 已被下一段 LG2.8 切片取代：dependency-plan gate 现在已经移入
  GraphGateway product methods；compatibility service wrappers 只负责
  request/config 解析和 API response shape。
- 本切片不新增任何 clinical/static ADaM rule。静态检查仍然只做 generic
  contracts 和 source-backed rule-pack governance。Demo 观察不能直接升级为
  blocking check，除非先被改写成通用 contract，或通过带 authority、scope、
  severity、evidence 的 versioned rule pack 准入。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_api_phase8.Phase8ApiTests.test_draft_spec_uses_gateway_terminal_failure_preflight -v
```

结果：2 tests passed。

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_generate_code_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_draft_spec_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_requires_repair_code_before_regenerating_code tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review -v
```

结果：7 tests passed。

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
```

结果：118 tests passed。

### 2026-05-30 - LG2.8 Gateway Product Dependency Gate Ownership 切片

已完成：

- 将 dependency-plan gate 移入 GraphGateway product methods，覆盖：
  - finalize inputs
  - 通过 `finalize_inputs(force_new_draft_spec=True)` 进入的显式 draft spec
    generation
  - R code generation
  - approved R execution
- 从 FastAPI compatibility service wrappers 中移除直接
  `dependency_gate_for_product_step()` 调用和 dependency artifact 构造。
- 保留显式 `dependency_gate_for_product_step()` 作为诊断和 dependency-review
  API 边界，但产品步骤不再依赖 service 层先调用它。
- 将 code generation 所需的 dependency artifact 解析移入 GraphGateway，并继续
  排除 reference ADaM，避免 reference ADaM 被当成 runtime dependency。
- 移除 `GraphGateway.generate_code()` product method 对外部
  `dependency_artifacts` 的注入口。runtime dependency artifacts 总是由
  gateway-owned dependency plan 派生后再写入 code-review state。
- 移除 `GraphGateway.finalize_inputs()`、`GraphGateway.generate_draft_spec()`
  和 `GraphGateway.generate_code()` 对外部 `dependency_resolution` 的注入口。
  这些 product methods 现在总是从 gateway-owned plan 读取 dependency
  resolution。
- 在 GraphGateway 内新增 dependency-review handoff：
  - blocking dependency status 仍然在进入产品图前 fail closed；
  - 非阻断的 `no_dependency_evidence` review 会保留在审计 metadata 中，但不再
    留下一个 study-level `dependency_review` interrupt 去遮住产品级
    `draft_spec_review` 或 `code_review` interrupt。
- 加强回归测试，确认：
  - product service wrappers 不能直接调用 `validate_product_step_start()` 或
    `dependency_gate_for_product_step()`；
  - `GraphGateway.generate_code()` 不暴露 caller-provided
    `dependency_artifacts` 参数；
  - GraphGateway product spec/code methods 不暴露 caller-provided
    `dependency_resolution` 参数。

当前边界：

- public route path 和 response schema 不变。
- FastAPI service wrappers 仍负责 config/provider override 解析和 API response
  model 构造。它们不再拥有 product step 的 terminal-failure preflight 或
  dependency-plan gate。
- study-level dependency planning 仍属于 `StudyGraph`；本切片只把 product-step
  gate 和 handoff responsibility 移入 `GraphGateway`。
- static-rule governance 不变。本切片不新增 clinical、dataset-specific、
  study-specific 或 demo-specific static check。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_code_does_not_accept_external_dependency_artifacts tests.test_graph_gateway.GraphGatewayTests.test_gateway_product_spec_methods_do_not_accept_external_dependency_resolution tests.test_api_phase8.Phase8ApiTests.test_execute_rejects_changed_runtime_dependency_artifact -v
```

结果：6 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_returns_plan_when_open tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_do_not_own_terminal_failure_preflight tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_generate_code_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_draft_spec_dependency_gate_does_not_write_service_start_projection tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry -v
python -m compileall -q src\adam_agent
```

结果：6 gateway tests passed；5 API tests passed；compileall passed。

### 2026-05-30 - LG2.8 Gateway-Owned Dependency Plan 验证切片

已完成：

- 增加回归测试，覆盖 `GraphGateway.generate_code()` 必须使用 gateway-owned
  canonical dependency plan 中的 dependency resolution，而不是调用方传入的
  dependency list 这一边界。
- 该测试先同时 seed 一个 graph-backed 且 completed 的 `ADSL` run output 和一个
  decoy `reference_adam/adsl.csv`，再重新 plan `ADAE`，随后验证：
  - `DatasetGraph` invocation 收到当前 graph plan 里的 `ADAE -> ADSL`
    dependency resolution；
  - generated-code state 记录同一个 graph-backed runtime dependency artifact；
  - runtime dependency artifact 指向 `run_output`，不是 decoy reference ADaM。
- 未修改 runtime logic；现有实现已经满足这个边界，本切片只是补防回归测试。

当前边界：

- `GraphGateway.record_code_generation()` 仍是 tests 和 graph internals 使用的
  lower-level trusted persistence boundary。产品调用方应使用
  `GraphGateway.generate_code()`，由它从 graph-owned dependency plan 派生
  dependency artifacts。
- 本切片不改变 dependency planning semantics、UI 行为、static rules、R
  execution 或 provider behavior。
- static-rule governance 不变：静态检查仍然是 generic contract/rule-pack
  checks，不是 demo/study/dataset-specific rules。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_code_uses_gateway_owned_dependency_plan -v
```

结果：1 focused test passed。

### 2026-05-30 - LG2.8 Product Service Low-Level Recorder Guard 切片

已完成：

- 加强 product service wrapper 的 AST 防回归测试，确保 dependency review、
  finalize inputs、显式 draft spec generation、draft-spec review、R code
  generation、code review、approved R execution 和 terminal-failure review
  这些兼容 endpoint 不能直接调用低层 GraphGateway recorder methods，也不能
  直接调用 workflow-state 写入 helper。
- 被保护的 service wrappers 必须继续通过 GraphGateway product/review methods
  进入，例如 `finalize_inputs()`、`generate_draft_spec()`、`generate_code()`、
  `review_code()` 和 `execute_approved_code()`。
- 这是 regression guard，不修改 runtime logic。

当前边界：

- `GraphGateway.record_compare()` 仍是显式 compare/report action boundary，
  不属于本切片保护的 product split-flow wrapper guard。
- 低层 recorder methods 仍可供 tests 和 graph internals 使用，但 compatibility
  product endpoints 不应把它们当业务入口。
- static-rule governance 不变。本切片不新增 clinical、dataset-specific、
  study-specific 或 demo-specific static check。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods -v
```

结果：1 focused test passed。

### 2026-05-30 - LG2.8 Graph-State Input Upload Invalidation 切片

已完成：

- 将 study input 上传后的失效处理接入 graph-owned stale marking，而不是只更新
  UI 侧 `workflow_state.json` read model。
- 新增 `GraphGateway.mark_inputs_changed()`：
  - 读取 canonical `graph_state.json`
  - 重新计算 study input fingerprint
  - 写入 `dependency_plan.plan_stale`、`dependency_plan.input_diff` 和
    stale reason
  - 输入变化后打开 study-level `dependency_review` interrupt
  - 将已有产品进度的 dataset 标记为 `needs_review`
  - 如果已有 generated code，则将 code state 标记为 `stale`
  - 如果只有 spec/draft-spec 进度，则将 spec state 标记为 `stale`
  - 从 graph state 重新投影 `workflow_state.json`
- `/studies/files` response 新增 `touched_graph_runs`，让调用方能区分哪些
  active graph run 被 canonical graph invalidation 触达，哪些只是 legacy
  workflow read-model touch。
- Product step 现在遇到 stale dependency plan 会 fail closed。用户必须重新运行
  dependency planning，之后才能继续 finalize inputs、draft spec 或 code
  generation。

静态规则边界重申：

- 本切片不新增任何 clinical/static ADaM rule。
- 静态检查仍然只做 generic contract/rule-pack checks。
- 未来 blocking rule 必须验证某个已经声明的 contract，并说明权威来源：
  system contract、approved spec、user policy，或带 authority_type、source、
  scope、severity、evidence 的 source-backed versioned rule pack。
- demo failure 或 PSY201 观察只能成为 generic contract 的测试，或进入受治理的
  rule-pack item 候选；不能作为 dataset/study/file-specific 分支写入 generic
  static-rule engine。

当前边界：

- `workflow_state.json` 仍是 projection，不是 source of truth。
- 没有 `graph_state.json` 的 legacy run 在上传时会跳过 graph invalidation，
  仍只受 legacy read-model invalidation 保护。
- UI 仍会在上传后清空浏览器端缓存的 plan/code/review 数据；真正的后端保护是
  canonical graph-state invalidation。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_inputs_changed_updates_canonical_state_and_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_inputs_changed_raises_for_legacy_run_without_graph_state tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_workflow_state_stale tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
```

结果：4 focused tests passed。

相关验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
```

结果：111 gateway/API tests passed。

子 agent 审查：

- 只读子 agent 审查返回 GO。
- 未发现阻断性的 schema、backcompat、stale gate 或 static-rule boundary 问题。
- 后续切片提醒：上传失效目前仍先从 legacy workflow read model 发现 active
  runs，再更新 canonical graph state。后续 hardening 应直接扫描
  `runs/*/graph_state.json`，并把 `workflow_state.json` 只当 projection。

### 2026-05-30 - LG2.8 Canonical Graph-Run Upload Invalidation Hardening 切片

已完成：

- 将上传失效时的 graph-run discovery 移入 `GraphGateway`。
- 新增 `GraphGateway.list_graph_runs()`，从 `runs/*/graph_state.json` 发现
  canonical graph runs，不再依赖 UI `workflow_state.json` projection。
- 新增 `GraphGateway.mark_all_inputs_changed()`，API 上传路径可以失效所有
  stored input fingerprint 已变化的 canonical graph runs。
- `/studies/files` 现在返回：
  - `touched_runs`：legacy workflow read-model invalidations
  - `touched_graph_runs`：canonical graph-state invalidations
  - `skipped_graph_runs`：无法加载或更新的 graph-state runs
- 增加关键 failure mode 回归：即使 `workflow_state.json` 缺失，上传后仍能发现
  `graph_state.json`，将 dependency plan 标记为 stale，打开 graph-level
  `dependency_review`，并从 canonical state 重新生成 workflow projection。
- 重复 upload rescan 时，如果 run 已 stale 但本次没有新的 input change，会继续保持
  fail-closed stale 状态，但不会重复报告为 newly touched graph run。
- 增加 corrupt `graph_state.json` 的 skipped-run reporting 和回归测试。
- 更新 UI 上传提示，让它报告 `touched_graph_runs` 中的 canonical graph
  invalidations，而不只看 legacy workflow projection touches。

静态规则边界重申：

- 本切片不修改 static-rule logic，也不新增 clinical、dataset-specific、
  study-specific 或 demo-specific rule。
- 静态检查继续只做 generic contract/rule-pack checks。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_scans_canonical_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_preserves_existing_stale_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_reports_corrupt_graph_state_as_skipped tests.test_api_phase8.Phase8ApiTests.test_upload_invalidates_graph_run_even_when_workflow_projection_is_missing tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
```

结果：5 focused tests passed。

相关验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
python -B -m unittest tests.test_static_rules tests.test_state_schemas tests.test_graph_smoke -v
```

结果：115 gateway/API tests passed；87 static/state/graph smoke tests passed。

### 2026-05-30 - LG2.8 Legacy `/runs` Shim Boundary 切片

已完成：

- 给 `POST /runs` 一次性运行 response 增加明确 legacy metadata：
  - `workflow_control: legacy_run_to_completion_compatibility_shim`
  - `graph_state_path: null`
  - `workflow_state_path`
- LLM run-to-completion 被拒绝的路径也在 `workflow_state.json` 中写入同样的
  legacy 标记，同时保留 `current_interrupt: split_flow_required` 和原有错误提示，
  指向 `/runs/prepare` 与 dataset-level review gates。
- 增加 API 回归测试覆盖两条路径：
  - stub `/runs` 可以成功，但明确标记为 legacy compatibility
  - LLM `/runs` 被拒绝，并记录必须走 product split-flow
- 更新 `docs/phase8_1_api_contract.md`，避免文档继续暗示真实 LLM/R 生成应使用
  `POST /runs`。
- 根据子 agent 审查意见，更新 `RunStudyRequest` schema docstring，让 OpenAPI
  schema 也把 `/runs` 描述为 legacy/smoke compatibility。

当前边界：

- 本切片不删除 `POST /runs`，也不改变它的 stub/smoke 行为。
- 本切片不会为 legacy run-to-completion 路径创建 canonical `graph_state.json`。
  `graph_state_path: null` 是有意设计，让调用方能区分它和 GraphGateway-owned
  product flow。
- 真实 LLM/R ADaM generation 仍然被此 endpoint 阻断，必须走 `/runs/prepare`
  加 dataset-level finalize/draft/code-review/execute gates。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_demo_study_rejects_run_to_completion_llm_endpoint -v
```

结果：3 tests passed。

Broader verification：

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_static_rules tests.test_reference_store tests.test_state_schemas -v
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md docs/phase8_1_api_contract.md src/adam_agent/api/models.py src/adam_agent/api/service.py tests/test_api_phase8.py
python -m compileall -q src/adam_agent
```

结果：63 个 Phase 8 API tests 通过；135 个 graph/static/schema 相关测试通过；
diff check 和 compileall 在子 agent review 前通过。

子 agent 审查：

- 子 agent review 返回 GO，未发现 blocking finding。
- 非阻塞建议是澄清 `RunStudyRequest` schema docstring；提交前已处理。

### 2026-05-30 - LG2.8 产品 Read-Model 写入隔离切片

已完成：

- 移除产品 compatibility endpoints 对 `mark_workflow_inputs_current()` 的直接调用：
  - `generate-code`
  - `finalize-inputs`
  - 显式 `draft-spec`
- 这些 endpoint 现在不再在 graph-owned 产品步骤之前写 service-owned `*_start`
  checkpoint；`workflow_state.json` 更新交给 `GraphGateway`/canonical graph
  projection。
- 增加 API 回归覆盖：当 `finalize-inputs`、`generate-code` 或显式
  `draft-spec` 被 dependency gate 阻止时，持久化的 `workflow_state.json`
  仍然是 LangGraph projection，不会被覆盖成 service-owned `*_start` node。
- 更新 LG2 baseline：`workflow_state.json` 是 compatibility read model，不是
  产品事实来源；剩余直接兼容写入都属于要移除或隔离的 legacy surface。

当前边界：

- 本切片不删除 `workflow_state.json`；当前 UI 仍把它作为 compatibility
  projection 读取。
- Upload invalidation 和旧 `/runs` 非 LLM compatibility path 仍使用
  workflow-state helpers。本切片只移除产品 split-flow start writes，避免它们与
  graph projection 竞争。
- Canonical input fingerprint 保护仍在 `GraphGateway` 和 graph state records
  中完成。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code -v
```

结果：初始 3 个 focused tests passed。子 agent review 后，为 `generate-code`
和显式 `draft-spec` 补充两个 dependency-gate read-model 测试；3 个
read-model isolation tests passed。

Broader verification：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
git diff --check -- src/adam_agent/api/service.py tests/test_api_phase8.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
python -m compileall -q src/adam_agent
```

结果：183 个 gateway/API/graph/static/reference 相关测试通过；49 个 core tests
通过；diff check 和 compileall 通过。

静态规则方案调整：

- LG2.5 方案现在明确：静态规则必须从底层原则设计成可复用的 contract check，
  不能把 demo failure 直接写成补丁规则。
- 增加 rule lifecycle checklist：candidate observation -> generic contract
  -> authority binding -> deterministic check -> reviewer visibility。
- 再次明确 generic engine 里没有“例外登记表”；未来 clinical/domain rule 要能
  block run，必须先进入 approved spec 或带来源的 rule pack。

子 agent 审查：

- 子 agent review 返回 GO。
- 未报告 blocking finding。
- 主要非阻塞建议是把相同的 rejected-gate read-model invariant 补到
  `generate-code` 和显式 `draft-spec`；提交前已补上。
- 小幅清理测试 helper 名称后，最终子 agent 复核仍未发现 blocking finding。

### 2026-05-30 - LG2 静态规则边界与 Terminal-Failure Review Gateway 切片

已完成：

- 按产品级约束加固 LG2.5 static-rule 方案：静态检查必须是通用
  contract/rule-pack 检查，不能变成针对 PSY201、某个上传文件、某个 ADaM
  dataset，或 demo 中某个临床变量个案的补丁规则。
- 增加未来 static-check 工作的 rule-design review checklist：每条能 block
  run 的规则都必须说明它检查的 declared contract，以及规则权威来源：
  system contract、approved spec、user policy，或带 source/scope/severity/evidence
  的 versioned rule pack。
- 新增 `GraphGateway.review_terminal_failure()`，作为 terminal-failure triage
  compatibility endpoint 的 graph-owned 入口。
- `persist_terminal_failure_review()` 现在把 graph-state loading、dataset
  validation、action normalization 和 `HumanCommand` construction 委托给
  `GraphGateway`。
- 增加 gateway-level 回归测试，确认新入口能持久化 retry triage decision、
  清理 terminal-failure interrupt，并在 canonical graph state 中记录 next action。
- 增加 wrapper 负向测试，确认 `GraphGateway.review_terminal_failure()` 会拒绝不支
  持的 decision，并且当 dataset 没有处于 open `terminal_failure` interrupt 时
  不允许写入 review 状态。

当前边界：

- 本切片不新增任何 clinical/static ADaM rule。
- 静态检查继续只做 generic contract/rule-pack checks。未来 dataset-specific
  standards rule 必须通过带来源的 rule pack 进入，不能写成 generic engine 里的
  分支逻辑。
- 本切片不改变 terminal-failure routing semantics：
  `retry_execution`、`repair_code`、`revise_spec`、`request_new_input`、
  `skip_dataset`、`continue_other_datasets` 保持既有行为。
- `record_terminal_failure_review()` 仍是 tests 和 graph internals 使用的
  lower-level recorder；`review_terminal_failure()` 是更安全的 compatibility
  endpoint 入口。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_skip_terminal_failure_marks_study_failed tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_skip_dataset_does_not_unlock_code_regeneration -v
```

结果：子 agent review 前，初始 7 tests passed。子 agent review 后补充两个
wrapper 负向测试，9 个 focused tests passed。

Broader verification：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
git diff --check -- src/adam_agent/graph/gateway.py src/adam_agent/api/service.py tests/test_graph_gateway.py docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
python -m compileall -q src/adam_agent
```

结果：181 个 gateway/API/graph/static/reference 相关测试通过；49 个 core tests
通过；diff check 和 compileall 通过。

子 agent 审查：

- 子 agent review 返回 GO。
- 未报告 major business、logic 或 schema issue。
- 唯一建议是给新 gateway wrapper 增加直接负向测试；提交前已补上。

### 2026-05-30 - LG2.2 Gateway Execution Approval Preflight 切片

已完成：

- 将“执行前必须在 canonical graph state 中存在已批准 code review”这个
  preflight 前移到 `GraphGateway.execute_approved_code()`。
- Gateway 现在会先验证 graph-owned code-review decision，再调用
  `DatasetGraph` 的 `graph_product_execute` mode。
- 这个 preflight 复用现有 graph execution contract 检查：
  - `code_state.status` 和 `decision` 已批准
  - 当前 input fingerprint
  - review artifact path
  - generated R code hash
  - static-check artifact path/hash/schema/status
  - 如存在 approved spec，则检查 spec path/hash
  - runtime dependency artifact hash
- 新增 gateway 测试，确认当 graph state 只有 generated code、但没有 approved
  code-review decision 时，会在调用 `DatasetGraph` 之前 fail closed。
- 更新正向 gateway execution 测试，使其按产品流程 seed：
  generate-code -> code-review -> execute。

当前边界：

- 这是 workflow contract hardening，不是新增 clinical/static ADaM rule。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。
- 更深层 execution boundary 仍保留同样的 approval 检查；本次只是增加更早的
  fail-closed gate，不替代 sandbox 侧验证。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_execute_requires_graph_approved_code_before_dataset_graph_invocation -v
```

结果：2 focused tests passed。

### 2026-05-30 - LG2.2 Gateway-Owned Code Review Artifact 切片

已完成：

- 新增 `GraphGateway.review_code()`，作为 code-review compatibility endpoint
  的 graph-owned 入口。
- Gateway 现在拥有完整 code-review transition：
  - 验证 generated code 已记录在 canonical graph state 中
  - 验证当前 generated-code hash、static-check artifact、approved spec hash、
    dependency artifact hash 和 input fingerprint
  - 写入 `runs/{run_id}/review/{dataset}_code_review.json`
  - 将 code-review decision 持久化进 `graph_state.json`
  - 刷新 UI 使用的 `workflow_state.json` projection
- 将 `api/service.py::persist_code_review()` 收缩为 request validation、
  委托 `GraphGateway.review_code()` 和 compatibility response construction。
- 新增 gateway 测试，覆盖直接 review-code 入口，以及 review artifact 写出后
  graph-state recording 失败时的清理行为。

当前边界：

- public route path 和 response shape 不变。
- 本切片把 artifact/state ownership 移入 Gateway，但还没有新增真正的
  LangGraph interrupt-resume API。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_writes_artifact_and_records_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_cleans_artifact_when_recording_fails tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_dataset_code_review_in_canonical_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_code_review_cleans_approval_json_when_graph_recording_fails tests.test_api_phase8.Phase8ApiTests.test_code_review_requires_graph_generated_code_state tests.test_api_phase8.Phase8ApiTests.test_code_approval_is_invalidated_when_code_or_inputs_change -v
```

结果：3 focused gateway tests passed；4 focused API tests passed。

### 2026-05-30 - LG2.2 Gateway-Owned Draft-Spec Review Artifact 切片

已完成：

- 新增 `GraphGateway.review_draft_spec()`，作为 draft-spec review
  compatibility endpoint 的 graph-owned 入口。
- Gateway 现在拥有 draft-spec review transition：
  - 验证 draft spec 已记录在 canonical graph state 中
  - 在信任 draft spec 前验证 draft-spec hash 和当前 input fingerprint
  - 写入 `runs/{run_id}/reviews/{dataset}_draft_spec_review.json`
  - 当人工决定 approve 时，写入
    `runs/{run_id}/approved_specs/{dataset}_approved_spec.json`
  - 将 review decision 持久化进 `graph_state.json`
  - 刷新 UI 使用的 `workflow_state.json` projection
- 将 `api/service.py::persist_draft_spec_review()` 收缩为 request validation、
  委托 `GraphGateway.review_draft_spec()` 和 compatibility response construction。
- 新增 gateway 和 API 测试，覆盖直接 review-draft-spec 入口，以及 review
  artifacts 写出后 graph-state recording 失败时的清理行为。

当前边界：

- public route path 和 response shape 不变。
- 本切片把 draft-spec review artifact/state ownership 移入 Gateway，但还没有
  新增真正的 LangGraph interrupt-resume API。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_draft_spec_writes_artifacts_and_records_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_draft_spec_cleans_artifacts_when_recording_fails tests.test_api_phase8.Phase8ApiTests.test_draft_spec_review_cleans_artifacts_when_graph_recording_fails -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_draft_spec_review_in_canonical_state tests.test_api_phase8.Phase8ApiTests.test_missing_input_spec_requires_draft_spec_approval_before_code_generation tests.test_api_phase8.Phase8ApiTests.test_approved_draft_spec_is_invalidated_when_inputs_change tests.test_api_phase8.Phase8ApiTests.test_approved_draft_spec_is_invalidated_when_approved_file_changes -v
```

结果：3 focused ownership/cleanup tests passed；4 existing draft-spec review
regression tests passed。

### 2026-05-30 - LG2.2 Gateway-Owned Compare Report Artifact 切片

已完成：

- 将 compare-report artifact 写入迁到 `GraphGateway.record_compare()` 后面。
- service 仍然计算当前 CSV 结构/reference compare，但对于已有 graph state 的
  run，不再直接写 `runs/{run_id}/compare/{dataset}_compare_report.json`。
- `GraphGateway.record_compare()` 现在可以在一个 graph-owned transition 中：
  写 compare report artifact、把 artifact 记录到 canonical dataset state、
  更新 compare summary，并刷新 UI 使用的 `workflow_state.json` projection。
- 保留无 graph state 的 ad-hoc compare 兼容行为：endpoint 返回临时 compare
  response，但不创建 graph state，也不写 canonical compare report。
- 新增 gateway 和 API 覆盖，验证 Gateway 写 compare report，以及无 graph
  state 时的兼容边界。
- 明确 read/write 边界：`/review-summary` 可以为展示临时计算 compare status，
  但不能调用 `record_compare()`，也不能修改 canonical graph/workflow state。
  显式 `/datasets/{dataset}/compare` 仍是 compare/report 持久化动作。

当前边界：

- 本切片不改变 compare algorithm。它仍然只是初始 CSV 结构和 sampled-cell
  comparison，不是 clinical conformance validator。
- review-summary 是 read-model endpoint。reference 文件在上一次显式 compare
  后发生变化时，summary 可以展示当前临时比较状态；graph state 仍保留上一次
  显式 compare 记录，直到用户再次运行 compare。
- Reference ADaM 仍然只是 compare/output-shape evidence。本切片不把 reference
  ADaM 变成 derivation authority。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_reports_compare_without_mutating_graph_when_reference_disappears tests.test_api_phase8.Phase8ApiTests.test_review_summary_read_model_helpers_do_not_record_compare tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

结果：7 focused compare/read-model tests passed；178 related gateway/API/graph/static/
reference tests passed。

### 2026-05-30 - LG2.2 显式 Draft-Spec Gateway 切片

已完成：

- 新增 `GraphGateway.generate_draft_spec()`，作为显式 `/draft-spec`
  compatibility endpoint 的 graph-owned 入口。
- 该 endpoint 现在把 fresh draft-spec generation 委托给 Gateway 和
  `DatasetGraph`，不再在 `api/service.py` 里自行构建 LLM context、调用 provider，
  再手动记录 graph state。
- 在 dataset graph state 增加 `force_new_draft_spec`，让显式 draft-spec 生成可
  以请求新的 review-required draft，而不是复用已批准的 draft spec。
- 当用户已经提供 `input_spec` 时，Gateway 会拒绝显式 draft-spec generation，
  且不会写入 canonical graph state。
- dependency gate 本切片仍保留在 service compatibility wrapper，符合当前
  LG2.2 边界。

当前边界：

- public route path 和 response shape 不变。
- `finalize-inputs` 和显式 `/draft-spec` 现在共用 graph-owned
  prepare/draft-spec 路径，但用户意图不同：`finalize-inputs` 可以接受已有
  input spec 或 approved draft；显式 `/draft-spec` 只在没有 input spec 时强制
  生成 fresh draft。
- 静态规则继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generate_draft_spec_forces_fresh_draft_and_rejects_input_spec tests.test_api_phase8.Phase8ApiTests.test_missing_input_spec_requires_draft_spec_approval_before_code_generation tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

结果：4 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

结果：169 related gateway/API/graph/static/reference tests passed；49
additional core tests passed。

### 2026-05-30 - LG2.2 GraphGateway-Owned Dependency Gate 切片

已完成：

- 新增 `GraphGateway.dependency_gate_for_product_step()`，作为 dataset product
  steps 的 graph-owned dependency gate。
- 将产品步骤的 dependency decision 从 `api/service.py` 移到 Gateway，覆盖：
  - finalize inputs
  - 显式 draft spec generation
  - R code generation
  - approved R execution
- service compatibility wrappers 现在仍负责 config/provider 解析和 API response
  shape，但是否允许进入产品步骤由 Gateway 判断。
- 删除旧的 service-local `_assert_target_dependency_gate_open_for_product_step()`
  和重复读取/启动 plan 的 helper，避免 FastAPI 层复制 graph dependency state
  决策。
- 新增 gateway 层测试，覆盖：
  - 产品步骤没有 plan 时自动启动 dependency plan；
  - 上游 ADaM dependency 未解决时 fail closed；
  - gate 打开时返回 dependency plan 字段。

当前边界：

- public route path 和 response shape 不变。
- dependency planning 本身仍属于 `StudyGraph`；本切片只把产品步骤 gate ownership
  移到 `GraphGateway`。
- 静态规则继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_starts_plan_and_blocks_unresolved_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_gate_returns_plan_when_open tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_review_required_dependency_evidence tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_dependency_warning tests.test_api_phase8.Phase8ApiTests.test_execute_rejects_changed_runtime_dependency_artifact -v
```

结果：5 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

结果：171 related gateway/API/graph/static/reference tests passed；49
additional core tests passed。

### 2026-05-30 - LG2.2 GraphGateway-Owned Execution 切片

已完成：

- 新增 `GraphGateway.execute_approved_code()`，作为 approved R execution 的
  graph-owned 入口。
- 新 gateway method 负责：
  - 通过 `validate_product_step_start(step="execute")` 做 terminal-failure
    preflight；
  - 用 `graph_product_execute` mode 调用 `DatasetGraph`；
  - 抽取兼容 API response 所需字段；
  - 通过 `record_execution()` 写回 canonical graph state。
- 将 `api/service.py::execute_approved_dataset_code()` 收缩成 compatibility
  wrapper：只校验 dependency plan、委托 `GraphGateway` 执行，并保持原 response
  shape。
- 更新 retry-gate 测试，把 patch 点从
  `adam_agent.api.service.compile_dataset_graph` 移到
  `adam_agent.graph.gateway.compile_dataset_graph`，证明 execution graph call
  已经从 service 层移出。
- 新增 gateway 层测试，确认 approved-code execution 会用
  `execution_mode == graph_product_execute` 调用 `DatasetGraph`，写入 canonical
  `graph_state.json`，并刷新 UI projection。

当前边界：

- public route path 和 UI 行为不变。
- dependency-plan gate 暂时仍留在 service compatibility wrapper；等 service
  wrapper 进一步收缩后，再把 dependency gate ownership 移到 gateway。
- 本切片不增加自主 retry 或更宽的 repair policy。terminal failure routing
  仍依赖现有 human terminal-failure review decision。
- 静态规则继续遵守 generic contracts 和 source-backed rule packs 的治理边界；
  本切片没有加入 dataset/study/demo-specific static checks。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path -v
```

结果：3 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules tests.test_reference_store -v
```

结果：111 related gateway/API/static/reference tests passed。

### 2026-05-30 - LG2.2 GraphGateway-Owned Code Generation 切片

已完成：

- 新增 `GraphGateway.generate_code()`，作为 generated R code creation 和
  code-review interrupt persistence 的 graph-owned 入口。
- 新 gateway method 负责：
  - `generate_code` 的 terminal-failure preflight；
  - 用 `graph_product_generate_code` mode 调用 `DatasetGraph`；
  - 抽取 generated-code/static-check/spec artifact hash；
  - 通过 `record_code_generation()` 写 canonical graph state；
  - 抽取兼容 API response 所需字段。
- 将 `api/service.py::generate_dataset_code()` 收缩为：
  - HTTP/request validation；
  - config/provider/exposure resolution；
  - dependency-plan gating；
  - 委托 `GraphGateway.generate_code()`；
  - 构造兼容 response。
- provider builder 和 context builder 仍由 service 注入 gateway，避免把 provider
  policy 写死进 gateway，也保留 browser-scoped provider 测试的边界。
- 新增 gateway 层测试，确认 generated-code orchestration 会用
  `execution_mode == graph_product_generate_code` 调用 `DatasetGraph`，写
  canonical graph state，并刷新 UI projection。

当前边界：

- public route path 和 response shape 不变。
- 在这个切片当时，dependency-plan gate 和 `finalize-inputs` 仍属于
  compatibility-wrapper responsibilities。
- 后续切片已替代该边界：dependency-plan gating 和 `finalize-inputs` 现在是
  graph/gateway-owned product boundaries。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_generate_code_uses_browser_scoped_real_provider_settings -v
```

结果：3 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

结果：166 related gateway/API/graph/static/reference tests passed。

### 2026-05-30 - LG2.2 GraphGateway-Owned Finalize Inputs 切片

已完成：

- 新增 `GraphGateway.finalize_inputs()`，作为 upload-complete/spec-readiness
  checkpoint 的 graph-owned 入口。
- 新 gateway method 负责：
  - `finalize_inputs` 的 terminal-failure preflight；
  - 用 `graph_product_prepare` mode 调用 `DatasetGraph`；
  - 通过 `record_input_spec_ready()` 记录 input-spec ready；
  - 通过 `record_approved_draft_spec_ready()` 记录 approved-draft-spec ready；
  - 通过 `record_draft_spec_generation()` 记录 review-required draft spec。
- 将 `api/service.py::finalize_dataset_inputs()` 收缩为：
  - HTTP/request validation；
  - config/provider/exposure resolution；
  - dependency-plan gating；
  - 委托 `GraphGateway.finalize_inputs()`；
  - 构造兼容 response。
- 移除 service 层直接 `compile_dataset_graph` import。现在 service 不再为产品
  `finalize`、`generate`、`execute` 步骤直接调用 `DatasetGraph`。
- 新增 gateway 层覆盖：
  - existing input_spec 分支；
  - missing-spec draft-spec 分支。

当前边界：

- public route path 和 response shape 不变。
- 在这个切片当时，dependency-plan gate 和独立 `/draft-spec` endpoint 仍有
  compatibility-wrapper responsibilities。
- 后续切片已替代该边界：dependency-plan gating 和显式 draft-spec generation
  现在都通过 graph/gateway-owned product methods。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_uses_existing_input_spec_without_draft_generation tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_generates_review_required_draft_spec_when_spec_missing tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_passes_rscript_path_to_context_builder -v
```

结果：5 focused tests passed。

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_state_schemas tests.test_llm_generated_code tests.test_sandbox -v
```

结果：168 related gateway/API/graph/static/reference tests passed；49
additional core tests passed。

### 2026-05-30 - LG2.5 Static-Rule Governance Clarification And Retry Regression 切片

已完成：

- 根据用户审查意见，收紧 LG2.5 static-rule 方案：静态规则只能验证已经声明的
  contract，不能演变成 demo/dataset-specific 临床观察补丁清单。
- 明确新的 blocking ADaM/CDISC/company-standard 检查必须先进入带来源的
  rule-pack 准入 contract，包含 source、version、scope、declared severity 和
  evidence。
- 增加 source-level 回归保护，防止 demo study 名、dataset 名、demo 中观察到
  的临床变量个案进入 generic `static_rules.py` engine。
- 进一步增加 AST-level 分支扫描：generic static-rule 的分支条件不能依赖 demo/
  study/dataset 名或手挑的临床变量字面量。
- 增加 terminal-failure retry 正向回归：人工选择 `retry_execution` 后，
  `/execute-approved-code` 可以进入 graph-owned `graph_product_execute` 路径，
  并在 graph state 中记录 retry follow-up 已由 execute 消费。

当前边界：

- static-rule guard 只针对 generic engine。dataset 名和 standards 术语仍可以
  出现在测试或未来带版本的 rule-pack fixtures 中。
- AST guard 是结构性约束：它阻止 generic engine 的分支条件退化成补丁表，但仍
  允许 dataset 术语出现在测试、approved spec 和受治理的 rule-pack fixture 中。
- retry 回归使用 mocked DatasetGraph return value，证明的是 compatibility
  wrapper gate 和 graph-state recording path，不是真实 R 执行。

验证：

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules tests.test_sandbox -v
```

结果：24 static-rule tests passed；3 focused retry tests passed；205 core tests
passed。

子 agent 审查：

- 子 agent 复审返回 GO。
- 未发现 major logic flaw、misleading architecture claim 或 invalid retry
  regression。

### 2026-05-30 - LG2.5 Rule-Pack Admission Contract 切片

已完成：

- 新增通用 `StaticRulePack` 和 `StaticRulePackItem` contract。
- 新增 `validate_static_rule_pack_payload()` 和 `load_static_rule_pack()`，
  后续 standards/company rules 必须先通过 provenance 准入，才能在未来切片中
  被使用。
- 当前准入要求 authority_type、source、version、scope、declared severity、
  evidence，以及唯一的 `rule_id`。
- scope value 必须是明确字符串，避免 object-shaped scope payload 变成隐藏的
  engine 逻辑。

当前边界：

- 本切片不执行 standards-pack rules，也不新增任何 ADaM/CDISC 临床检查。
- 它只建立未来 CDISC/P21/company-standard rules 必须进入的治理入口。

验证：

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_static_rules tests.test_llm_generated_code tests.test_downstream_runner tests.test_graph_gateway tests.test_api_phase8 -v
```

结果：20 static-rule tests passed；126 related core tests passed。

### 2026-05-30 - LG2.5 Reference Tool Interface 切片

已完成：

- 新增本地 reference lookup tool contracts：
  - `search_cdisc_reference`
  - `lookup_adam_rule`
  - `lookup_p21_rule`
  - `lookup_company_standard`
- 每个 tool 都映射到 `references/` 下的明确本地 reference root。
- 新增 `ReferenceToolRequest` 和 `ReferenceToolResult`，方便 agent 记录调用了
  哪个工具、查询了什么、返回了哪些 hits，以及有哪些 warnings。
- 将 reference-store 测试拆到 `tests/test_reference_store.py`。

当前边界：

- 这些工具只搜索本地 text-like 文件；不证明 compliance，也不执行 standards
  rules。
- 空检索结果只记录为 warning，不代表某条规则不存在。
- Reference hits 仍然只是 agent/review 的证据，不是隐藏推导权威。
- `reference_root` 只是本地配置/测试参数。如果后续 API 把它暴露给用户或 agent
  输入，必须先增加 resolved-path allowlist 和 symlink escape 检查。

验证：

```text
python -B -m unittest tests.test_reference_store tests.test_static_rules -v
python -B -m unittest tests.test_reference_store tests.test_static_rules tests.test_state_schemas tests.test_llm_context tests.test_downstream_runner tests.test_api_phase8 -v
```

结果：22 focused reference/static tests passed；117 related core tests passed。

子 agent 审查：

- 子 agent 复审返回 GO。
- 唯一残留提醒是：caller-configurable `reference_root` 对本地工具 contract
  可以接受，但在暴露给 API 或 agent-supplied parameter 前必须 hardened。

### 2026-05-30 - LG2.2 Product Stub-Path Isolation 切片

已完成：

- graph-product agent nodes 现在直接进入 `summarize_dataset`：
  - `draft_spec_agent`
  - `generate_r_code_agent`
  - `execute_approved_code`
- legacy stub chain 只保留在 `prepare_dataset` 的显式 `stub_chain` 分支里。
- 移除了 `prepare_dataset` 里对 `graph_product_execute` 的提前执行；R execution
  只发生一次，并且只在显式 `execute_approved_code` graph node 中发生。
- 保留 terminal-failure `revise_spec` 语义：用户选择 revise spec 后，后续
  finalize/draft step 必须生成新的 draft spec，不能复用旧的 approved draft
  spec。
- 增加 graph smoke tests，确认 product nodes 不再流经 `draft_lineage_stub`，并
  确认 `graph_product_execute` mode 下 prepare 不会执行 R。

当前边界：

- legacy stub nodes 仍保留给显式 legacy/test modes。
- FastAPI compatibility endpoints 仍按单步调用 graph modes；本切片只清理
  DatasetGraph 产品路径本身。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review tests.test_graph_smoke -v
```

结果：55 tests passed。

### 2026-05-30 - LG2.2 Execution Preflight Gate 切片

已完成：

- 将 terminal-failure execution gate 前移到 FastAPI compatibility wrapper
  调用 DatasetGraph 之前。
- `/execute-approved-code` 现在会先调用
  `GraphGateway.validate_product_step_start(step="execute")`，再进入
  `graph_product_execute`。因此未审核的 terminal failure 不会启动 execution
  graph 或 R boundary。
- 保留 graph-layer `record_execution()` 里的校验，作为执行后的第二层
  fail-closed guard。
- 保留 execute retry 被 terminal failure 阻止时的用户可读错误信息。
- 增加 API 回归测试：patch `compile_dataset_graph` 并确认第二次执行在
  terminal-failure review 前被阻止时不会调用 DatasetGraph。

当前边界：

- 这是 compatibility-wrapper 的 preflight guard。该 endpoint 仍是一次调用一个
  graph mode；完整 LangGraph interrupt resume 仍留给后续切片。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit -v
```

结果：3 tests passed。

### 2026-05-30 - LG2.8 Graph-Mutating Endpoint Projection 切片

已完成：

- 新增 `_assert_run_projection()` 测试 helper，用来覆盖会修改 run、但 response
  不返回 compatibility shim metadata 的 endpoint。
- 该 helper 读取 `runs/{run_id}/graph_state.json` 和
  `runs/{run_id}/workflow_state.json`，然后运行
  `workflow_projection_consistency()`，确认 UI projection 仍然由 canonical graph
  state 派生。
- 增加代表性覆盖：
  - 生成 compare report 后
  - reference 文件消失后刷新 compare summary
  - terminal-failure review 选择 `repair_code`
  - terminal-failure review 选择 `retry_execution`
  - terminal-failure review 选择 `skip_dataset`

当前边界：

- 这是 regression coverage，不把 `workflow_state.json` 变成 source of truth，也
  不扩大 `workflow_projection_consistency()` 已定义的投影等价范围。
- 它主要覆盖上一轮 LG2.8 compatibility metadata 切片没有覆盖的 graph-mutating
  endpoint 类型。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：58 tests passed。

### 2026-05-30 - LG2.8 Compatibility Shim Metadata 切片

已完成：

- 给旧的 dataset-level 产品 endpoint response 增加明确兼容元数据：
  - `workflow_control: graph_gateway_compatibility_shim`
  - `graph_state_path`
  - `workflow_state_path`
- 覆盖主要兼容 endpoint：
  - finalize inputs
  - draft spec generation
  - draft spec review
  - R code generation
  - code review
  - approved local execution
- 增加 API 测试，确认这些 response 指向真实存在的 canonical graph state 和
  workflow projection 文件。

当前边界：

- 这是 observability/deprecation 切片。它把旧 URL 明确标记为 compatibility
  shim，并让调用方看见 graph ownership。
- 不删除旧 endpoint，不改 route path，也不改变 backend execution order。
- canonical truth 仍然是 `graph_state.json`；`workflow_state.json` 仍是
  compatibility projection。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：58 tests passed。

子 agent 审查：

- 子 agent 复审返回 GO。
- 未报告 major logic bug、schema/backcompat issue 或 misleading metadata。
- 审查确认文档没有 overclaim：这是 compatibility observability/deprecation
  metadata，不改变 gates、execution order、route paths 或 sandbox behavior。

最终验证：

```text
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md src/adam_agent/api/models.py src/adam_agent/api/service.py tests/test_api_phase8.py
```

结果：无 whitespace error。

```text
python -B -m unittest tests.test_api_phase8 -v
```

结果：58 tests passed。

### 2026-05-30 - LG2.8 Dependency Review Gateway Ownership 切片

已完成：

- 新增 `GraphGateway.review_dependency()`，作为 study-level dependency review
  决策的 graph-owned 高层入口。
- 将 dependency-review 的 graph state 读取、interrupt 校验、human command 构造
  和 resume 逻辑收进 gateway。
- 将 `api/service.py::persist_dependency_review()` 缩减为 request validation、
  gateway delegation 和 compatibility response shaping。
- 加强 product service wrapper 回归保护：wrapper 必须调用对应的 GraphGateway
  高层方法，不能直接调用 `load_graph_state()`、`resume()`、low-level recorders
  或直接写 `workflow_state.json`。
- 增加 gateway 测试，确认 dependency-review approve 会把 human command 写入
  canonical graph state，并刷新 UI projection。

当前边界：

- Public route path 和 response shape 不变。
- 不移除低层 `resume()` primitive；它仍然是 graph 内部 building block 和显式
  test helper。
- 不改变 dependency planning semantics、product generation、execution、compare、
  UI layout 或 sandbox behavior。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_graph_gateway.GraphGatewayTests.test_dependency_review_endpoint_resumes_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_dependency_owns_interrupt_resume -v
python -m compileall -q src\adam_agent
```

结果：3 个 focused gateway/API tests passed；compileall passed。

### 2026-05-30 - LG2.8 Prepare Response Graph-State Metadata 切片

已完成：

- 给 `RunPlanResponse` 增加 `graph_state_path`。
- 更新 `prepare_run_plan()`，让 `/runs/prepare` response 同时暴露 canonical
  `graph_state.json` path 和 compatibility `workflow_state.json` projection
  path。
- 增加 service helper 和 FastAPI endpoint response shape 的回归测试。

当前边界：

- 这是 compatibility/read-model observability 改动。
- 不改变 dependency planning semantics、interrupts、generation、execution、
  compare、UI layout 或 sandbox behavior。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_prepare_run_plan_uses_graph_projection tests.test_graph_gateway.GraphGatewayTests.test_graph_state_endpoint_returns_canonical_state -v
```

### 2026-05-30 - LG2.8 GraphGateway-Owned Compare Entry Point 切片

已完成：

- 新增 `src/adam_agent/tools/compare.py`，作为确定性 compare tool boundary，
  负责：
  - generated output 是否可用于比较；
  - reference ADaM 查找；
  - generated-vs-reference CSV 结构/单元格比较。
- 新增 `GraphGateway.compare_reference_output()`，作为有状态 compare 的
  graph-owned 高层入口。
- compare endpoint 对已经 prepare 过的 graph run，现在委托
  `GraphGateway.compare_reference_output()`，不再由 service 层先计算 compare 再
  调低层 recorder。
- 保留无 `graph_state.json` 的旧兼容行为：endpoint 仍可返回一次临时 compare
  response，但不会创建 graph state，也不会写 compare report。
- 增加回归覆盖：
  - stateful compare 必须委托 gateway；
  - gateway compare 会计算并记录 canonical compare state；
  - 无 graph state 的 compare 仍然是 transient。

当前边界：

- 本切片不改变 compare 算法。它仍只是初始 CSV 结构和抽样单元格比较，不是临床
  推导验证。
- `review-summary` 仍是 read-model endpoint，可以重新计算一次不写入状态的
  compare preview，用来显示 reference 文件消失这类当前事实，而不改写历史
  graph state。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_reports_compare_without_mutating_graph_when_reference_disappears -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_reference_output_computes_and_records_compare tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -m compileall -q src\adam_agent
```

结果：4 个 focused API tests passed；4 个 focused gateway tests passed；
compileall passed。

### 2026-05-30 - LG2.8 Upload Invalidation Gateway Ownership 切片

已完成：

- 新增 `GraphGateway.mark_study_inputs_changed()`，作为上传文件后输入证据变化
  失效处理的唯一高层入口。
- 简化 `api/service.py::save_uploaded_file_bytes()`：service 只负责保存文件、
  重新扫描输入，并把 run invalidation 全部委托给 gateway。
- 保留 upload response 的公开字段：
  - `touched_runs` 继续作为 compatibility projection/read-model 字段；
  - `touched_graph_runs` 继续作为 canonical graph-state invalidation 信号；
  - `skipped_graph_runs` 报告无法加载的 canonical graph run。
- 增加边界测试，证明 upload service helper 调用
  `GraphGateway.mark_study_inputs_changed()`，不再直接调用
  `invalidate_active_workflows()` 或 `mark_all_inputs_changed()`。
- 增加 gateway 测试，证明高层 upload invalidation 入口能同时返回旧兼容触达
  列表和 graph 触达列表，并保持 projection consistency。

当前边界：

- `invalidate_active_workflows()` 仍作为 compatibility projection helper 保留，
  但 FastAPI service upload helper 不再直接调用它。Product service code 应使用
  `GraphGateway.mark_study_inputs_changed()`。
- 旧字段名 `touched_runs` 只为 API/UI 兼容保留。新的 graph-aware 行为应以
  `touched_graph_runs` 为准。
- 本切片不改变 dependency planning semantics、generation、execution、compare、
  UI layout、route paths 或 sandbox behavior。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。Static rules
  仍然只能是 generic contract checks 或 governed rule-pack items。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_scans_canonical_graph_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_preserves_existing_stale_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_all_inputs_changed_reports_corrupt_graph_state_as_skipped tests.test_graph_gateway.GraphGatewayTests.test_gateway_mark_study_inputs_changed_returns_legacy_and_graph_touches tests.test_api_phase8.Phase8ApiTests.test_upload_endpoint_delegates_input_invalidation_to_gateway tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_workflow_state_stale tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation tests.test_api_phase8.Phase8ApiTests.test_upload_invalidates_graph_run_even_when_workflow_projection_is_missing tests.test_api_phase8.Phase8ApiTests.test_upload_reports_corrupt_graph_state_as_skipped -v
```

结果：9 个 focused tests passed。

### 2026-05-30 - LG2.8 Legacy `/runs` Gateway Ownership 切片

已完成：

- 为剩余的旧 run-to-completion surface 新增 graph-gateway 入口：
  - `GraphGateway.block_legacy_run_to_completion()`：为被拒绝的 LLM `/runs`
    调用写入 blocked compatibility projection，强制走 split-flow review gates。
  - `GraphGateway.run_legacy_to_completion()`：运行旧 stub/test compatibility
    graph path，并写入 legacy workflow projection。
- 从 `api/service.py` 移除剩余的直接 `workflow_state.json` 写入 helper。
- 从 `api/service.py::run_study_from_request()` 移除直接
  `compile_study_graph()` 调用。
- 加强 API 边界测试：service helpers 不能直接调用 `update_workflow_state()`，
  旧 `/runs` 状态必须委托给 gateway methods。
- 增加 gateway 测试，覆盖 LLM run-to-completion 被拒绝和旧 stub
  run-to-completion projection。

当前边界：

- `POST /runs` 仍是 legacy compatibility route，不是产品 LLM generation path。
- 当前产品路径仍然是 `/runs/prepare` 加 per-dataset
  finalize/draft-spec/code-review/execute gates。
- 旧 `/runs` response shape 保持不变，包括
  `workflow_control: legacy_run_to_completion_compatibility_shim` 和
  `graph_state_path: null`。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。

Focused verification：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_blocks_legacy_llm_run_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_runs_legacy_stub_to_completion_with_projection tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_run_study_from_request_delegates_legacy_run_state_to_gateway tests.test_api_phase8.Phase8ApiTests.test_legacy_run_workflow_helpers_are_removed_from_service_layer tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_demo_study_rejects_run_to_completion_llm_endpoint -v
```

结果：8 个 focused tests passed。

### 2026-05-30 - LG2.8 Graph-Owned Progress Read Model 切片

已完成：

- 新增 `GraphGateway.progress_summary()`，作为 run progress 和 next action
  的 graph-owned read model。
- 新增 `GET /runs/{run_id}/progress`，让 UI 直接询问 gateway：当前 graph
  state 到底意味着什么，而不是在浏览器端自己从 raw JSON 拼流程逻辑。
- 新增 response contracts：
  - `RunProgressResponse`
  - `DatasetProgressItem`
- progress payload 会返回：
  - study-level status 和 current interrupt；
  - graph-owned next action；
  - per-dataset status、current interrupt、spec/code/execution/validation/
    compare status；
  - dataset 自己的下一步动作是否被 study-level dependency gate 暂时阻塞。
- 增加测试证明：
  - next-action read model 归 gateway 所有；
  - FastAPI endpoint 能暴露该 read model；
  - service helper 只委托 `GraphGateway.progress_summary()`，不直接调用
    `load_graph_state()` 读取内部状态。
  - stale dependency plan 会显示为 `replan_dependencies`，而不是被普通
    dependency review 文案掩盖。
  - progress blocking 与真实 product dependency gate 对
    review-required dependency evidence 的阻塞逻辑保持一致。

当前边界：

- 这是 read-model 切片，不改变 dependency planning、draft-spec generation、
  code generation、code review、execution、compare、route semantics 或 UI
  layout。
- endpoint 仍通过 `GraphGateway` 读取当前 canonical `graph_state.json`。这
  是向 graph-owned UI orchestration 迈进的一步，不是 native LangGraph
  interrupt/checkpointer replacement。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。Static rules
  仍然只能是 generic contract checks 或 governed rule-pack items。
- static-rule 设计原则继续收紧为“从原则出发、可复用”：未来任何 blocking
  ADaM/CDISC/company check 都必须表达成 generic contract，或者通过带版本、
  source、scope、severity、evidence 的 rule pack admission 进入，不能把 demo
  失败现象直接写成补丁规则。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions -v
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
git diff --check -- src\adam_agent\api\models.py src\adam_agent\api\service.py src\adam_agent\api\app.py src\adam_agent\graph\gateway.py tests\test_graph_gateway.py tests\test_api_phase8.py
```

结果：focused tests passed；compileall passed；168 个相关 gateway/API/static-rule
tests passed；diff check passed。

子 agent review：

- 初次 review 返回 NO-GO，指出两个 progress-read-model 问题：
  - stale input fingerprints 可能被展示成普通 dependency review，而不是
    replan action；
  - 来自真实 evidence source 的 `review_required` dependency decisions 可能
    在 progress 中显示为未阻塞，但 product methods 实际会拒绝继续。
- 两个问题均已修复，并增加 regression tests。
- 最终相关验证通过 168 个 gateway/API/static-rule tests。

### 2026-05-30 - LG2.8 UI Progress Read Model 接入切片

已完成：

- 本地 UI 新增 `state.runProgress`，通过 `GET /runs/{run_id}/progress`
  读取 graph-owned progress read model。
- Study progress panel、top graph status、dataset cards、action hints 和
  human review queue 现在优先使用 `/progress` 返回的 next action、
  blocked reason、per-dataset spec/code/execution 状态。
- raw `graphState` 仍作为 audit/debug 和兼容 fallback 保留，但 UI 不再把
  “下一步是什么”主要交给浏览器端自行拼接。
- 上传文件、prepare plan、finalize inputs、draft-spec review、code generation、
  code review、execution、review summary refresh 后都会刷新 graph read models。
- 增加前端契约测试，确认 UI 暴露 `/progress` 调用、保存 `runProgress`，并在
  progress panel/review queue 中优先使用 graph-owned read model。

当前边界：

- 这是 UI 接线切片，不改变 dependency planning、LLM generation、R execution、
  compare、route semantics 或页面总体布局。
- static-rule governance 不变。本切片不新增任何 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific static rule。
- 根据用户审查意见，后续 static-rule 工作继续按通用规则边界推进：static
  engine 只能执行 artifact/execution/spec declared-contract checks，或执行已通过
  authority/source/version/scope/severity/evidence 准入的 rule-pack item。任何从
  demo、PSY201、单个 legacy 程序、单个 ADaM 数据集或某个变量观察到的问题，
  不能直接写成 blocking static rule；必须先抽象为通用 contract，或进入受治理
  rule-pack/backlog/reviewer-note。

验证：

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

结果：focused UI/API tests passed；完整相关 gateway/API/static-rule regression
通过 168 个 tests。

子 agent review：

- 初次 review 返回 NO-GO：UI 已经从 `/progress` 展示 graph-owned blocked
  reasons，但主操作按钮仍可能保持可点击，因为 `setButtonAvailability()` 只写
  提示和 `data-action-ready`。
- 已修复：`setButtonAvailability()` 现在直接按同一份 `actionAvailability()`
  contract 禁用按钮，并在 finalize inputs、generate code、approve/run 三个前门
  入口增加 guard。graph progress 已阻塞时，UI 会在前端产品动作入口拦住，而不是
  等后端拒绝后才暴露问题。
- 复审继续发现 approve/run 仍漏掉同一个 `progressBlocked` 条件。现已修正：
  `approveRun.ready` 也要求 `!progressBlocked`，并优先展示 graph-owned blocked
  reason。
- 最终 review 返回 GO。

### 2026-05-30 - LG2.8 Legacy Stub 显式启用切片

已完成：

- `DatasetGraph` 在 `execution_mode` 缺失或未知时不再落入 legacy fake stub
  chain。
- `StudyGraph` 不再给 dataset task 自动补 `execution_mode="stub"`。只有明确
  想跑 legacy/test stub 的调用方，才需要显式传入 `execution_mode="stub"`。
- `graph_product_execute` 仍然是合法产品模式：prepare 阶段只初始化 execution
  state，真正 R execution 仍由专门的 `execute_approved_code` node 负责。
- 原本用于验证 legacy stub 的 smoke tests 现在都显式声明 stub mode。新增回归
  测试证明缺失 execution mode 会 fail closed，而不是生成 completed stub dataset。

当前边界：

- 本切片不删除 legacy stub nodes；只是把它们收紧成显式 compatibility/test
  behavior。Product graph modes 和 LLM downstream modes 不变。
- static-rule governance 不变，并继续按“从原则出发”的规则边界推进。本切片
  不新增任何 static rule。后续 static check 只能以 generic
  artifact/execution/spec contract evaluator 的形式进入，或者作为带
  authority/source/version/scope/severity/evidence 的 versioned rule-pack item
  进入；demo、study、dataset 或单个 variable 观察不能直接升级成 blocking
  rule。

验证：

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_smoke -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_runs_legacy_stub_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_blocks_legacy_llm_run_to_completion_with_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
```

结果：compileall passed；57 个 graph smoke tests passed；focused gateway
compatibility/product tests passed。

### 2026-05-30 - LG2.8 Product DatasetGraph / Legacy StubGraph 分离切片

已完成：

- `compile_dataset_graph()` 现在只编译产品 DatasetGraph，不再包含旧的 synthetic
  `*_stub` nodes。默认不使用自己的持久 checkpointer；后续 native interrupt
  试点可以为内部测试显式传入 checkpointer。
- 新增 `compile_legacy_stub_dataset_graph()`，只有这个 legacy/test compiler 会包含
  stub chain。
- `execution_mode="stub"` 在产品图里会 fail closed；即使调用方试图传入内部
  stub 标记，也不能让产品图进入 stub chain。Stub 执行只能通过显式 legacy/test
  graph compiler 进入。
- `StudyGraph` 只有在显式 `execution_mode="stub"` 的 compatibility/test run 中
  才分发到 legacy stub graph。Product modes 和 LLM downstream modes 仍走产品
  DatasetGraph。
- Graph smoke tests 现在证明：
  - 产品图拓扑不包含 legacy stub nodes；
  - legacy stub graph 才包含显式 stub chain；
  - 直接对产品图发起 `stub` 请求会 fail closed；
  - 现有显式 legacy stub compatibility 行为仍可用。

当前边界：

- 本切片仍保留 legacy stub node functions，用于 compatibility tests 和旧 `/runs`
  stub 行为；它做的是把 stub 从默认产品图拓扑中拆出去，而不是立刻删除所有
  历史测试脚手架。
- static-rule governance 不变。本切片不新增任何 static rule，也不新增
  clinical、dataset-specific、study-specific、demo-specific 或 variable-specific
  check。

验证：

```text
python -m compileall -q src\adam_agent
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_state_isolation_across_stub_runs tests.test_graph_smoke.GraphSmokeTests.test_product_dataset_graph_rejects_stub_mode_without_legacy_compiler tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_dataset_graph_contains_only_explicit_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

结果：focused graph split tests passed；compileall passed；228 个相关
graph/gateway/API/static-rule tests passed。

### 2026-05-31 - LG2.8 API/CLI 入口禁止隐式 Stub 切片

已完成：

- 移除 `POST /runs` 入口残留的隐式 fallback：当 config 使用 mock provider 且
  请求没有传 `execution_mode` 时，系统不再偷偷选择 `execution_mode="stub"`。
- 移除 CLI `adam-agent run-study` 中同样的 fallback：mock provider 的 CLI run
  如果没有显式传 `--execution-mode`，现在会 fail closed。
- 保留显式 legacy 兼容行为：
  - `POST /runs` 显式传 `execution_mode="stub"` 仍然可以跑 legacy
    compatibility/test path；
  - mock config 显式传 `--execution-mode llm_downstream_provider` 仍然保留现有
    configured provider boundary 测试路径；
  - 非 mock 的旧 `/runs` 请求如果没有传 `execution_mode`，仍会解析为 LLM
    run-to-completion，然后被 split-flow gate 拦截，避免绕过 review gates。
- 新增 API 和 CLI 回归测试，证明 omitted execution mode 不会再生成 completed
  legacy stub run。

当前边界：

- 本切片不改变 product split-flow endpoints、dependency planning、DatasetGraph
  产品拓扑或 LLM/R sandbox 行为。
- static-rule governance 不变。本切片不新增任何 static rule，也不新增
  clinical、dataset-specific、study-specific、demo-specific 或 variable-specific
  check。后续 static check 仍只能作为 generic artifact/execution/spec
  contract，或作为带 authority/source/version/scope/severity/evidence 的
  governed rule-pack item 进入。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_requires_explicit_execution_mode_instead_of_implicit_stub tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_requires_explicit_execution_mode_with_mock_config tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_uses_configured_provider_boundary_with_mock tests.test_graph_smoke.GraphSmokeTests.test_study_graph_missing_execution_mode_fails_closed_not_completed_stub -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; count=0; ...; print(f'syntax ok: {count} files')"
git diff --check -- src\adam_agent\api\service.py src\adam_agent\cli.py tests\test_api_phase8.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused API 和 CLI entry tests passed；230 个相关
API/graph/gateway/static-rule tests passed；AST syntax check 覆盖 61 个
Python 文件；diff check passed。`python -m compileall` 仍被本地 Windows
pycache 权限问题阻塞（`PermissionError` / `WinError 5`），不是语法失败。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：API/CLI implicit stub fallback 已移除；显式 legacy stub
  compatibility 仍保留；显式 LLM run-to-completion 仍会被 split-flow gate
  拦截；DatasetGraph/legacy stub graph 分离没有被削弱；static-rule
  governance 仍然只限 generic contract/rule-pack。

### 2026-05-31 - LG2.8 Execution-Mode 入口契约切片

已完成：

- 新增共享 execution-mode contract 模块，用于入口层 allowlist。
- `POST /runs` 现在会在调用 legacy graph path 之前拒绝未知
  `execution_mode`。这个旧 endpoint 的 allowlist 被刻意收窄为：
  `stub`、`llm_downstream_provider`、`llm_downstream_r_sandbox`。
- `adam-agent run-study` 现在也会在 compile/invoke `StudyGraph` 之前拒绝未知
  `--execution-mode`。CLI allowlist 保留显式 developer modes，但拒绝任意字符串。
- 更新 Phase 8.1 API contract，避免文档暗示 legacy run endpoint 接受任意 mode。
- 新增 API 和 CLI 回归测试，证明 unknown execution mode 不会创建 run 目录或
  workflow projection。

当前边界：

- 本切片不改变 DatasetGraph routing、product split-flow endpoints、dependency
  planning、provider behavior、R execution 或 UI state。
- `llm_downstream_provider` 和 `llm_downstream_r_sandbox` 仍被 `POST /runs`
  接受，只是为了写入 `split_flow_required` 并返回明确阻断；它们不是产品级
  run-to-completion path。
- static-rule governance 不变。本切片不新增 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific rule。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_unknown_execution_mode_before_legacy_graph tests.test_api_phase8.Phase8ApiTests.test_create_run_requires_explicit_execution_mode_instead_of_implicit_stub tests.test_api_phase8.Phase8ApiTests.test_create_run_stub_is_marked_legacy_compatibility_shim tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_rejects_unknown_execution_mode_before_graph tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_requires_explicit_execution_mode_with_mock_config tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_uses_configured_provider_boundary_with_mock tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_can_execute_llm_downstream_r_sandbox -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\api\service.py src\adam_agent\cli.py tests\test_api_phase8.py tests\test_graph_smoke.py docs\phase8_1_api_contract.md docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused API 和 CLI entry tests passed；232 个相关
API/graph/gateway/static-rule tests passed；diff check passed。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：未知 API/CLI mode 会在 graph invocation 前被拒绝；省略 mode
  仍然 fail-closed；显式 legacy `stub` compatibility 仍保留；显式 LLM
  `/runs` 请求仍会被 split-flow gate 拦截；CLI developer modes 仍可用；
  Product Graph / legacy stub 边界没有被削弱；static-rule governance 未被改动。

### 2026-05-31 - LG2.8 Graph Execution-Mode 常量边界切片

已完成：

- 将 `src/adam_agent/graph/execution_modes.py` 从 API/CLI 入口 allowlist
  扩展为 graph execution-mode 名称的共享来源。
- DatasetGraph、routing helpers、StudyGraph dispatch、GraphGateway product
  invocation、demo-study 默认 mode 中的 product/downstream/stub mode 分支判断，
  改为引用共享常量。
- 更新 non-legacy route guard 回归测试，让 downstream mode cases 来自共享 mode
  set，而不是测试里自己复制一份字符串清单。

当前边界：

- 这是行为保持切片。不改变 product routing、dependency planning、LLM
  generation、R execution、compare、UI state 或 static rule semantics。
- 剩下的 `"stub"`、`"graph_product_execute"` 等字符串是审计 metadata key，
  不是 execution-mode routing decision。
- static-rule governance 不变。本切片不新增 clinical、dataset-specific、
  study-specific、demo-specific 或 variable-specific rule。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_non_legacy_modes_never_route_to_stub_chain tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_unknown_execution_mode_fails_closed_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_graph_product_execute_prepare_does_not_run_r_before_execute_node tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_demo_study_endpoint_prepares_shiny_demo_shape tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_unknown_execution_mode_before_legacy_graph tests.test_graph_smoke.GraphSmokeTests.test_cli_run_study_rejects_unknown_execution_mode_before_graph -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\graph\dataset_graph.py src\adam_agent\graph\routing.py src\adam_agent\graph\study_graph.py src\adam_agent\graph\gateway.py src\adam_agent\api\service.py tests\test_graph_smoke.py
```

结果：focused graph/API checks passed；232 个相关
API/graph/gateway/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：常量集合覆盖了之前的 mode 字符串；DatasetGraph 行为仍然等价且
  fail-closed；Product Graph / legacy stub graph 分离没有被削弱；剩余 literal
  是测试样例或 audit metadata label，不是 routing decision；static-rule
  governance 未被触碰。

### 2026-05-31 - LG2.8 StudyGraph Execution-Mode 预检切片

已完成：

- 在 StudyGraph 层增加显式 unsupported execution mode 预检，在 runnable
  dataset tasks 下发之前拦截。
- 保持 dependency planning 和 plan-only gateway 行为不变。缺失 execution mode
  仍由已有 lower-level fail-closed 路径处理，或用于只规划不执行的测试路径。
- 新增 smoke regression，证明显式 unknown StudyGraph mode 不会调用 dataset
  subgraph。

当前边界：

- 本切片不改变 API entry validation、dependency semantics、product split-flow
  endpoints、LLM generation、R execution、compare、UI state 或 static-rule
  governance。
- Unsupported datasets 和 unresolved dependencies 保留原有业务失败原因，不会被
  execution-mode preflight 覆盖。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_unknown_execution_mode_fails_before_dataset_dispatch tests.test_graph_smoke.GraphSmokeTests.test_study_graph_missing_execution_mode_fails_closed_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_non_ad_target_is_blocked_as_unsupported_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_reference_sas7bdat_dependency_artifact_does_not_satisfy_runtime_dependency tests.test_graph_smoke.GraphSmokeTests.test_run_output_dependency_artifact_wins_over_reference_adam tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions tests.test_graph_smoke.GraphSmokeTests.test_study_graph_writes_dependency_plan_review_artifacts tests.test_graph_smoke.GraphSmokeTests.test_dependency_review_artifacts_include_conflict_warning tests.test_graph_smoke.GraphSmokeTests.test_terminal_failure_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_smoke tests.test_graph_gateway tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\execution_modes.py src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py
```

结果：focused StudyGraph preflight 和 dependency-regression tests passed；
233 个相关 API/graph/gateway/static-rule tests passed；AST syntax check 覆盖
79 个 Python 文件；diff check passed。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：StudyGraph mode set 覆盖当前允许的 study/product entries，但没有把
  retired ADSL template mode 重新放回合法入口；preflight 条件足够窄，保留
  plan-only、unsupported 和 dependency-blocked 结果；显式 unknown mode 不会
  dispatch dataset subgraphs；static-rule governance 未被触碰。

### 2026-05-31 - LG2.8 StudyGraph Audit Node 命名清理切片

已完成：

- 将 StudyGraph 最终 audit 节点从 `write_audit_manifest_stub` 改名为
  `write_audit_manifest`。
- 保持 audit manifest 行为不变。这个节点已经会在有 study directory 时写真实
  study-level audit manifest，并在 manifest metadata 中记录 `stub: false`。
- 新增拓扑回归测试，证明产品 StudyGraph 不再暴露任何 `*_stub` node name，并且
  `reduce_dataset_results` 仍然流向真实 audit manifest 节点。

当前边界：

- 这是 compatibility/deprecation cleanup，不改变 dependency planning、dataset
  dispatch、LLM generation、R execution、compare、UI state 或 static-rule
  governance。
- 本切片有意重命名一个 StudyGraph node id。历史 checkpoint 如果正好停在旧的
  final audit node，不属于这个 cleanup branch 的兼容承诺范围。
- 历史文档仍可能提到 Phase 3 旧 stub node name；当前产品 StudyGraph topology
  不能再暴露这些名字。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_uses_real_audit_manifest_node_name tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused StudyGraph topology 和 smoke tests passed；234 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。也尝试过 `python -m compileall -q
src\adam_agent`，但当前 workspace 的既有 `__pycache__` 路径写入被
`PermissionError` 阻止，因此使用不写 pyc 的 AST parsing 作为语法检查。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：本切片只是围绕同一套 audit-manifest 逻辑重命名 node/function；
  产品 StudyGraph topology 不再暴露 `*_stub` node name；DatasetGraph legacy
  stub 覆盖没有被影响；static-rule governance 未被触碰。

### 2026-05-31 - LG2.8 通用 Dependency Status Label 切片

已完成：

- 将 StudyGraph task label `depends_on_adsl` 替换为通用
  `depends_on_upstream_adam`。
- 从 `_dependency_status()` 移除 ADSL-specific 分支。任何带上游 ADaM 依赖的
  dataset 现在都会得到同一个通用状态标签。
- 新增 smoke regression，证明 ADSL dependency 和 ADLB dependency 都会产生
  `depends_on_upstream_adam`，不会再产生 ADSL special case。

当前边界：

- 这是 task-label cleanup，不改变 dependency planning、execution batches、
  dependency approval semantics、DatasetGraph routing、LLM generation、R
  execution、compare、UI state 或 static-rule governance。
- 当 evidence 显示目标依赖 ADSL 时，`ADSL` 仍是一个普通 ADaM dataset；系统不会
  把它当作 product template path。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_dependency_status_is_generic_for_upstream_adam tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "depends_on_adsl|depends_on_adam" src\adam_agent tests --glob "*.py"
```

结果：focused dependency-status regression passed；235 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。Source scan 未发现 retired
`depends_on_adsl` / `depends_on_adam` label 的运行时代码使用；唯一剩余 Python
命中是回归断言，确认旧的 ADSL-specific label 不存在。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：dependency scheduling 仍由 `dataset_dependencies` 和
  `execution_batches` 控制，而不是由 task label 控制；新测试在 ThreadPool
  execution 下不依赖完成顺序；DatasetGraph legacy stub behavior 和 static-rule
  governance 未被触碰。

### 2026-05-31 - LG2.8 Legacy Stub 沙盒失败场景命名清理切片

已完成：

- 新增 dataset-neutral 的 legacy stub 场景名 `sandbox_failure`。
- 保留旧字符串 `fail_adsl`，但只作为既有 stub 测试或历史 harness 的兼容别名。
- 将当前 graph smoke tests 中用于模拟 ADSL、ADAE、ADLB 沙盒失败的场景名改为
  `sandbox_failure`。
- 新增 focused regression，证明通用场景名可用于多个 ADaM dataset，并证明旧
  `fail_adsl` 别名仍会以同样受控的方式失败。

当前边界：

- 这是 legacy/test stub 命名清理，不改变产品 DatasetGraph routing、StudyGraph
  dependency semantics、LLM generation、R execution、compare、UI state 或
  static-rule governance。
- 产品路径不能再把 `fail_adsl` 当作业务语言使用。保留这个别名只是为了让旧
  stub harness 兼容失败，而不是因为一个命名变化触发无关 input error。
- 未新增任何 dataset-specific clinical rule 或 static-rule 行为。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_sandbox_failure_scenario_is_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_legacy_fail_adsl_stub_scenario_remains_alias tests.test_graph_smoke.GraphSmokeTests.test_adsl_failure_blocks_downstream_without_running_it tests.test_graph_smoke.GraphSmokeTests.test_downstream_stub_failure_does_not_change_completed_adsl_status tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\state.py src\adam_agent\graph\dataset_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "fail_adsl|sandbox_failure" src\adam_agent tests docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused legacy stub 命名回归通过；237 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。Source scan 显示 `fail_adsl` 只保留在显式
legacy alias 路径、alias regression 和文档说明中；当前失败场景测试使用
dataset-neutral 的 `sandbox_failure` 名称。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：`fail_adsl` 只保留为显式 legacy stub alias 和类型 literal；
  dataset-neutral 的 `sandbox_failure` 路径已覆盖多个 dataset；旧 alias 有单独
  回归；没有引入 dependency-planning 或 static-rule semantic drift。

### 2026-05-31 - LG2.8 Study Audit Virtual Artifact 命名切片

已完成：

- 移除 StudyGraph 在无 `study_dir` 时返回的残留 `*_study_stub` audit manifest
  artifact id。
- 改为 `*_study_virtual`，因为这个 artifact 是尚未落盘的 graph-state 引用，
  不是 fake/stub study product。
- 保持 study audit manifest 的 `metadata["stub"] = False`，并新增
  `manifest_materialized`，让调用方能区分真实写入磁盘的 manifest 和 virtual
  reference，而不用再借用 stub 语言。
- 将无 `study_dir` 时 audit-agent summary 的 metadata 从 `stub: true` 改为
  `materialized: false`。
- 新增 graph smoke regression，证明 virtual study audit refs 不再带 stub
  metadata。

当前边界：

- 这是 audit metadata 和命名清理，不改变 StudyGraph planning、dataset
  dispatch、DatasetGraph product routing、legacy stub graph behavior、LLM
  generation、R execution、compare、UI state 或 static-rule governance。
- Dataset-level legacy stub audit artifacts 仍然只在
  `compile_legacy_stub_dataset_graph()` 的显式 legacy stub 图中保留 stub 标签；
  本切片只移除 study-level fallback 上误导性的 stub 语言。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_virtual_audit_refs_are_not_stub_metadata tests.test_graph_smoke.GraphSmokeTests.test_study_graph_uses_real_audit_manifest_node_name tests.test_graph_smoke.GraphSmokeTests.test_study_graph_runs_foundation_then_downstream_stub_datasets tests.test_graph_smoke.GraphSmokeTests.test_study_graph_writes_dependency_plan_review_artifacts -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[...]; ...; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "study_stub|study_virtual|manifest_materialized|materialized: false|materialized\": false" src\adam_agent tests docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused StudyGraph audit metadata 回归通过；238 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。Source scan 未发现运行时代码继续使用
`study_stub` artifact id；剩余 `study_stub` 命中是文档说明和回归断言，确认旧
后缀不存在。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：无 `study_dir` 的 StudyGraph manifest 现在使用 virtual artifact id
  和 materialization metadata；audit-agent summary 不再给 virtual reference 写
  stub metadata；legacy DatasetGraph stub audit metadata 仍保留；没有引入
  static-rule 或 product-routing drift。

### 2026-05-31 - LG2.8 Dataset-Neutral Stub Scenario Defaults 切片

已完成：

- 移除 StudyGraph 中最后一个针对 `ADAE` 的默认 stub 场景特判。
- StudyGraph 现在给每个 runnable dataset 同一个 legacy stub 默认场景：
  `success`。
- 测试需要模拟 repair/failure 时，必须显式传入 `stub_scenarios`。
- 新增回归，证明 ADAE 和 ADLB 在默认 stub 场景下都不会隐式产生 repair
  attempt；同时证明显式 `code_error_then_success` 仍可触发 legacy repair 路径。

当前边界：

- 这是 legacy/test stub 默认值清理，不改变 dependency planning、product
  DatasetGraph routing、LLM generation、R execution、compare、UI state 或
  static-rule governance。
- 显式 DatasetGraph `code_error_then_success` 场景仍保留，用于测试 repair
  routing；它不再隐藏成 StudyGraph 对 ADAE 的默认行为。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_default_stub_scenarios_are_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_study_graph_stub_repair_requires_explicit_scenario -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "code_error_then_success\" if dataset == \"ADAE\"|dataset == \"ADAE\"" src\adam_agent\graph tests\test_graph_smoke.py
```

结果：focused default-scenario 回归通过；240 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。Source scan 未发现 graph runtime 代码仍保留
`ADAE` special-case default stub repair scenario。

子 agent review：

- 只读 review 返回 GO。
- 审核确认：StudyGraph 现在对 downstream 和 foundation tasks 使用同一个默认
  stub scenario；显式 repair simulation 仍可通过 `stub_scenarios` 使用；新增测试
  覆盖了默认路径和显式 repair 路径；没有引入 dependency-planning、
  product-routing 或 static-rule governance drift。

### 2026-05-31 - LG2.8 StudyGraph Non-Execution Compare Status 切片

已完成：

- 将 StudyGraph 级别的 non-execution failures 从 `compare_status:
  not_run_stub` 改为 `compare_status: not_run`。
- 覆盖 unsupported targets、unresolved/blocked dependency results、
  downstream blocked-by-dependency summaries，以及 StudyGraph execution-mode
  preflight failures。
- 新增回归断言，证明这些 StudyGraph 路径不再报告 stub compare status。

当前边界：

- 这是 status-label cleanup，不改变 dependency planning、dataset dispatch、
  DatasetGraph product routing、legacy stub graph behavior、LLM generation、R
  execution、compare implementation、UI state 或 static-rule governance。
- DatasetGraph legacy stub summaries 仍在显式 legacy/test compiler path 内使用
  `not_run_stub` 和 `passed_stub`。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_state_isolation_across_stub_runs tests.test_graph_smoke.GraphSmokeTests.test_legacy_stub_sandbox_failure_scenario_is_dataset_neutral tests.test_graph_smoke.GraphSmokeTests.test_reference_sas7bdat_dependency_artifact_does_not_satisfy_runtime_dependency tests.test_graph_smoke.GraphSmokeTests.test_non_ad_target_is_blocked_as_unsupported_not_completed_stub tests.test_graph_smoke.GraphSmokeTests.test_study_graph_unknown_execution_mode_fails_before_dataset_dispatch tests.test_graph_smoke.GraphSmokeTests.test_midstream_dependency_failure_blocks_only_dependent_datasets -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
git diff --check -- src\adam_agent\graph\study_graph.py tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
rg -n "compare_status=\"not_run_stub\"|not_run_stub" src\adam_agent\graph tests\test_graph_smoke.py docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md
```

结果：focused StudyGraph non-execution compare-status 回归通过，包括 direct
dependency-preflight blocking 和显式 legacy stub 边界；240 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。Source scan 显示 `not_run_stub` 只剩在
DatasetGraph legacy stub runtime code、legacy test fixtures 和本文档说明中。

子 agent review：

- GO。未发现重大业务或架构回归。review 确认 DatasetGraph legacy stub 行为未
  改变，StudyGraph 改动只是 non-execution/no-output 情况的状态标签清理，真实
  execution/compare failure 仍由 DatasetGraph/Product/Gateway 路径负责。

### 2026-05-31 - LG2.8 Stub Dependency Artifact Guard 切片

已完成：

- 收紧 run-output dependency availability：graph state 标记为
  `completed_stub` 的上游输出，不能静默满足下游 ADaM runtime dependency。
- guard 同时拦截 graph execution state 中的 `completed_stub`、
  `structural_stub_pass` 或 `stubbed_r_execution: true`。
- 新增回归测试，证明上游 `ADSL` 如果只是 structural stub graph state，下游
  `ADAE` 会保持阻塞，dependency resolution 为 `found_but_unusable`。
- 正常 completed run output 仍可用，前提是 graph state 记录了真实 output
  artifact，且 execution state 不是 terminal/failure/stub。

当前边界：

- 这是 dependency-quality signal guard，不改变 mock code generation、legacy
  stub graph execution、product DatasetGraph routing、UI state、compare、R
  execution 或 static-rule governance。
- 本地 mock/demo flow 仍可产生 `completed_stub` 输出用于 UI smoke testing；
  这些输出仍可见，但不再解锁下游 runtime dependency。
- 本切片不拦截所有 `not_real_derivation` 结果。mock provider 加 real R
  execution 是否能作为依赖，是另一个需要单独决定的 policy 问题。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_run_output_dependency_artifact_wins_over_reference_adam tests.test_graph_smoke.GraphSmokeTests.test_unbacked_run_output_dependency_is_not_usable tests.test_graph_smoke.GraphSmokeTests.test_terminal_failure_run_output_dependency_does_not_satisfy_downstream tests.test_graph_smoke.GraphSmokeTests.test_completed_stub_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
```

结果：focused dependency-availability 回归通过；241 个相关
graph/gateway/API/static-rule tests passed；AST syntax check 覆盖 79 个
Python 文件；diff check passed。

子 agent review：

- GO。未发现 blocking findings。审核确认：guard 只作用于当前 run 的
  `run_output` dependency candidates，保留“当前 run output 优先于 reference
  ADaM”的规则，不影响 reference ADaM visibility 或 UI/demo progress paths，也
  不会拦截所有 `not_real_derivation` 情况。
- 根据 review 的非阻塞建议，本切片同时把 unusable-run-output reason 拆成更精
  确的消息，分别说明 terminal failure、缺失/损坏 graph state、output path 不
  匹配，以及 structural stub output 等原因。

### 2026-05-31 - LG2.8 Not-Real Derivation Dependency Guard 切片

已完成：

- 在 GraphGateway 拥有的 `code_state` 中加入 code-generation quality record，
  记录 provider/model 信息，并把 mock provider 生成的代码标记为
  `not_real_derivation`。
- approved-code execution 成功后，把这份质量信号带入 `execution_state`，这样
  dependency resolver 可以只根据 canonical graph state 判断依赖是否可用，而
  不需要依赖 UI 或 service 的临时约定。
- 收紧 run-output dependency availability：即使上游状态是 `completed`，只要
  output 被标记为 `not_real_derivation`，也不能静默满足下游 runtime ADaM
  dependency。
- 新增回归测试，证明 `generation_quality.not_real_derivation: true` 的 ADSL
  run output 会让 ADAE 保持 `found_but_unusable` 阻塞；同时证明 mock code
  generation 会把质量信号写入 graph state。

当前边界：

- 这是 dependency-quality guard，不代表所有 mock-assisted work 都毫无用途。
  mock 产物仍可用于 UI smoke testing 和本地审核展示，但不能解锁下游 runtime
  dependency。
- 真实 provider 输出不会被本切片自动标记为 `not_real_derivation`。它的临床质
  量仍取决于 approved spec、人工 code review、static checks、本地 R execution、
  validation，以及未来更强的规则层。
- 本质量字段出现之前创建的旧 run 不会被 retroactively 判定。若要把这些旧产
  物作为下游 runtime evidence，应先重新生成或做状态迁移。
- 本切片不改变 provider calls、R execution、UI state、compare 或 static-rule
  governance。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_generation_quality_marks_only_mock_signals_as_not_real tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_execution_preserves_generation_quality_signal tests.test_graph_smoke.GraphSmokeTests.test_not_real_derivation_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_not_real_derivation_run_output_dependency_does_not_satisfy_downstream tests.test_graph_smoke.GraphSmokeTests.test_completed_stub_run_output_dependency_does_not_satisfy_downstream -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 tests.test_static_rules -v
```

### 2026-05-31 - LG2.8 Output Quality Read Model 切片

已完成：

- 新增共享的 output-quality read model，供 UI/API 展示使用。它把生成输出标记
  为 `real_runtime_output`、`not_real_derivation`、`structural_stub`、
  `terminal_failure` 或 `not_completed`。
- 将这份质量信号接入 GraphGateway progress items 和 review-summary dataset
  reviews。
- 增加用户可读 warning：mock/offline 和 structural demo 输出仍可见、可审查，
  但会明确说明不能作为下游 runtime evidence。
- 更新浏览器 UI：结果审核区显示质量提示横幅；dataset card 在对应情况下显示
  `review only` 或 `demo output`。

当前边界：

- 这是 read-model 和 UI clarity 切片，不改变 LLM 调用、R execution、
  dependency resolution、compare 或 static-rule 行为。
- review-summary 从已有 `workflow_state.json` compatibility projection 读取质
  量细节，不直接读取或修改 canonical graph state。
- 这不是临床正确性评分，只说明这个输出是否适合作为下游 runtime evidence。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_read_model_helpers_do_not_record_compare -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_output_quality_classification_matrix tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_output_is_not_previewed_or_downloadable tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'syntax ok: {len(files)} files')"
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway tests.test_graph_smoke tests.test_static_rules -v
git diff --check
```

结果：focused progress/review-summary 和 quality-matrix tests passed；247 个
相关 API/gateway/graph/static-rule tests passed；AST syntax check 覆盖 80 个
Python 文件；diff check passed。

子 agent review：

- GO。未发现 blocking findings。
- 审核确认本切片停留在 read-model/UI 展示层，没有修改 dependency resolution、
  R execution、compare 或 canonical graph state。
- 已按最小建议修正：`dataset_output_quality()` 也读取 validation report 中的
  terminal-failure 和 partial-output-usability 标记，并用分类矩阵测试和
  terminal-failure review-summary 回归覆盖该路径。

### 2026-05-31 - LG2.8 Study Output Quality Rollup 切片

已完成：

- 在 dataset `output_quality` 之上新增 study 级 output-quality rollup read
  model。
- 通过 run progress API response 暴露 `output_quality_rollup`。
- 修改 study 级 next-action 文案：如果计划内 targets 只有 review-only/demo
  输出，不再把整个 run 描述成真实完成。
- 修改 mixed completion 文案：让真实 runtime output 和 review-only/demo
  output 在 study 级进度里明确分开。
- 修改 dataset 级 completed label：review-only/demo output 不再和 real runtime
  output 使用同一套“输出已可用”的文案。
- 更新浏览器进度 header：当 rollup 显示 review-only complete 或 mixed
  completion 时，顶部状态显示 `review only` 或 `mixed output`。

当前边界：

- 这仍然是 read-model 和 UI clarity 切片，不修改 canonical graph state、
  dependency planning、dependency resolution、LLM generation、R execution、
  compare 或 static-rule 行为。
- 底层 dataset status 可以继续保留 `completed`，用于表示 review/demo artifact
  已经产出；rollup 负责解释它是否是真实 runtime evidence。
- 这不是临床正确性评分，只区分真实 runtime output 与“可审核但不能满足下游
  runtime dependency”的输出。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_not_real_outputs_as_review_only tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_marks_mixed_completion_quality tests.test_graph_gateway.GraphGatewayTests.test_study_output_quality_rollup_distinguishes_review_only_completion -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

结果：focused rollup/API/UI regressions 通过；更宽的 graph-gateway/API suite
通过 151 个测试；AST syntax check 覆盖 80 个 Python 文件；`git diff --check`
只有 CRLF line-ending warnings。

子 agent review：

- GO。未发现 blocking findings。
- 审核确认 output-quality rollup 是纯 read-model 逻辑；stale plan、
  dependency review、interrupt 的优先级仍高于 completed-quality 文案；API/UI
  改动降低了用户误解风险，没有扩大业务语义。

### 2026-05-31 - LG2.8 Review Summary 优先读取 Graph State 切片

已完成：

- 将 `/review-summary` 改为优先读取 canonical
  `runs/{run_id}/graph_state.json`，用于 dataset status、code state、
  execution state、validation summary、compare status、output quality 和
  warnings。
- `workflow_state.json` 只保留为兼容 fallback：只有 canonical graph state
  无法加载时才使用。
- 增加 graph-state dataset discovery：即使 manifest/workflow projection 不完整，
  review summary 也能展示 graph state 已知的 datasets。
- 收紧 graph state 中记录的 output artifact path：只允许解析到当前
  `run_dir` 内；绝对路径或 `..` 跳出当前 run 的路径会被忽略。
- 保持 reference ADaM 的边界：它仍只用于 preview/compare evidence，不决定
  derivation logic，也不决定 output-quality eligibility。

当前边界：

- 这是 read-model 对齐切片，不修改 canonical graph state、workflow
  projection、dependency plan、dependency resolution、LLM generation、
  R execution、compare report 或 static-rule 行为。
- 磁盘 validation report 仍可服务于 legacy/fallback 读取；但只要 canonical
  graph state 可用，graph validation 必须优先于可能过期的磁盘文件。
- 本切片没有删除 compatibility projection，只是停止让 `/review-summary` 把它
  当作首要来源。

验证：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

结果：更宽的 graph-gateway/API suite 通过 155 个测试；AST syntax check 覆盖
80 个 Python 文件；`git diff --check` 只有 CRLF line-ending warnings。

子 agent review：

- 第一次 review：NO-GO。它发现旧磁盘 validation report 仍可能在
  output-quality 分类中覆盖 canonical graph validation。
- 已修复：review summary 现在构造 `review_validation` 时 graph validation
  优先，并一致用于 output quality、validation status/report、warnings 和
  errors。
- 最终 review：GO。未发现 blocking findings。

### 2026-05-31 - LG2.8 Review Summary 来源元数据切片

已完成：

- 给 `RunReviewSummary` 增加明确的 read-model 来源字段：
  `read_model_source`、`graph_state_path`、`workflow_state_path`。
- run 级 `study_id` 和 `status` 也遵循和 dataset review 一样的
  graph-state-first 规则。
- 将 fallback 来源拆成三类：
  - `graph_state`：成功读取 canonical graph state。
  - `workflow_state_fallback`：graph state 不可用，但 legacy projection 可用。
  - `artifact_fallback`：只能从 outputs、validation files、manifest 等 run
    artifacts 恢复 review summary。

当前边界：

- 这只是 read model 的来源可见性切片，不修改 graph state、workflow
  projection、dependency resolution、LLM generation、R execution、compare 或
  static-rule 行为。
- 这些字段用于减少 UI/API 状态歧义，同时保留 legacy fallback。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

结果：focused source-metadata tests 通过；更宽的 graph-gateway/API suite
通过 155 个测试；AST syntax check 覆盖 80 个 Python 文件；`git diff
--check` 只有 CRLF line-ending warnings。

子 agent review：

- 第一次 review：GO，同时提出非阻塞建议：把 artifact-only fallback 与
  workflow fallback 分开。
- 已修复：现在显式返回 `artifact_fallback`，并有回归测试覆盖。
- 最终 review：GO。

### 2026-05-31 - LG2.8 Review Summary 来源 Advanced UI 切片

已完成：

- 将 review-summary 来源元数据接入浏览器 Advanced/Audit 面板：
  `review_summary_source`、`graph_state`、`workflow_state`。
- 技术路径仍不进入主工作流界面。draft-spec notice、result summary、action
  area 仍只提示用户到 Advanced settings 和 audit files 查看路径，不直接打印。
- 更新 UI 回归测试：Advanced 区必须显示 source/path metadata；draft-spec 和
  result-facing 区域仍继续隐藏直接技术路径。

当前边界：

- 这是 UI read-model 展示切片，不改变 API state transition、dependency
  planning/resolution、LLM generation、R execution、compare 或 static-rule 行为。
- 目的只是审计清晰度：用户能知道 `/review-summary` 来自 canonical graph
  state、workflow fallback 还是 artifact fallback，同时不污染正常产品流程。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_recovers_multiple_outputs_from_same_run -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

结果：focused UI/review-summary tests 通过；更宽的 graph-gateway/API suite
通过 155 个测试；AST syntax check 覆盖 80 个 Python 文件；`git diff
--check` 只有 CRLF line-ending warnings。

子 agent review：

- GO。未发现 blocking findings。
- 审核确认本切片只修改 Advanced UI 展示，技术路径没有进入主产品面板，也没有
  新增后端状态写入或 workflow 行为。

### 2026-05-31 - LG2.8 Target Planning 与 Active Detail UI 区分切片

已完成：

- 在浏览器 UI 增加 target selection summary，明确区分两件事：
  - 当前一起做 dependency planning 的 ADaM datasets。
  - 当前 review/code/result 面板正在看的单个 active dataset。
- 在 dataset execution cards 上增加 context label，说明每个 dataset 是
  `planned in this run`，还是只是 `view-only history/candidate`。
- 更新 UI contract tests，锁定 planned targets 与 active detail target 不能
  被 UI 混成一件事。

当前边界：

- 这是 UI clarity 切片，不改变 canonical graph state、dependency planning、
  dependency resolution、LLM generation、R execution、compare 或 static-rule 行为。
- 代码生成和 R 执行现在仍然是一次处理一个 active dataset；多 target 选择
  目前只影响 dependency planning 和 dashboard context。
- 没有新增任何临床规则、demo-specific rule 或 blocking static rule。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

结果：focused UI contract tests 通过；更宽的 graph-gateway/API suite 通过
155 个测试；AST syntax check 覆盖 80 个 Python 文件；`git diff --check` 只有
CRLF line-ending warnings。

### 2026-05-31 - LG2.8 Dependency Map 可读性 UI 切片

已完成：

- 将浏览器 Dependency Map 从编号步骤列表改成每个 target 一张用户可读说明卡：
  - 当前识别到哪些 SDTM source evidence。
  - graph plan 给出的 dependency decision。
  - 当前 target status 在 runtime 上意味着什么。
  - 用户下一步该做什么。
- 保留 reference ADaM 边界提醒：reference output 仍只能作为
  comparison/output-shape evidence，不能作为 derivation authority，也不能单独
  算 runtime dependency evidence。
- 更新 UI contract tests，锁住新的 dependency-map 结构，并继续防止 reference
  ADaM 被 UI 描述成 runtime dependency。

当前边界：

- 这是浏览器 UI presentation 切片，不改变 dependency planning、dependency
  resolution、canonical graph state、LLM generation、R execution、compare 或
  static-rule 行为。
- Dependency Map 仍然只展示现有 graph read model；它不新增临床规则，也不推断
  新依赖。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check
```

### 2026-05-31 - LG2.8 顶栏状态可读性 UI 切片

已完成：

- 将浏览器右上角状态区改成更清楚的 current-status read model：
  - 当前正在做什么；
  - 当前加载的 study；
  - 当前查看的 active detail target；
  - 从 graph progress/read model 读出的下一步；
  - running、done、failed 三种操作进度状态。
- 让顶栏概览在这些时机从已有状态刷新：
  - API health check 后；
  - Study Progress 渲染后；
  - target 渲染或切换后；
  - 页面初始化时。
- 增加 UI contract tests，防止后续改动把顶栏状态字段或 progress refresh hook
  弄丢。

当前边界：

- 这是浏览器 UI read-model 切片。
- 不改变 canonical graph state、dependency planning、dependency resolution、
  LLM generation、R execution、compare 或 static-rule 行为。
- 不新增任何临床规则、demo-specific rule 或 blocking validation。
- 顶栏只读取已有状态（`runProgress`、selected target、plan、input summary），
  不成为第二套 workflow state machine。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
git diff --check -- docs\langgraph_2_construction_plan.md docs\langgraph_2_construction_plan_zh.md src\adam_agent\api\web.py tests\test_api_phase8.py
```

### 2026-05-31 - LG2.4 Agent Node IO Contract 切片

已完成：

- 扩展 bounded agent contract 层，新增明确的节点输入/输出包：
  - `AgentNodeInput`
  - `AgentNodeOutput`
  - `build_agent_node_input()`
  - `build_agent_node_output()`
- 输入包明确一个 agent 节点能看到什么：agent role、graph node、study/run id、
  可选 dataset、task、声明过的 inputs、artifact ids、risk flags、
  evidence bundle id、reference query ids。
- 输出包明确一个 agent 节点能返回什么：agent role、graph node、study/run id、
  可选 dataset、status、decision、reason、声明过的 outputs、artifact ids、
  risk flags，以及匹配的 audit decisions。
- 增加校验：一个 output 不能悄悄夹带来自其他 agent、其他 node、或其他 dataset
  的 audit decision。
- 收紧 audit-decision 时间戳契约：必须是 UTC `Z` 格式；decision dataset 会统一
  归一化成大写。
- 从 `adam_agent.agents` 导出新 contract。
- 增加 focused tests，覆盖 JSON-safe input package、output package、自动生成
  匹配 audit decision、时间戳校验、dataset 归一化，以及跨 agent / 跨 node /
  跨 dataset decision 拒绝。

当前边界：

- 这是给未来 multi-agent graph nodes 准备的 contract 切片。
- 不新增自主行为、临床规则、依赖推断、LLM prompt、R execution 行为、compare
  行为或 static-rule 行为。
- 现有 graph/product 路径仍继续使用已有 `AgentDecision` 记录。后续切片可以按
  agent role 逐步把具体节点迁到这些 IO packages。
- Reference ADaM 仍然只是 compare/output-shape evidence；这里没有增加
  derivation authority。

验证：

```text
python -B -m unittest tests.test_agents_contract -v
python -B -m unittest tests.test_graph_gateway -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/agents/contracts.py'), pathlib.Path('src/adam_agent/agents/__init__.py'), pathlib.Path('tests/test_agents_contract.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Evidence Agent IO Migration 切片

已完成：

- 将现有 DatasetGraph `prepare_product_context` evidence-agent 节点迁到新的
  typed agent IO packages：
  - `AgentNodeInput`
  - `AgentNodeOutput`
- evidence-agent input 现在记录明确的 task、execution mode、
  dependency-resolution 数量、是否强制重新生成 draft spec、risk flags、
  evidence bundle id、reference query ids。
- evidence-agent output 现在包住这个节点已有的 evidence decisions：
  - `input_spec_ready`
  - `approved_draft_spec_ready`
  - `draft_spec_required`
- 这个节点的现有 `agent_decisions` 现在来自 typed evidence-agent output，
  不再单独手写一份。
- Dataset summary metadata 暴露 `agent_node_inputs` 和 `agent_node_outputs`；
  StudyGraph batch execution 会把这些 packages 带到 study audit manifest
  metadata。
- 增加 focused smoke tests，证明直接 DatasetGraph path 和 StudyGraph batch
  path 都能保留 evidence-agent IO package。

当前边界：

- 本切片只迁移 evidence/product-context 节点。
- 不改变 dependency planning、spec generation、code generation、code review、
  R execution、compare、static rules 或 repair routing。
- IO packages 是 audit/read-model 数据；不解锁 workflow gate，也不成为第二套
  state machine。
- 后续可以按角色逐步迁移 spec/code/static/execution/validation/repair 节点。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_uses_input_spec_without_stub_code tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('src/adam_agent/graph/study_graph.py'), pathlib.Path('src/adam_agent/graph/state.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Draft Spec Agent IO Migration 切片

已完成：

- 将现有 DatasetGraph `draft_spec_agent` 成功路径迁到 typed agent IO packages：
  - `AgentNodeInput`
  - `AgentNodeOutput`
- spec-agent input 现在记录明确 task、当前 spec source、准备好的 context keys、
  warning 数量、context artifact id、risk flags、evidence bundle id、reference
  query ids。
- spec-agent output 现在包住已有 `draft_spec_generated` decision，并暴露
  draft-spec path、变量数量、next action、risk flag、prompt/response/spec
  artifact ids。
- 这个节点的 `agent_decisions` 现在来自 typed `AgentNodeOutput`，避免并行手写
  两套 audit 记录。
- 增加 focused smoke assertions，证明没有 input spec 时生成 draft spec 的路径
  会把 `spec_agent` IO package 带到 DatasetGraph result 和 summary metadata。

当前边界：

- 本切片只改变成功生成 draft spec 后的 audit packaging。
- 不改变 evidence preparation、dependency planning、input-spec 权威性、
  code generation、R execution、compare、static rules、repair、UI 或 provider
  行为。
- draft spec 仍然必须人工审核；只有 graph approval gate 记录后才是 approved
  draft spec。
- Reference ADaM 仍然只是 compare/output-shape evidence，不能作为 derivation
  authority。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Code And Static Review Agent IO Migration 切片

已完成：

- 将 DatasetGraph `generate_r_code_agent` 成功路径迁到两个 bounded role 的
  typed IO packages：
  - `code_agent`
  - `static_review_agent`
- code-agent input 现在记录 approved spec source、准备好的 context keys、
  included datasets、变量数量摘要、product-context artifact id、risk flags、
  evidence bundle id、reference query ids。
- code-agent output 现在包住已有 `r_code_generated` audit decision，并记录生成
  R code 的 artifact ids 和下一步人工动作。
- static-review input 现在记录 generated-code artifact id、required identifier
  数量、required identifier source id，以及 limited-check scope。
- static-review output 现在包住已有 `static_check_passed_for_review` warning
  decision，并记录 static-check artifact id。
- 增加 focused smoke assertions，证明直接 DatasetGraph code generation 和
  StudyGraph batch execution 都会保留 code/static IO packages。

当前边界：

- 本切片只改变 code generation 和 limited static review 成功路径的 audit
  packaging。
- 不改变 prompt construction、provider calls、approved-spec gates、generated R
  code parsing、static-rule semantics、code review、R execution、compare、
  repair、dependency planning 或 UI 行为。
- Static review 仍然明确是 limited scope。本切片没有新增 clinical、
  dataset-specific、study-specific、demo-specific 或 variable-specific static
  rule。
- Reference ADaM 仍然只是 compare/output-shape evidence，不能作为 derivation
  authority。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_generate_code_uses_input_spec_and_stops_for_review tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Execution Agent IO Migration 切片

已完成：

- 将 DatasetGraph `execute_approved_code` 结果包装迁到 `execution_agent` 的
  typed IO packages。
- execution-agent input 现在记录明确 execution task、code path、static-check
  path、是否提供 Rscript path，以及可用上游 artifact ids。
- execution-agent output 现在包住已有 execution audit decision：
  - `r_execution_completed`
  - `r_execution_terminal_failure`
- output 记录 validation status、terminal-failure status、output path、risk
  flags，以及 execution/validation/failure artifact ids。
- 增加 focused smoke tests，用 patched execution boundary 覆盖成功执行和
  terminal-failure 执行，因此测试验证 graph state packaging，而不依赖本机 R
  是否可用。

当前边界：

- 本切片只改变现有 execution boundary 返回后的 result audit packaging。
- 不改变 code-review validation、stale-input checks、static-rule validation、
  R sandbox behavior、output validation、terminal-failure routing、repair
  policy、compare、dependency planning、provider calls 或 UI 行为。
- 这不是 sandbox hardening 切片。更强的 OS/container 隔离仍然是独立的生产化
  工作。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_execute_records_execution_agent_io tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_execute_records_terminal_failure_agent_io -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Audit Agent IO Migration 切片

已完成：

- 将 StudyGraph `write_audit_manifest` 的 audit-summary 步骤迁到
  `audit_agent` typed IO packages。
- audit-agent input 现在记录 study status、target datasets、dataset-result
  数量、agent-decision 数量、risk-flag 数量，以及上游 audit artifact ids。
- audit-agent output 现在包住一条新的 study-level
  `agent_audit_summary_written` decision，并记录 agent-summary artifact id、
  summary type、decision count、dataset count。
- 最终 study manifest metadata 现在包含 audit-agent IO package 和
  audit-agent decision，同时保留之前 dataset-node IO packages。
- 增加 StudyGraph smoke assertion，证明 audit-agent IO package 会出现在最终
  graph result 和 audit manifest metadata 中。

当前边界：

- 本切片只改变最终 audit packaging。
- 不改变 dependency planning、dataset dispatch、dataset generation、human
  gates、provider calls、R execution、compare、repair、static rules 或 UI。
- agent summary 仍然只是 derived read model，不成为 workflow truth，也不改变
  dataset state。
- audit-agent decision 会在 summary 写出后追加，所以 summary 不会把自己算进
  统计里。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/study_graph.py'), pathlib.Path('tests/test_graph_smoke.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-05-31 - LG2.4 Gateway Agent IO Persistence 切片

已完成：

- 在 canonical `DatasetRunState` 和 `StudyRunState` 里新增
  `agent_node_inputs` 与 `agent_node_outputs`。
- `GraphGateway.finalize_inputs()`、`generate_code()`、
  `execute_approved_code()` 现在会保留 DatasetGraph product nodes 返回的
  typed IO packages。
- Gateway recorder methods 会用现有 `AgentNodeInput` / `AgentNodeOutput`
  合同校验 IO package，再写入 canonical state。
- 每次 canonical graph-state 写入时，Gateway 都会把 dataset-level agent IO
  汇总到 study-level read model，模式与已有 agent-decision rollup 保持一致。
- 增加 focused Gateway regression，证明 input-spec finalize 和 code
  generation 会把 agent IO 同时写到 dataset state、study state 和
  `graph_state.json`。
- 后续补充 focused coverage，证明 approved-code execution 也会把
  `execution_agent` IO 同时写到 dataset state、study state 和
  `graph_state.json`。

当前边界：

- 本切片只补齐 DatasetGraph product-node IO packages 到 Gateway-owned
  canonical state 的持久化链路。
- 不改变 workflow routing、provider calls、dependency planning、draft-spec
  authority、code generation prompts、static-rule semantics、R execution、
  compare、repair、terminal-failure handling 或 UI 行为。
- Static rules 仍然是 generic 且 limited-scope。本切片没有加入
  dataset-specific、demo-specific、variable-specific 或 PSY201-specific 规则。
- Reference ADaM 仍然只是 compare/output-shape evidence，不能作为
  derivation authority。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_existing_input_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_generates_code_through_dataset_graph_and_records_state -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('src/adam_agent/schemas/graph_state.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
```

### 2026-05-31 - LG2.7 Agent Node Trace UI 切片

已完成：

- 扩展现有 Agent Audit 面板，用人话显示来自 canonical graph state 的 agent
  node handoff trace。
- UI 现在优先读取 active dataset 的 `agent_node_inputs` 和
  `agent_node_outputs`，没有 active dataset 记录时再回退到 study-level graph
  state。
- Trace cards 显示 bounded agent role、node、task、decision/status、dataset
  scope、risk-flag 数量和 artifact-reference 数量。
- 面板仍然不显示原始 JSON，主界面也不暴露技术路径。
- 增加 focused UI contract test，证明面板使用 graph-state agent IO 字段，并
  继续避免 `JSON.stringify`。

当前边界：

- 本切片只改变 browser read model 和展示。
- 不改变 FastAPI endpoints、GraphGateway persistence、workflow routing、
  provider calls、dependency planning、static rules、R execution、compare、
  repair 或 Reference ADaM authority。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Initial Compare Action UI 切片

已完成：

- 更新 result Compare tab：当 generated output preview 和 reference ADaM preview
  都存在时，用户可以直接点击第一次 `Run Compare`。
- 已有 compare summary 后，保留原来的 `Run Compare Again`。
- 当 generated output 或 reference ADaM table 缺失时，明确显示 compare 目前不可用。
- 在 empty state 中再次说明：reference ADaM 只是 comparison evidence，不是
  derivation authority。
- 增加 focused UI contract test，覆盖首次 compare 可用状态和 reference 缺失状态。

当前边界：

- 本切片只改变 browser result panel 的 action wiring 和文案。
- 不改变 `/compare` API endpoint、GraphGateway compare persistence、
  dependency planning、dependency resolution、provider calls、static rules、R
  execution、repair 或 Reference ADaM authority。
- 不自动运行 compare；仍然由用户点击 `Run Compare`。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_allows_initial_compare_when_reference_preview_exists tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway -v
python -B -m unittest tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Review-Only Output UI 文案切片

已完成：

- 增加 browser helper，从 graph progress 或 review-summary read model 读取
  dataset output quality，再决定用户可见的输出状态文案。
- 调整 Dependency Map 的 runtime meaning：structural demo 和 mock/offline
  outputs 会明确显示为 review-only，且不能满足下游 runtime dependency。
- 调整 Dataset Execution Cards：review-only/demo outputs 使用 warning stage
  style，不再和真实 local R runtime output 共用 completed-run 视觉状态。
- 调整 completed execution 后的 next action：review-only/demo output 只能作为
  review evidence 检查，不能作为另一个 dataset 的 runtime input。
- 增加 focused UI contract test，固定共享 output-quality 文案路径。

当前边界：

- 本切片只改变 browser read-model 文案和 stage styling。
- 不改变 canonical graph state、dependency planning、dependency resolution、
  provider calls、static rules、R execution、compare、repair 或 Reference ADaM
  authority。
- 不阻止 mock/demo output 被展示给用户 review，只避免 UI 把它说成真实 runtime
  dependency evidence。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -m unittest tests.test_api_phase8 -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Input-Change Target Selection Reset UI 切片

已完成：

- 当上传输入发生变化、旧 plan/code/review 状态被 invalidated 时，清空
  browser 里的 `selectedTargetsForPlan`。
- 更新 stale-plan 文案，提醒用户先重新检查 output selection，再刷新
  dependency planning。
- 增加 focused UI contract test，证明 input-change invalidation 会和
  plan/generated/review state 一起清理 stale planned targets。

当前边界：

- 本切片只改变 browser invalidation state 和解释文案。
- 不改变 upload/scanning behavior、`/study-inputs`、dependency planning
  semantics、GraphGateway invalidation、provider calls、static rules、
  R execution、compare、repair 或 Reference ADaM authority。
- target candidates 仍然由现有 render path 根据最新 input summary 重新计算；
  本切片只清理旧 planned selection。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_input_change_clears_stale_planned_targets tests.test_api_phase8.Phase8ApiTests.test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Reference-Only Target Selection UI 切片

已完成：

- 增加 browser-side target evidence read model，记录每个 target candidate
  来自 input specs、legacy code、reference ADaM、manual entry、graph state
  还是 progress state。
- Reference-only target 仍然显示在 Choose Output 和 Dataset Execution
  Cards 中，但不再被系统自动纳入 plan。
- Demo/autoselect 流程现在只有在至少存在一个非 reference-only target 可以
  auto-plan 时，才会自动准备 dependency plan。
- target chip 上增加来源提示，例如 `spec evidence`、`legacy evidence`、
  `spec + reference`、`reference only`。
- Finalize/generate 按钮现在会提示用户：若要生成 reference-only target，
  必须先明确勾选该 target。
- 增加 focused UI contract tests，覆盖 reference-only target-selection 行为。

当前边界：

- 本切片只改变 browser target-selection state 和解释文案。
- 不改变 `/study-inputs`、target discovery payloads、dependency planning
  semantics、GraphGateway state transitions、provider calls、static rules、
  R execution、compare、repair 或 Reference ADaM authority。
- 用户仍然可以明确选择 reference-only target。本改动只是防止 UI 把
  Reference ADaM evidence 静默变成默认 generation request。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae tests.test_api_phase8.Phase8ApiTests.test_index_keeps_reference_only_targets_unplanned_until_explicitly_selected tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Advanced Setup Wording UI 切片

已完成：

- 将高级折叠面板重命名为 `Advanced setup and audit files (usually not
  needed)`。
- 增加人话说明：正常流程不需要修改这些技术字段。
- 重写 run id、config file、provider/model、API key、local Rscript 的可见
  label/help text，让它们表现为排障/审计设置，而不是普通用户必须理解的产品
  输入。
- 保留原 field ids 和 request payload 行为。
- 增加 focused UI contract checks，覆盖新文案。

当前边界：

- 本切片只改变 browser UI 的静态 HTML/CSS 文案和对应 UI contract tests。
- 不改变 provider resolution、API key handling、run id generation、
  config loading、Rscript invocation、GraphGateway state、dependency
  planning、static rules、R execution、compare 或 repair。
- 不移除 real LLM provider controls，只是明确它们属于 advanced setup
  controls。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 Reference-Only Dataset Card UI 切片

已完成：

- 更新 Dataset Execution Cards：如果某个 target 只是因为用户上传了
  Reference ADaM 文件而出现，不再让它看起来可以直接进入 code generation
  阶段。
- reference-only card 的 `code` 阶段保持 inactive，除非该 target 真的被纳入
  当前 run 的 plan，或者已有 generated code / review history。
- 增加明确 card context 文案：Reference ADaM 只用于 compare/output-shape
  evidence，不是 generation input。
- 增加 focused UI contract test，锁住 reference-only card 行为。

当前边界：

- 本切片只改变 browser read-model rendering 和解释文案。
- 不改变 target discovery、`/study-inputs`、dependency planning、
  GraphGateway state transitions、provider calls、static rules、R execution、
  compare、repair 或 Reference ADaM authority。
- 不移除 Reference ADaM 可见性，只是避免 card stage strip 把
  reference-only evidence 表达成 runnable generation progress。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"; node --check .tmp_tests/ui_script_check.js; Remove-Item -LiteralPath .tmp_tests\ui_script_check.js -ErrorAction SilentlyContinue
```

### 2026-05-31 - LG2.7 SAS7BDAT Preview Status UI 切片

已完成：

- 改造 browser file-card 对 `.sas7bdat` 文件且 API preview status 为
  `not_previewed` 时的展示。
- UI 现在把这些文件标成 `runtime input`，并说明 SAS dataset 已被识别为 R
  execution 输入，即使当前 browser preview 不可用。
- 增加 focused UI contract test，覆盖 status label 和 summary text。

当前边界：

- 本切片只改变 browser file-card 文案和 pill 样式。
- 不改变 `/study-inputs` API payloads、file scanning、upload handling、
  sas7bdat runtime support、R package requirements、dependency planning、
  GraphGateway state transitions、provider calls、static rules、R execution、
  compare、repair 或 Reference ADaM authority。
- 文案不声称已经成功 preview，而是把 runtime input support 和 browser
  preview availability 分开。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_explains_sas7bdat_not_previewed_as_runtime_input tests.test_api_phase8.Phase8ApiTests.test_study_inputs_marks_sas7bdat_as_runtime_supported_when_preview_unavailable -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Input Warning Path Hiding UI 切片

已完成：

- 改造 browser input-warning 展示：invalid input files 现在显示为
  `Skipped filename: reason`，不再暴露完整本地路径。
- 增加 `inputWarningText()` 这个小的 UI formatter，只供
  `renderInputSummary()` 使用。
- 增加 focused UI contract test，证明 `inputWarnings` 不再直接用
  `item.path` 拼消息。

当前边界：

- 本切片只改变 browser warning 文案。
- 不改变 `/study-inputs` API payloads、file scanning、upload handling、
  dependency planning、GraphGateway state transitions、provider calls、
  static rules、R execution、compare、repair 或 Reference ADaM authority。
- 完整技术路径仍只保留在 API/audit artifacts 和明确的 advanced surfaces 中。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_hides_invalid_file_paths_from_input_warnings tests.test_api_phase8.Phase8ApiTests.test_index_hides_technical_paths_outside_advanced_artifact_view -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Dataset-Neutral Target Selection UI 切片

已完成：

- 移除 browser 在没有推断出 ADaM target 时自动发明 `ADAE` 的 fallback。
- 移除 browser 对 `ADAE` 的 target-selection 优先级；UI 现在从真实
  read model 的 inferred/planned targets 中选择第一个，而不是静默偏向某个
  dataset。
- 增加 focused UI contract test，覆盖 inference、demo auto-selection、
  progress recovery 和 graph-state recovery。

当前边界：

- 本切片只改变 browser target-selection defaults。
- 不改变 target inference rules、manual target entry、dependency planning、
  GraphGateway state transitions、provider calls、static rules、R execution、
  compare、repair 或 Reference ADaM authority。
- Demo data 中如果 specs/reference evidence 包含 ADAE，UI 仍会显示 ADAE。
  UI 不再在缺少证据时自己发明 ADAE。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Approve-Run Dependency Gate UI 切片

已完成：

- 收紧 browser `Approve And Run Locally` 可用性判断：它现在和 finalize、
  code generation 使用同一套 active dependency block gate。
- 如果 graph progress 暂时不可用，但从 recovered dependency plan 能看出 active
  target 被阻塞，UI 会继续禁用 local execution，并说明必须先解决 dependency
  review。
- 增加 focused UI contract assertion 覆盖 approve/run gate。

当前边界：

- 本切片只改变 browser action availability 和用户可见的 gate 文案。
- 不改变 backend execution preflight、dependency planning、GraphGateway state
  transitions、provider calls、static rules、R execution、compare、repair 或
  Reference ADaM authority。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Demo Load Graph Read Model Refresh 切片

已完成：

- 移除 browser demo-load 路径里的直接 progress-only refresh。
- Demo load 仍调用 `scanInputs()`；而 `scanInputs()` 已刷新 graph read
  models，其中包含 graph-owned progress read model。
- 增加 focused UI contract test，证明 `createDemoStudy()` 在扫描 inputs 后不再
  直接调用 `refreshRunProgress()`。

当前边界：

- 本切片只改变 demo load 后的 browser read-model refresh 顺序。
- 不改变 demo file copying、input scanning、target inference、dependency
  planning、GraphGateway state transitions、provider calls、static rules、R
  execution、compare、repair 或 Reference ADaM authority。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_demo_load_does_not_refresh_progress_directly tests.test_api_phase8.Phase8ApiTests.test_index_scan_inputs_refreshes_graph_read_models -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.7 Scan Refresh Graph Read Models 切片

已完成：

- 更新 browser `scanInputs()` 路径：study files 重新扫描后刷新 graph read
  models。
- 这样 demo load 或手动刷新 inputs 之后，Agent Audit、agent-node traces、
  dependency progress 和 review gates 都会继续对齐 canonical graph state。
- 增加 focused UI contract test，证明 `scanInputs()` 调用
  `refreshGraphReadModels()`，而不是只刷新 progress。

当前边界：

- 本切片只改变 browser read-model refresh 顺序。
- 不改变 upload handling、input scanning、dependency planning、GraphGateway
  state transitions、provider calls、static rules、R execution、compare、repair
  或 Reference ADaM authority。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_scan_inputs_refreshes_graph_read_models tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(path.read_text(encoding='utf-8')) for path in files]; print('AST OK')"
python -B -c "from pathlib import Path; html=Path('src/adam_agent/api/web.py').read_text(encoding='utf-8'); script=html.split('<script>', 1)[1].split('</script>', 1)[0]; Path('.tmp_tests').mkdir(exist_ok=True); Path('.tmp_tests/ui_script_check.js').write_text(script, encoding='utf-8')"
node --check .tmp_tests/ui_script_check.js
```

### 2026-05-31 - LG2.4 Validation Agent IO Compare 切片

已完成：

- 扩展 graph-owned compare recording：`validation_agent` 现在会为
  `compare_reference_output` 写入 typed `AgentNodeInput` 和
  `AgentNodeOutput`。
- 保留原有 `validation_agent` decision payload 形态，同时通过 typed node
  output 携带该 decision，避免破坏现有 audit summary 和 UI projection。
- Study-level `agent_node_inputs` 和 `agent_node_outputs` 现在会从 dataset
  state 汇总 compare IO，和 evidence/spec/code/static/execution agent 保持
  一致。
- 收紧 generic agent-record 去重逻辑：同一秒内重复运行但 payload 不同的记录
  会保留在 audit trail 中。
- 增加 gateway tests，证明 compare IO 会同时持久化到 dataset 与 study
  level，同一秒 compare rerun 会保留独立 audit records，并且 compare status /
  result-summary 行为不变。

当前边界：

- 本切片只改变 canonical agent IO persistence for compare。
- 不改变 compare calculation、Reference ADaM authority、dependency planning、
  provider calls、static rules、R execution、repair、terminal-failure routing
  或 UI 行为。
- Reference ADaM 仍然只是 comparison/output-shape evidence，不能作为
  derivation authority。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_keeps_same_second_rerun_audit_records tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_reference_output_computes_and_records_compare -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
```

### 2026-05-31 - LG2.4 Diagnosis/Repair Agent IO Terminal-Failure 切片

已完成：

- 扩展 `GraphGateway.record_terminal_failure_review()`：`diagnosis_repair_agent`
  现在会为 `terminal_failure_review` 写入 typed `AgentNodeInput` 和
  `AgentNodeOutput`。
- 保留原有 terminal-failure triage 语义：
  - 记录人工 triage decision
  - 记录下一步受控 product action
  - 不自动 repair code、revise spec、retry R，也不产出 output
- Study-level `agent_node_inputs` 和 `agent_node_outputs` 现在会从 dataset
  state 汇总 terminal-failure triage IO。
- 增加 focused gateway assertions，证明 diagnosis/repair IO 会出现在 dataset
  level、study level 和持久化 `graph_state.json` 中。

当前边界：

- 本切片只改变 terminal-failure review 的 canonical agent IO persistence。
- 不实现 dedicated LLM repair prompt、spec-revision subgraph、retry
  execution、dependency planning、static rules、compare、R execution 或 UI 行为。
- Terminal-failure actions 仍然是 human-controlled gates。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_index_exposes_agent_audit_from_graph_state -v
python -B -c "import ast, pathlib; files=[p for p in pathlib.Path('src').rglob('*.py')]+[p for p in pathlib.Path('tests').rglob('*.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print(f'AST OK: {len(files)} Python files')"
```

### 2026-05-31 - LG2.4 Dependency Agent IO Planning 切片

已完成：

- 扩展 canonical dependency planning：study-level `dependency_agent` 现在会为
  `dependency_plan` 写入 typed `AgentNodeInput` 和 `AgentNodeOutput`。
- 保留原有 `dependency_plan_prepared` decision payload，并把它绑定到 typed
  output package 中，所以 agent audit summary 仍然统计同一条 decision。
- 更新 study-level agent IO 同步逻辑：先保留 study-scoped agent records，再
  汇总各 dataset state 的 dataset-scoped records。这样后续 dataset 动作不会
  抹掉 dependency-agent handoff。
- 增加 gateway regression，证明 dependency-agent IO 会出现在 canonical graph
  state 和持久化 `graph_state.json` 中，同时原有 dataset-level IO 仍然存在。

当前边界：

- 本切片只改变 dependency planning 的 canonical agent IO persistence。
- 不改变 dependency planning logic、dependency review routing、provider
  calls、draft spec generation、code generation、static rules、R execution、
  compare、repair 或 UI 行为。
- dependency agent 仍然只是受边界约束的 planning/audit 角色。它不发明临床
  推导逻辑，也不会把 Reference ADaM 当作 derivation authority。

验证：

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_agents_contract -v
```

### 2026-05-31 - LG2.4 Gateway Fallback Agent IO 切片

已完成：

- 为 Gateway fallback recorder 路径补充默认 typed `AgentNodeInput` /
  `AgentNodeOutput`。当调用方没有提供 DatasetGraph product-node IO 时，低层
  recorder 也不会只留下 agent decision。
- 覆盖以下 fallback IO：
  - draft spec recording
  - input spec readiness
  - approved draft spec readiness
  - generated code 与 limited static-check recording
  - execution result recording
- 保留原有 fallback `AgentDecision` payload，并把同一条 decision 绑定进 typed
  output package。
- 增加 focused gateway assertions，证明直接调用 Gateway recorder 时，dataset
  state 和 study-level rollup 都会留下 typed IO。

当前边界：

- 本切片只收口 fallback recorder path 的 audit packaging 缺口。
- 不改变 DatasetGraph product-node IO、dependency planning、provider calls、
  static rules、R execution、compare、repair 或 UI 行为。
- fallback IO 是兼容/审计安全网。正常产品流仍然优先使用 DatasetGraph product
  nodes 输出的更丰富 IO。

验证：

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_agents_contract -v
```

### 2026-05-31 - LG2.4 Agent Audit Node IO Summary 切片

已完成：

- 扩展 derived `agent_audit_summary` read model：除了 agent decisions，现在也会汇总
  typed `AgentNodeInput` / `AgentNodeOutput` handoff。
- 增加 study 级 node IO 计数、无效 node IO 计数、按 agent 统计的 node output
  计数，以及最新 node-output 摘要。
- 增加 dataset 级 node IO 计数和最新 node-output 摘要，让审计时能看到具体是
  哪些 bounded agent node 处理过某个 dataset。
- StudyGraph 直接写 audit summary 的路径也传入收集到的 node IO，与 canonical
  GraphGateway 路径保持一致。

当前边界：

- 本切片只增强 derived audit read model。
- 不改变 graph routing、dependency planning、provider calls、static rules、
  R execution、compare、repair、UI 行为或 Reference ADaM authority。
- canonical truth 仍然是 `graph_state.json`；summary 仍只是给人看的派生审计
  artifact。

验证：

```text
python -B -m unittest tests.test_agents_contract -v
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_executes_approved_code_through_dataset_graph_and_records_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state -v
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_batch_path_preserves_agent_decisions -v
```

### 2026-05-31 - LG2.7 Terminal-Failure Triage UI 切片

已完成：

- 当 graph state 显示当前 dataset 的 `execution.status == "terminal_failure"` 时，
  浏览器 UI 会显示 terminal-failure triage 面板。
- 面板暴露 GraphGateway 已支持的受控人工动作：
  - `retry_execution`
  - `repair_code`
  - `revise_spec`
  - `request_new_input`
  - `skip_dataset`
  - `continue_other_datasets`
- 这些动作会调用已有
  `/runs/{run_id}/datasets/{dataset}/terminal-failure-review` endpoint。
- 决策记录后，UI 会刷新 canonical graph read models，并显示 graph-owned next
  action，而不是在浏览器端自己发明新的 workflow path。
- 增加 UI contract 测试，确认面板、动作按钮、endpoint 调用和 graph-state refresh
  hook 都存在。

当前边界：

- 本切片只改变浏览器 UI。不改变 GraphGateway terminal-failure 语义、repair
  behavior、R execution、static rules、compare、dependency planning 或 Reference
  ADaM authority。
- 该动作仍是 human-controlled graph gate。UI 只记录选择，不会自动 retry、repair、
  revise spec、request inputs、skip，也不会自行继续下游 datasets。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned Terminal-Failure Actions 切片

已完成：

- 将 terminal-failure 可选动作列表上移到 graph progress read model。
  当某个 dataset 有 open `terminal_failure` interrupt 时，`GraphGateway`
  会在该 dataset 的 progress item 中暴露 `available_actions`。
- 保留同一组 6 个受控处置动作：
  `retry_execution`、`repair_code`、`revise_spec`、`request_new_input`、
  `skip_dataset`、`continue_other_datasets`。
- 浏览器 triage 面板现在优先读取 dataset progress 里的 actions。前端本地常量
  只作为旧 read model 的兼容 fallback。
- terminal-failure 决策被记录后，graph progress 不再暴露该 dataset 的过期
  triage actions，因此 UI 会隐藏旧按钮，并显示 graph-owned next action。
- 增加 API/model 和 gateway 测试，证明 terminal-failure actions 只在 graph-owned
  interrupt 仍 open 时可见。

当前边界：

- 本切片是 read-model ownership 修正。不改变 R execution、repair routing、
  static rules、compare behavior、dependency planning、provider calls 或
  Reference ADaM authority。
- UI 仍通过已有
  `/runs/{run_id}/datasets/{dataset}/terminal-failure-review` endpoint 记录人工
  决策；它不会自己执行后续动作。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions tests.test_api_phase8.Phase8ApiTests.test_execute_approved_code_terminal_failure_is_explicit tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.8 Progress Compatibility Projection Path 切片

已完成：

- 收紧 `GraphGateway.progress_summary()` 元数据：只有当 compatibility projection
  文件真实存在时，才返回 `workflow_state_path`。
- 将 `RunProgressResponse.workflow_state_path` 改为可空。
- 保持 graph-owned progress 行为不变：`graph_state.json` 仍是事实来源；
  即使 `workflow_state.json` 缺失，`/progress` 仍可用。
- 增加 gateway 层和 FastAPI endpoint 测试，证明缺失 `workflow_state.json` 时返回
  `None` / `null`，而不是返回一个不存在的路径。

当前边界：

- 这是 read-model metadata 真实性修正。
- 不删除 `workflow_state.json`，不改变 projection 写入，不改变 product transition，
  也不改变 UI 行为，只是暴露更真实的 null metadata。

审查：

- 子 agent 审查 GO。
- 审查指出其他 response model 仍要求 `workflow_state_path`，但那些 response 对应会
  创建或依赖 compatibility projection 的操作；本切片只针对 progress read model。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_missing_workflow_projection_as_none -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_missing_workflow_projection_as_null tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/models.py'), pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('tests/test_api_phase8.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Dependency Next-Action Text 切片

已完成：

- 收紧 dependency map 中的 `nextActionText(...)`。
- 当 graph progress 已加载，但某个 target 没有 dataset progress item 时，UI
  不再用本地 generated/review/execution 缓存推断“review code”或
  “inspect output”。
- dependency map 现在会提示刷新 graph state 或重新准备 dependency plan；同时保留
  Reference ADaM 只是 compare/output-shape evidence 的文案。
- 增加 Node 执行的 UI 测试，证明 graph progress 已加载但缺该 target 时，过期的
  本地 generated/review/execution 缓存不会影响下一步提示。

当前边界：

- 这是 UI guidance text 修正。
- 不改变 action gating、GraphGateway progress 生成、dependency planning、
  provider calls、R execution、compare、repair 或 Reference ADaM authority。

审查：

- 子 agent 审查 GO，并确认该 guard 在真实 graph target action
  （`blocked_reason` / `action_label`）之后才执行，不会隐藏有效 graph action。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_next_action_text_ignores_local_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_dataset_status_ignores_local_completion_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Dataset Status Fallback 切片

已完成：

- 收紧 `datasetStatus(...)`：本地 generated/review/execution 缓存只在 graph
  progress read model 不可用时才作为 fallback。
- 当 `state.runProgress` 已存在，但某个 target 没有 dataset progress item 时，
  UI 只退回到 plan/reference/candidate 类状态：
  `ready`、`reference evidence`、`waiting` 或 `candidate`。
- 同样收紧 `datasetOutputQualityStatus(...)`。当 graph progress 已加载时，
  persisted review-summary quality 不再驱动 dataset status。
- 增加 Node 执行的 UI 测试，证明本地 generated/execution/review 和
  output-quality 缓存即使过期存在，也不能在 graph progress 已存在但缺该 target
  时把 dataset 标成 completed 或 review-only。

当前边界：

- 这是 UI read-model fallback 行为修正。
- 不改变 GraphGateway progress 生成、product state、provider calls、
  R execution、compare、repair、dependency planning 或 Reference ADaM authority。

审查：

- 子 agent 第一次审查 NO-GO：output-quality fallback 仍在 graph-progress guard
  之前读取 persisted review summary。
- 已修复：`datasetOutputQualityStatus(...)` 在 `state.runProgress` 存在且目标没有
  progress quality 时返回空值。
- 子 agent 复审 GO。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_status_ignores_local_completion_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.7 Graph-Owned Generate Gate 切片

已完成：

- 新增 `graphAllowsCodeGeneration(progress)`，让浏览器把 graph progress 的
  `next_action` 作为代码生成可用性的依据。
- 当 dataset progress 说明下一步是 `generate_code`、`repair_generated_code`
  或 `revise_approved_spec` 时，UI 不再仅因为浏览器本地 spec-gate 缓存缺失或
  过期而阻止 Generate 动作。
- 在 `generateCode()` 内部也使用同一 graph-owned guard，保证按钮可用状态和实际
  点击路径一致。
- 保留人工审核边界：
  - `review_draft_spec` 仍进入 draft-spec review；
  - `revise_approved_spec` 仍调用 `/draft-spec`；
  - 生成后的代码仍需要 code review 才能执行。
- 增加 UI 行为测试，覆盖“graph 已允许生成，但本地 spec gate 缓存为空”的情况。

当前边界：

- 这是 UI gate/read-model 修正。
- 后端 GraphGateway/product endpoints 仍是真正的硬校验边界。
- 不改变 dependency planning、draft-spec approval 语义、provider calls、
  R execution、compare、repair、static rules 或 Reference ADaM authority。

审查：

- 子 agent 审查 GO。
- 审查确认这不会绕过 draft-spec/code-review 审核门，因为 graph progress 仍是事实
  来源，后端校验仍然保留。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_draft_review_gate_overrides_local_input_spec_shortcuts -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.7 Graph-Owned Dataset Card 阶段条切片

已完成：

- 将 dataset card 的阶段条（`code`、`review`、`run`）继续收敛到 graph
  progress read model。
- 新增前端 helper：
  - `codeStageClassFor(...)`
  - `reviewStageClassFor(...)`
  - `runStageClassFor(...)`
- 这些 helper 现在以 `GET /runs/{run_id}/progress` 中每个 dataset 的
  `code_status`、`execution_status`、`next_action` 作为阶段完成显示依据。
- 移除了本地浏览器/review-summary fallback 对阶段完成态的影响：
  `generatedFor(...)`、`reviewFor(...)`、`executionFor(...)`、
  `datasetReviewFor(...)` 在 graph progress 没有说明完成时，不能再把阶段条标成
  `done`。
- 保留 Reference ADaM 保护：reference-only target 不进入 code 阶段；
  review-only/mock/stub output 仍显示为 review-only，不显示成 runtime-complete。
- 增加 Node 执行的 UI matrix 测试，证明 graph progress 驱动阶段条，并证明本地
  generated/review/execution 缓存即使存在，也不能在 graph progress 为空时把阶段标
  成完成。

当前边界：

- 这是 UI read-model 展示切片。
- 不改变 GraphGateway、dependency planning、provider calls、static rules、
  R execution、compare、repair 或 terminal-failure 语义。
- 阶段条只是 graph progress 的查看器，不成为 workflow controller，也不决定 API
  调用。

审查：

- 子 agent 第一次审查 NO-GO：阶段 helper 仍把本地浏览器缓存和 persisted review
  summary 当成完成 fallback。
- 已修复：从完成态判断中移除这些 fallback，并增加“本地缓存存在但 graph progress
  为空”的回归用例。
- 子 agent 复审 GO。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_dataset_cards_keep_reference_only_targets_out_of_code_stage tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_dataset_card_stages_prefer_graph_progress_read_model -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned 主动作门控切片

已完成：

- 新增浏览器 `graphActionGate()` helper，让四个主要 dataset 动作优先由
  `GET /runs/{run_id}/progress` 中每个 dataset 的 `next_action` 驱动。
- 将 graph-owned next action 映射到 UI 动作：
  - `finalize_inputs` / `reconfirm_inputs` -> finalize inputs
  - `review_draft_spec` -> approve draft spec
  - `generate_code` / `repair_generated_code` -> generate R code
  - `revise_approved_spec` -> 通过 `/draft-spec` 生成修订版 draft spec，
    不走普通 finalize
  - `review_code` / `execute_approved_code` / `retry_approved_execution` ->
    code approval 或 local execution
- 当 graph progress 已经说明代码可执行或可重试执行时，UI 不再重复写入
  code-review 决策。
- 修正 revised-spec 路径：terminal-failure 后选择 `revise_spec` 时，不能静默
  复用上传的 input spec 或旧 approved draft。`DatasetGraph` 现在会先判断
  `force_new_draft_spec`，再决定是否接受 input-spec shortcut；UI 在
  `revise_approved_spec` 时调用 `/draft-spec`。
- 修正 draft-review UI gate：当 graph progress 是 `review_draft_spec` 时，
  graph 审核门优先于浏览器本地“已有 input spec / old approved draft”的快捷判断。
- 增加 Node 执行的 UI 行为测试，覆盖 next-action/button matrix；同时补充
  terminal-failure revise-spec API 回归测试。

当前边界：

- 本切片只改变 UI 动作门控和一个 DatasetGraph 控制流 guard。
- 不改变 dependency planning、provider calls、static rules、R execution、
  compare、repair implementation、terminal-failure triage semantics 或
  Reference ADaM authority。
- 浏览器仍然调用现有 split-flow endpoints；它不是 workflow controller。
  GraphGateway progress 仍是 UI 下一步动作的事实来源。

审查：

- 子 agent 第一次审查 NO-GO：`revise_approved_spec` 可能通过普通 finalize
  复用旧 spec 证据。已修复。
- 子 agent 第二次审查 NO-GO：`review_draft_spec` 仍可能被浏览器本地 input-spec
  shortcut 阻挡。已修复。
- 最终子 agent 审查 GO。提交前顺手修正了一个非阻塞提示文案。

验证：

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_graph_smoke -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/api/web.py'), pathlib.Path('src/adam_agent/graph/dataset_graph.py'), pathlib.Path('tests/test_api_phase8.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
node --check .tmp_tests\ui_script_check.js
```

### 2026-05-31 - LG2.7 Graph-Owned Human Review Queue 切片

已完成：

- 在 graph progress read model 中新增 `review_queue`。`GraphGateway` 现在从
  canonical study/dataset interrupt 和 graph-owned next action 推导当前打开的
  人工审核门。
- read model 包含 review scope、dataset、interrupt name、source、reason、
  action 和 action label，所以当前 progress payload 可用时，浏览器不需要再从
  raw graph state 自己推断审核队列。
- 浏览器 human-review queue 现在优先使用 `progress.review_queue`。旧的本地推断
  逻辑只作为旧 progress payload 的兼容 fallback 保留。
- 根据子 agent 审查意见修正 review-queue 语义：
  - terminal-failure 已经 triage 后的 follow-up 状态不再显示为 open triage
    review gate；
  - dependency status 需要审核但没有 interrupt payload 时，仍会显示 study-level
    dependency review；
  - dataset-level interrupt 从 dataset progress 处理，不会误当作 study-level
    interrupt。
- 增加 focused gateway 和 UI contract 测试，证明审核队列由 graph progress 暴露，
  并且 UI 会优先消费它。

当前边界：

- 本切片只改变 read-model ownership 和 UI consumption。
- 不改变 approval semantics、interrupt resume behavior、dependency planning、
  provider calls、static rules、R execution、compare、repair 或 Reference ADaM
  authority。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_blocks_review_required_dependency_sources tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
node --check .tmp_tests\ui_script_check.js
```

### 2026-06-01 - LG2.8 Gateway-Owned Compatibility Metadata 切片

已完成：

- 将 graph compatibility response metadata 继续收回 graph 边界：
  `project_graph_state_to_workflow()` 现在会把 `workflow_control`、
  `graph_state_path`、`workflow_state_path` 写入 compatibility
  `workflow_state.json` 投影。
- 移除 FastAPI service 中旧的“根据 `study_dir` 和 `run_id` 自己拼
  graph/workflow 路径”的 helper。
- 产品 split-flow responses 现在从 `GraphGateway` result projection 中复制
  compatibility metadata。
- 对 schema 不包含 `workflow_control` 的响应：
  `RunPlanResponse`、`DependencyReviewResponse`、
  `TerminalFailureReviewResponse`，只从同一个 gateway projection 复制两个路径
  字段。
- 增加回归测试，证明：
  - graph projection 自己携带它对外声明的 compatibility metadata；
  - service 不再保留旧的本地拼路径 helper；
  - dependency-review 和 terminal-failure-review response 会转发 gateway
    projection paths，而不是在 service 层重新拼一个看起来正确的路径。

当前边界：

- 这是 read-model/compatibility metadata 清理，不改变业务流程。
- 不改变 canonical `graph_state.json`、dependency planning、interrupt
  semantics、provider calls、static rules、R execution、compare 或 legacy
  `/runs` behavior。
- `GET /runs/{run_id}/progress` 仍是 graph-state read model；当 compatibility
  projection 文件不存在时，仍返回 `workflow_state_path: null`。

审查：

- 子 agent 审查 GO。
- 审查指出的测试缺口已补：新增 dependency-review 和
  terminal-failure-review 的 API 层 projection path 转发测试。

验证：

```text
python -B -m unittest tests.test_graph_gateway -v
python -B -m unittest tests.test_api_phase8 -v
python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Checkpointer Boundary Extraction 切片

已完成：

- 新增 `src/adam_agent/graph/checkpointing.py`，作为明确的 LangGraph checkpointer
  边界。
- `GraphGateway` 现在通过这个边界获得默认 checkpointer，并由同一个模块生成
  runtime persistence metadata。
- 当前边界只暴露 in-memory backend；未知 backend 会 fail closed。
- 自定义 checkpointer 会被报告为 `custom`，但不会被说成 persistent。

当前边界：

- 本切片不安装、不启用 `langgraph-checkpoint-sqlite`。
- 默认产品路径的 `langgraph_checkpointer_persistent` 仍为 `false`。
- restart recovery 仍来自 `graph_state.json`。
- `graph_checkpoints.sqlite` 仍是 product audit ledger，不是 LangGraph checkpointer。

审查：

- 子 agent 审查返回 GO。
- 审查确认 checkpointer 边界集中管理创建和 metadata，但没有启用未支持的
  persistent checkpointer；`GraphGateway` 默认仍保持之前的 InMemory 行为；未知
  backend 会 fail closed；custom checkpointer 不会被说成 persistent；公开 API/UI
  默认行为未改变。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_backend tests.test_graph_gateway.GraphGatewayTests.test_custom_checkpointer_is_reported_without_persistence_claim -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 267 tests in 28.419s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Dependency-Review Interrupt 试点切片

已完成：

- 新增一个很窄的内部 `StudyGraph` mode：`native_dependency_review`，在 study-level
  dependency-review gate 使用 LangGraph 原生 `interrupt()` 暂停。
- 新增 `GraphGateway.start_native_dependency_review()` 和
  `GraphGateway.resume_native_dependency_review()` 两个试点入口。
- 试点会把 native interrupt metadata 写入 `runtime_persistence`，但仍保持
  `graph_state.json` 是产品事实来源。
- 增加回归测试，证明 StudyGraph 可以停在 `wait_for_dependency_review`，再用
  `Command(resume=...)` 恢复，并通过 GraphGateway 持久化审核后的状态。

当前边界：

- 这个试点还没有接入公开 UI/API 默认路径。
- 它只覆盖 study-level `dependency_review` gate，不覆盖 draft-spec、code-review、
  terminal-failure 或 multi-dataset product execution gates。
- 当前仍使用默认 `InMemorySaver`，所以进程重启后的恢复仍依赖 canonical
  `graph_state.json`；只有后续接入 persistent LangGraph checkpointer 后，才能宣称
  checkpointer-backed recovery。
- 现有 split-flow endpoints 不变。

审查：

- 子 agent 审查返回 GO。
- 审查确认该试点真实使用 LangGraph `interrupt()` 和 `Command(resume=...)`，
  与公开/default split-flow endpoints 隔离，persistence 说明保持保守，并且没有引入
  可能破坏现有行为的 reducer/schema 问题。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_study_graph_native_dependency_review_interrupt_can_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dependency_review_interrupt_roundtrip_persists_state -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke -v
Ran 155 tests in 12.515s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 109 tests in 18.032s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Dataset Draft-Spec Native Interrupt 试点切片

已完成：

- 在 `DatasetGraph` 中新增内部试点节点 `wait_for_draft_spec_review`。
- 当 dataset state 显式设置 `native_draft_spec_review=True`，且
  `draft_spec_agent` 已生成需要审核的 draft spec 时，DatasetGraph 会使用
  LangGraph 原生 `interrupt()` 暂停在 dataset-level `draft_spec_review`。
- 使用 `Command(resume=...)` 恢复后：
  - `approve` 会关闭 native interrupt，并把下一步标记为
    `persist_draft_spec_review`；
  - `reject` 会关闭 native interrupt，并把 dataset 标为 failed，失败类型为
    `human_rejected_draft_spec`。
- `compile_dataset_graph()` 现在接受可选 checkpointer，用于内部 native
  interrupt 测试；默认调用方式保持兼容。

当前边界：

- 这是 dataset-level native interrupt 的内部试点，不接入公开 UI/API 默认路径。
- 当前公开产品流仍通过 `GraphGateway` split-flow endpoint 记录 draft-spec review
  artifact、校验 hash/fingerprint，并投影到 `workflow_state.json`。
- 本切片不实现 persistent LangGraph SQLite/Postgres checkpointer，不改变 LLM
  生成、R execution、compare、static rules、repair 或 UI 行为。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_draft_spec_review_interrupt_can_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_draft_spec_review_reject_closes_interrupt_as_failed tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_prepare_generates_draft_spec_then_stops_for_review -v
```

### 2026-06-01 - LG2.8 Legacy Run Projection Metadata 切片

已完成：

- 收紧 legacy successful `POST /runs` response path：FastAPI service 层不再
  自己重建 compatibility metadata。
- `run_study_from_request()` 现在把完整 `GraphGatewayLegacyRunResult` 交给
  response construction。
- `_response_from_legacy_graph_result()` 从 gateway-owned legacy
  `workflow_projection` 复制 `workflow_control`、`graph_state_path`、
  `workflow_state_path`。
- 删除 service 层不再使用的 legacy compatibility constant import。
- 增加 sentinel API 回归测试，证明 legacy response 转发 gateway projection
  path，而不是本地重新拼路径。

当前边界：

- 本切片只改变 legacy `/runs` response metadata ownership。
- 不改变 legacy stub run behavior、blocked LLM `/runs` split-flow guard、
  graph state、dependency planning、product split-flow endpoints、review
  summary read models 或 UI behavior。

审查：

- 子 agent 审查 GO。
- 审查确认 blocked LLM `/runs` 不会进入新的 response constructor，
  `RunStudyResponse` 也接受这些从 projection 复制出来的字段。

验证：

```text
python -B -m unittest tests.test_api_phase8 -v
python -B -m unittest tests.test_graph_gateway -v
python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.8 当前 API Contract Split-Flow 文档切片

已完成：

- 重写 `docs/phase8_1_api_contract.md`，让它围绕当前真实产品流组织：
  workspace/input upload -> graph-owned dependency planning -> 必要时
  dependency review -> dataset finalize/draft-spec/code-review/execute gates
  -> progress/review/compare/download read models。
- 在 API contract 中明确 `GraphGateway` 的状态归属：
  `graph_state.json` 是产品事实来源，`workflow_state.json` 是当前 UI/API
  read path 使用的 compatibility projection。
- 补充上传失效说明：上传后通过 `GraphGateway` 标记受影响 run，并返回
  `touched_graph_runs`、`touched_runs`、`skipped_graph_runs`。
- 将 `POST /runs` 移到 Legacy Compatibility 小节，并保留
  `graph_state_path: null` 的解释，避免调用方把 legacy smoke output 误认为
  graph-owned product state。
- 补齐当前端点说明：`/product-workspace`、`/studies/files`、
  `/runs/prepare`、`/dependency-review`、dataset split-flow endpoints、
  `/progress`、`/graph-state`、`/review-summary`、table preview、compare、
  download、artifact read 和 LLM connection testing。

当前边界：

- 这是文档对齐，不改变业务行为。
- 不改变 FastAPI 行为、GraphGateway 状态跳转、provider calls、static rules、
  R execution、compare、UI behavior 或 legacy `/runs` behavior。
- 文档有意不宣称已经具备 production-grade CDISC/P21 规则或强化沙盒。

审查：

- 子 agent 审查返回 GO。
- 非阻断建议是补充仍存在的 compatibility artifact read endpoints；已接受，并新增
  一个短小的 compatibility read 小节。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_create_run_rejects_llm_run_to_completion tests.test_api_phase8.Phase8ApiTests.test_upload_endpoint_delegates_input_invalidation_to_gateway tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
git diff --check -- docs/phase8_1_api_contract.md docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

### 2026-06-01 - LG2.8 早期阶段状态摘要校准切片

已完成：

- 更新 LG2.0/LG2.1/LG2.2 的当前状态摘要，避免继续暗示产品 split-flow 状态
  跳转仍由 FastAPI service wrappers 拥有。
- 澄清当前边界：
  - 产品 split-flow 状态变更现在已经归 `GraphGateway` 所有；
  - FastAPI service wrappers 仍负责 request validation、config/provider
    resolution 和 response shaping；
  - 完整产品循环还不是单次 native LangGraph interrupt/checkpointer run。
- 将 open items 收敛到真实剩余架构工作：native full-loop interrupt/resume、
  checkpointer-backed restart recovery、automatic repair/spec-revision routing，
  以及最终移除 legacy stub。

当前边界：

- 这是文档/状态摘要校准，不改变代码。
- 不改变 tests、API behavior、GraphGateway behavior、UI behavior、static rules、
  R execution、repair 或 compare。

审查：

- 子 agent 审查返回 GO。
- 审查确认这些表述符合当前 service-to-gateway delegation，也没有夸大 native
  LangGraph/checkpointer 成熟度。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_compare_endpoint_delegates_stateful_compare_to_gateway -v
git diff --check -- docs/langgraph_2_construction_plan.md docs/langgraph_2_construction_plan_zh.md
```

### 2026-06-01 - LG2.8 Closeout Verification 切片

已完成：

- 对当前 LangGraph-v2 split-flow 架构做收尾级回归检查。
- 覆盖范围包括：
  - FastAPI split-flow API 与 `GraphGateway` 状态委托；
  - graph progress、dependency review、draft-spec/code review、approved execution、
    terminal-failure review、compare 和 upload invalidation；
  - DatasetGraph/StudyGraph product-mode guards；
  - bounded agent IO/audit contract；
  - generic static-rule policy 和 rule-pack admission；
  - local Rscript sandbox boundary。
- 验证结果支持当前阶段结论：已覆盖的 product split-flow 状态变更已经收敛到
  `GraphGateway` 和
  canonical `graph_state.json`；`workflow_state.json` 仍是 compatibility
  projection；UI/API 仍通过 split-flow endpoints 逐步驱动，不是完整 native
  LangGraph interrupt/checkpointer run。

当前边界：

- 本切片是 closeout evidence 记录，不改变代码或产品行为。
- 仍不宣称已经完成：
  - native LangGraph full-loop interrupt/resume；
  - checkpointer-backed process restart 后的完整人工审核恢复；
  - 多 dataset 自动批量生成/审核/执行；
  - automatic repair/spec-revision closed loop；
  - production-grade CDISC/P21/company standards 检查；
  - container/OS-level hardened R sandbox；
  - legacy stub/test-mode path 的最终删除。
- 当前 local Rscript runner 仍只是开发级执行边界，虽然已有 forbidden-call、
  path、argument 和 environment guards。
- 当前 static rules 仍是 generic contract/policy gate，不是完整临床合规证明。

审查：

- 子 agent 审查返回 GO。
- 审查确认 closeout 记录没有夸大 native LangGraph/checkpointer 成熟度，也没有遗漏
  multi-dataset 批量执行、repair/spec-revision、CDISC/P21、强化 sandbox 和
  legacy stub 删除这些未完成边界。

验证：

```text
python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
Ran 179 tests in 19.112s - OK

python -B -m unittest tests.test_graph_smoke tests.test_agents_contract tests.test_static_rules tests.test_sandbox -v
Ran 126 tests in 8.109s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1/LG2.8 Runtime Persistence Boundary 切片

已完成：

- 在 canonical `StudyRunState` 中新增 `runtime_persistence` metadata。
- `GraphGateway._persist_graph_state()` 每次写入 graph state 前都会更新这份 metadata。
- `project_graph_state_to_workflow()` 会把同一份 metadata 投影到
  `workflow_state.json`。
- `GraphGateway.progress_summary()` 和 `RunProgressResponse` 现在也暴露
  `runtime_persistence`，方便 UI/API/reviewer 看清当前恢复能力。
- 当前 metadata 明确说明：
  - 产品事实来源是 `graph_state.json`；
  - `graph_checkpoints.sqlite` 是本地 product audit ledger，不是 LangGraph
    SQLite checkpointer；
  - 当前默认 LangGraph checkpointer 是 `InMemorySaver`；
  - `langgraph_checkpointer_persistent` 当前保守标记为 `false`，只有后续正式接入
    persistent LangGraph checkpointer 后才能改为 `true`；
  - `native_interrupt_resume` 仍是 `false`；
  - restart recovery 目前来自 `graph_state.json`。

当前边界：

- 本切片不实现真正的 LangGraph SQLite/Postgres checkpointer。
- 本切片不改变 workflow routing、review semantics、LLM generation、R execution、
  compare、static rules、UI 行为或 legacy `/runs` 行为。
- 这只是把现有 persistence 能力显式化，防止后续误把本地
  `graph_checkpoints.sqlite` ledger 当作原生 LangGraph checkpoint store。

审查：

- 子 agent 审查返回 GO。
- 审查确认该切片没有把 `workflow_state.json` 重新变成事实来源，也没有夸大 native
  LangGraph/checkpointer resume 成熟度。
- 已接受一个非阻断建议：`langgraph_checkpointer_persistent` 不再根据 class name
  乐观推断，而是在当前切片中保守标记为 `false`。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_gateway_checkpoint_can_be_read_from_same_graph_instance tests.test_graph_gateway.GraphGatewayTests.test_gateway_persists_canonical_state_for_process_restart_resume -v
python -B -c "import ast, pathlib; files=[pathlib.Path('src/adam_agent/graph/gateway.py'), pathlib.Path('src/adam_agent/schemas/graph_state.py'), pathlib.Path('src/adam_agent/api/models.py'), pathlib.Path('src/adam_agent/graph/workflow_state.py'), pathlib.Path('tests/test_graph_gateway.py')]; [ast.parse(p.read_text(encoding='utf-8'), filename=str(p)) for p in files]; print('AST OK')"
```

### 2026-06-01 - LG2.1 Strict Human Resume Gate 切片

已完成：

- 收紧 `GraphGateway.resume()`：human command 必须匹配当前 open graph interrupt，
  否则 fail closed。
- study-level command 只能恢复当前 open study interrupt，例如
  `dependency_review`。
- dataset-level command 只能恢复同一 dataset 上当前 open dataset interrupt，例如
  `ADAE/draft_spec_review`。
- resolved interrupt、缺失 dataset interrupt、dataset 上的错误 interrupt 名称，都不会再
  被静默写入 canonical graph state。
- 增加回归测试，证明不匹配 command 不会写出 `graph_state.json`。

当前边界：

- 本切片只收紧 human command -> interrupt 的匹配规则。
- 不实现完整 native LangGraph interrupt/checkpointer resume。
- 不改变 dependency planning、draft/spec/code review 业务语义、LLM generation、R
  execution、compare、static rules、UI 行为或 legacy `/runs` 行为。
- 这是为后续 native interrupt/resume 做前置安全约束：人工命令必须先被证明属于当前
  open gate，才能进入 canonical state。

审查：

- 子 agent 审查返回 GO。
- 审查确认 resume gate 在 canonical state 持久化前执行，能正确区分 study-level
  和 dataset-level interrupt，能保留无关 dataset 的 open interrupt，也不会阻断现有
  split-flow review endpoints。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_dependency_owns_interrupt_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_preserves_other_dataset_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_study_command_without_matching_open_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_dataset_command_for_other_open_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_resume_rejects_dataset_command_without_dataset_interrupt -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 260 tests in 29.362s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Dataset Code Review Native Interrupt 试点切片

已完成：

- 新增 dataset-level `wait_for_code_review` graph node。
- 当 dataset state 显式设置 `native_code_review=True`，且
  `generate_r_code_agent` 已生成需要审核的 R code 时，DatasetGraph 会使用
  LangGraph 原生 `interrupt()` 暂停在 `code_review`。
- 使用 `Command(resume=...)` 恢复后：
  - `approve` 会关闭 native interrupt，并把下一步标记为
    `persist_code_review`；
  - `reject` 会关闭 native interrupt，并把 dataset 保持为 `needs_review`，
    下一步标记为 `regenerate_code`。
- 新增 `native_code_review_status`、`native_code_review_resume` state 字段，
  用于记录内部试点恢复结果。

当前边界：

- 这是 dataset-level native interrupt 的内部试点，不接入公开 UI/API 默认路径。
- 本切片不写 `review/{dataset}_code_review.json`，也不替代
  `GraphGateway.review_code()` 的 code hash、static-check hash、input
  fingerprint 和 approved spec 校验。
- approve 后不会直接运行 R code；公开产品流仍必须通过 gateway 持久化
  code-review artifact 后，才能进入 approved-code execution。
- 本切片不实现 persistent LangGraph SQLite/Postgres checkpointer，不改变 LLM
  生成、R execution、compare、static rules、repair 或 UI 行为。

审查：

- 子 agent 审查返回 GO。
- 审查确认 `native_code_review` 是显式 opt-in，默认 `GraphGateway`/FastAPI/UI
  split-flow 不会进入该节点；该试点没有绕过 `GraphGateway.review_code()` 的
  review artifact、code hash、static-check hash、input fingerprint 和 approved
  spec 校验；approve 后也不会直接执行 R。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_interrupt_can_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_reject_closes_interrupt_for_regeneration tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes -v
python -B -m unittest tests.test_graph_smoke tests.test_graph_gateway tests.test_api_phase8 -v
Ran 271 tests in 27.177s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Code Review Command Bridge 切片

已完成：

- 新增 `GraphGateway.review_code_from_command()`。
- 该入口接收 graph-native `HumanCommand`，先读取当前 canonical
  `graph_state.json`，并用现有 resume gate 校验 command 是否匹配当前 open
  dataset-level `code_review` interrupt。
- 校验通过后，委托现有 `GraphGateway.review_code()` 写出
  `review/{dataset}_code_review.json`，并复用已有 code hash、static-check hash、
  input fingerprint、approved spec 校验和 canonical state 持久化。
- 增加测试证明：
  - 匹配的 `code_review` command 会进入正式 code-review artifact/state flow；
  - 不匹配当前 open interrupt 的 command 会 fail closed，且不会写 review artifact。

当前边界：

- 本切片只提供 native command 到 gateway artifact flow 的安全桥接入口。
- 不改变公开 UI/API 默认路径，不自动把 DatasetGraph native interrupt 接入浏览器。
- 不复制 `review_code()` 内部校验，不绕过 existing code-review artifact/hash/fingerprint
  语义。
- 不实现 persistent LangGraph SQLite/Postgres checkpointer，不改变 LLM 生成、R
  execution、compare、static rules、repair 或 UI 行为。

审查：

- 子 agent 审查返回 GO。
- 审查确认该桥接入口会先加载 canonical graph state，并使用 open interrupt gate
  校验 command，再委托现有 `review_code()`；它没有复制或削弱 code hash、
  static-check hash、input fingerprint、approved spec 校验；不匹配的 command 会在
  写 review artifact 前 fail closed。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_native_interrupt_to_artifact_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_mismatched_interrupt_without_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_writes_artifact_and_records_canonical_state -v
python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 273 tests in 29.084s - OK

python -B -m compileall -q src tests
git diff --check
```

### 2026-06-01 - LG2.1 Native Code Review Command Guard 切片

已完成：

- 收紧 `GraphGateway.review_code_from_command()`：
  - 如果 study-level interrupt 仍然 open，例如 `dependency_review`，dataset-level
    `code_review` command 会先 fail closed；
  - 保留原有 dataset-level open interrupt 匹配检查。
- 增加 reject bridge 测试，确认 native `reject` command 会进入正式
  code-review artifact/state flow，但不会解锁 execution。
- 增加 study-level gate 测试，确认 study-level interrupt 未解决时不会写
  code-review artifact，也不会把 generated code 状态误改为 approved/rejected。
- 增加 dataset-level rollup 测试，确认顶层 `current_interrupt` 若是带 dataset 的
  `code_review` interrupt，不会被误判成 study-level gate。

当前边界：

- 本切片只补强 native command bridge 的安全语义和测试覆盖。
- 不改变公开 UI/API 默认路径，不自动接入 DatasetGraph native interrupt。
- 不改变 `GraphGateway.review_code()` 的 artifact/hash/fingerprint 校验，不改变 R
  execution、LLM generation、compare、static rules、repair 或 UI 行为。
- 不实现 persistent LangGraph SQLite/Postgres checkpointer。

审查：

- 子 agent 审查返回 GO。
- 审查确认 native command bridge 仍先读取 canonical graph state，study-level
  gate 会在 artifact 写入前 fail closed；dataset-level interrupt 不会被误判成
  study-level gate；reject bridge 继续复用 `review_code()`，没有分叉
  artifact/hash/fingerprint/approval 语义；公开 FastAPI/UI split-flow 默认路径不变。
- 接受一个非阻断建议并补充 dataset-level rollup 回归测试。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_native_interrupt_to_artifact_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_bridges_reject_without_execution_unlock tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_mismatched_interrupt_without_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_rejects_dataset_command_behind_study_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_code_from_command_allows_dataset_interrupt_as_current_rollup -v
Ran 5 tests in 0.258s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 276 tests in 28.657s - OK

python -B -m compileall -q src tests
git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.1 Terminal-Failure Command Bridge 切片

已完成：

- 新增 `GraphGateway.review_terminal_failure_from_command()`，作为
  terminal-failure triage 的 graph-native command bridge。
- 这个 bridge 会：
  - 读取 canonical `graph_state.json`；
  - 在接受 dataset-level terminal-failure command 前，先拒绝任何 open
    study-level interrupt；
  - 使用 `_assert_resume_command_matches_open_interrupt()` 证明 command 匹配当前
    dataset 的 `terminal_failure` interrupt；
  - 用 `TERMINAL_FAILURE_REVIEW_ACTIONS` 校验 action；
  - 委托现有 `review_terminal_failure()`，因此 terminal-failure triage 规则、
    next-action 映射、agent audit 写入和 input fingerprint 处理仍集中在
    `record_terminal_failure_review()`。
- 增加回归测试：
  - 匹配的 `terminal_failure` command 会进入正式 triage flow，并写入
    diagnosis/repair agent audit output；
  - dataset interrupt 不匹配时 fail closed，不写 `terminal_failure_review`；
  - 不支持的 command action fail closed，不改 triage state；
  - open study-level interrupt 后面的 dataset terminal-failure command 会先
    fail closed，不改 dataset triage state。

当前边界：

- 本切片只是 command bridge，还没有新增 DatasetGraph 原生 `terminal_failure`
  interrupt roundtrip。
- 不改变公开 FastAPI/UI split-flow 默认路径。
- 不实现 automatic repair、spec revision、retry execution 或 persistent
  LangGraph SQLite/Postgres checkpointer。

审查：

- 子 agent 审查返回 GO。
- 审查确认 bridge 会读取 canonical graph state，先阻断 study-level gate，校验当前
  dataset interrupt，然后委托现有 terminal-failure triage flow，没有复制或绕过
  规则。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_bridges_to_triage_flow tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_mismatched_interrupt_without_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_invalid_action_without_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_from_command_rejects_dataset_command_behind_study_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_review_terminal_failure_entrypoint_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_terminal_failure_review_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed -v
Ran 7 tests in 0.313s - OK
```

### 2026-06-01 - LG2.1 Optional Checkpointer Backend Guard 切片

已完成：

- 将显式 checkpointer backend 名称扩展为 `memory`、`sqlite`、`postgres`。
- 保持默认 `memory` backend 不变：
  - `build_checkpointer()` 仍返回 `InMemorySaver`；
  - `describe_checkpointer()` 仍报告
    `langgraph_checkpointer_persistent=false`、
    `native_interrupt_resume=false` 和
    `restart_recovery_source=graph_state_json`。
- 增加 optional backend fail-closed guard：
  - `sqlite` 需要可选 `langgraph-checkpoint-sqlite` 包；
  - `postgres` 需要可选 `langgraph-checkpoint-postgres` 包；
  - 即使可选包存在，本版本也会继续拒绝启用，直到 database lifecycle
    management 被接入。
- 接受子 agent 的低风险措辞建议：可选包检测先使用
  `importlib.util.find_spec()`，再 import，避免把传递依赖缺失误说成
  LangGraph checkpointer 包未安装。
- 增加回归测试，覆盖默认 memory、不可用 SQLite、不可用 Postgres、custom
  checkpointer metadata，以及 runtime persistence projection。

当前边界：

- 本切片不启用 persistent LangGraph checkpointing。
- 不改变公开 FastAPI/UI 行为、graph invocation 语义或 native interrupt 行为。
- 这是给后续 SQLite/Postgres checkpointer 实现准备的更安全配置边界。

审查：

- 子 agent 审查返回 GO。
- 审查确认 SQLite/Postgres fail closed，不会回退到 memory，也不会宣称
  persistence；默认产品路径仍是 memory-backed。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_sqlite_backend tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_postgres_backend tests.test_graph_gateway.GraphGatewayTests.test_custom_checkpointer_is_reported_without_persistence_claim tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary -v
Ran 5 tests in 0.039s - OK
```

### 2026-06-01 - LG2.1 Native Draft-Spec Gateway Roundtrip 切片

已完成：

- 新增内部 `GraphGateway.start_native_draft_spec_review()`：
  - 使用 DatasetGraph 原生 `draft_spec_review` interrupt 真正暂停；
  - 暂停后仍通过 shared
    `_record_draft_spec_generation_from_dataset_result()` 进入现有
    canonical `record_draft_spec_generation()` 状态和正式 draft-spec review
    interrupt；
  - 记录 `native_draft_spec_review_interrupt` metadata，并明确标记为
    `draft_spec_review_pilot_only` 边界。
- 新增内部 `GraphGateway.resume_native_draft_spec_review()`：
  - 在 native resume 前先检查 canonical `graph_state.json`；
  - 如果 study-level interrupt 仍然 open，会在消费 native DatasetGraph
    checkpoint 前 fail closed；
  - 只有 canonical dataset-level `draft_spec_review` interrupt 仍匹配时，
    才恢复 DatasetGraph native interrupt；
  - 将 DatasetGraph 返回的 human command 交给
    `review_draft_spec_from_command()`；
  - 因此正式 review artifact、draft-spec hash、approved-spec hash 和 input
    fingerprint 校验仍全部复用现有 gateway 校验。
- 新增 `GraphGateway.review_draft_spec_from_command()` 作为 draft-spec review
  的 native command bridge。它读取 canonical graph state，拒绝未解决的
  study-level gate，校验 dataset interrupt，然后委托现有
  `review_draft_spec()` artifact flow。
- 将 `finalize_inputs()` 的 DatasetGraph draft-spec 记录逻辑抽成 shared
  helper，保证 public split-flow 和 native pilot 共用同一条 canonical
  draft-spec generation 记录路径。
- 增加回归测试：
  - native approve roundtrip 会写正式
    `review/{dataset}_draft_spec_review.json`，并关闭 `draft_spec_review`
    interrupt；
  - native reject roundtrip 会写正式 reject artifact，并保持 dataset 锁定在
    draft-spec review；
  - native resume 在恢复 DatasetGraph checkpoint 前会先检查 canonical dataset
    open interrupt；
  - native resume 遇到 open study-level interrupt 时，会在 native checkpoint
    resume 前失败，不消费 checkpoint，也不写 review artifact。

当前边界：

- 本切片仍是内部 pilot，不改变公开 FastAPI/UI split-flow 默认路径。
- 不实现 persistent LangGraph SQLite/Postgres checkpointer。
- 不改变 LLM draft-spec 生成质量、R execution、compare、repair、static
  rules 或 Reference ADaM authority。

审查：

- 子 agent 第一次审查 NO-GO：study-level interrupt 校验发生在 native
  DatasetGraph resume 之后，可能先消费 in-memory native checkpoint，再由正式
  artifact flow fail closed。
- 已修复：在 `resume_native_draft_spec_review()` 中把 study-level interrupt
  guard 前移到 native resume 之前，并增加 `compile_dataset_graph` patch 回归
  测试，证明 study-level gate open 时不会调用 native resume。
- 子 agent 复审返回 GO。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_reject_roundtrip_keeps_review_locked tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_resume_requires_canonical_draft_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_draft_spec_review_resume_rejects_study_interrupt_before_native_resume -v
Ran 4 tests in 0.539s - OK
```

### 2026-06-01 - LG2.1 Native Code Review Gateway Roundtrip 切片

已完成：

- 新增内部 `GraphGateway.start_native_code_review()`：
  - 使用 DatasetGraph 的 native `code_review` interrupt 真正暂停；
  - 暂停后仍通过 shared `_record_code_generation_from_dataset_result()` 进入现有
    canonical `record_code_generation()` 状态和正式 code-review interrupt；
  - 记录 `native_code_review_interrupt` metadata，但继续明确当前默认
    LangGraph checkpointer 不是 persistent。
- 新增内部 `GraphGateway.resume_native_code_review()`：
  - 先恢复 DatasetGraph native interrupt；
  - 再把 DatasetGraph 返回的 human command 交给 `review_code_from_command()`；
  - 因此正式 review artifact、code hash、static-check hash、spec hash 和 input
    fingerprint 仍全部复用现有 gateway 校验。
- 将 `generate_code()` 的 DatasetGraph result 记录逻辑抽到 shared helper，保证
  public split-flow 和 native pilot 共用同一条 canonical code-generation 记录路径。
- 增加回归测试：
  - native approve roundtrip 会写正式 `review/{dataset}_code_review.json`，并关闭
    `code_review` interrupt；
  - native reject roundtrip 会写正式 reject artifact，但保持 execution locked；
  - native resume 在恢复 DatasetGraph 前会先检查 canonical open `code_review`
    interrupt，避免 canonical state 已变更时关闭 native checkpoint。

当前边界：

- 本切片仍是内部 pilot，不改变公开 FastAPI/UI split-flow 默认路径。
- 不实现 persistent LangGraph SQLite/Postgres checkpointer。
- 不改变 R execution、compare、repair、static rules 或 Reference ADaM authority。

审查：

- 子 agent 审查返回 GO。
- 审查确认没有引入第二套 code-review 状态；native interrupt 只是临时暂停点，
  最终仍桥回 `review_code_from_command()` -> `review_code()` ->
  `record_code_review()`。
- 审查确认 `generate_code()` 仍保持 public 默认路径，没有启用
  `native_code_review=True`，FastAPI/UI split-flow 未被翻转。
- 接受两条非阻断建议并修正：
  - native interrupt metadata boundary 改为按 pilot 显式标注；
  - `resume_native_code_review()` 在恢复 native checkpoint 前先做 canonical
    open-interrupt 预检。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_reject_roundtrip_keeps_execution_locked tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_resume_requires_canonical_code_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dependency_review_interrupt_roundtrip_persists_state -v
Ran 4 tests in 0.546s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 279 tests in 27.897s - OK

python -B -m compileall -q src tests
git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.1 本地 SQLite Checkpointer 接线切片

已完成：

- 扩展 `CheckpointerBundle`，加入明确的资源生命周期字段：
  `checkpoint_path`、`close_callback` 和 `close()`。
- 新增 `default_sqlite_checkpointer_path(study_dir, run_id)`，让 LangGraph
  runtime checkpoint store 使用单独、可预测的路径：
  `runs/{run_id}/langgraph_checkpoints.sqlite`。
- `build_checkpointer("sqlite", sqlite_path=...)` 现在会：
  - 要求显式传入 SQLite path；
  - 在未安装 `langgraph-checkpoint-sqlite` 时 fail closed；
  - 当可选包安装后，构建 `langgraph.checkpoint.sqlite.SqliteSaver`；
  - 只有真实 SQLite saver 启用时，才标记
    `langgraph_checkpointer_persistent=true` 和
    `native_interrupt_resume=true`；
  - 同时报告 `native_interrupt_resume_scope=native_pilot_interrupts_only`，避免被误读成
    完整产品循环已经能在重启后恢复。
- `GraphGateway` 现在可以通过
  `checkpointer_backend="sqlite"` 和 `sqlite_checkpointer_path=...` 显式构建；
  公开默认路径仍保持 `memory`。
- 新增可选依赖声明：
  `adam-agent-studio[sqlite-checkpoint]` 会安装
  `langgraph-checkpoint-sqlite>=3.0.3,<3.1`。
- 增加测试覆盖：
  - SQLite path 必须显式传入；
  - 可选包缺失时 fail closed；
  - `langgraph_checkpoints.sqlite` 与产品审计 ledger
    `graph_checkpoints.sqlite` 分开；
  - 可选包存在时的 SQLite metadata；
  - restart-style readback：重新创建一个 `GraphGateway` 后，可以从同一个
    SQLite checkpoint 读回 native dependency-review interrupt。

当前边界：

- 公开 FastAPI/UI 产品路径仍默认使用 in-memory checkpointer。
- SQLite checkpointer 入口已经接好并做过 smoke 验证，但完整产品循环还不是一个
  可在进程重启后恢复所有 gate 的单次 native LangGraph run。
- 这个本地 SQLite saver 适合本地单进程恢复测试，不是生产多 worker 部署方案。

审查：

- 子 agent 审查返回 GO。
- 已接受两个中等风险建议：
  - 增加 `native_interrupt_resume_scope`，避免暗示 full product-loop native
    resume 已完成；
  - 将“可选包缺失”测试改为 patch `find_spec`，即使 CI 安装了 SQLite extra，
    该测试仍稳定。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_defaults_to_nonpersistent_memory tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_requires_sqlite_path tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_sqlite_backend tests.test_graph_gateway.GraphGatewayTests.test_checkpointing_boundary_rejects_unavailable_postgres_backend tests.test_graph_gateway.GraphGatewayTests.test_default_sqlite_checkpointer_path_is_separate_from_product_ledger tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_metadata_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary -v
Ran 7 tests in 0.044s - OK (skipped=1)

$env:PYTHONPATH = ".tmp_tests\sqlite_pkg;src"
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_metadata_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_sqlite_checkpointer_can_read_interrupt_after_new_gateway_when_package_available -v
Ran 2 tests in 0.078s - OK
```

### 2026-06-01 - LG2.1 Native Terminal-Failure Gateway Roundtrip 切片

已完成：

- 新增 DatasetGraph 原生 terminal-failure review 试点：
  - 新增 `wait_for_terminal_failure_review` 节点；
  - 新增 `native_terminal_failure_review`、
    `native_terminal_failure_review_status` 和
    `native_terminal_failure_review_resume` state 字段；
  - `execute_approved_code` 只有在执行失败并且显式打开 pilot flag 时，才会路由到
    这个 native interrupt。
- 新增 `GraphGateway.start_native_terminal_failure_review()`：
  - 复用现有 dependency gate 和 approved-code preflight；
  - 使用 `native_terminal_failure_review=True` 运行 DatasetGraph；
  - 通过现有 canonical `record_execution()` 路径记录失败执行；
  - 写入 `native_terminal_failure_review_interrupt` metadata，boundary 标记为
    `terminal_failure_review_pilot_only`。
- 新增 `GraphGateway.resume_native_terminal_failure_review()`：
  - 在恢复 native checkpoint 前先检查 canonical `graph_state.json`；
  - 如果 study-level interrupt 仍 open，或者 dataset 已不再等待
    `terminal_failure`，会 fail closed；
  - 只有 canonical guard 通过后，才恢复 DatasetGraph checkpoint；
  - 将 DatasetGraph 返回的 native human command 桥回现有
    `review_terminal_failure_from_command()` -> `review_terminal_failure()` ->
    `record_terminal_failure_review()` 流程。
- 增加回归测试：
  - DatasetGraph 可以在 native `terminal_failure` interrupt 暂停并恢复；
  - gateway roundtrip 会写正式 terminal-failure triage 状态和
    diagnosis/repair agent audit；
  - canonical dataset interrupt 已变化时，会在消费 DatasetGraph checkpoint 之前阻断
    native resume；
  - study-level interrupt 仍 open 时，也会在消费 DatasetGraph checkpoint 之前阻断
    native resume。

当前边界：

- 这仍是内部 pilot。公开 FastAPI/UI 执行路径没有翻转为 native terminal-failure
  resume。
- 它不自动执行 repair、revise-spec、retry execution 或 continue other datasets。
  它只通过现有 gateway flow 记录人工 triage 决策和下一步受控动作。
- 不改变 R 执行质量、LLM 生成、compare、static rules 或生产沙盒能力。

审查：

- 子 agent 审查返回 GO，没有 must-fix 项。
- 审查确认当前边界成立：DatasetGraph 只拥有 native pause/resume 试点，
  GraphGateway 仍然是 canonical 产品状态转换边界。
- 非阻断的后续加固建议：
  - 增加“pilot 执行但意外没有 interrupt”场景的保护测试；
  - 将 native interrupt action list 从共享 terminal-failure action contract
    派生，降低后续漂移风险。

验证：

```text
python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_terminal_failure_review_interrupt_can_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_requires_canonical_terminal_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_rejects_study_interrupt_before_native_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes -v
Ran 5 tests in 0.474s - OK

python -B -m compileall -q src tests

git diff --check
Exited 0; CRLF warnings only.

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 296 tests in 28.761s - OK (skipped=2)
```

### 2026-06-01 - LG2.1 Native Terminal-Failure 加固切片

已完成：

- 处理上一轮子 agent 审查给出的非阻断加固建议。
- 将 terminal-failure 人工动作合同移到
  `src/adam_agent/graph/terminal_failure_actions.py`，让 DatasetGraph native
  interrupt 和 GraphGateway 校验共用同一组 action names。
- 增加 internal pilot 启动路径的 fail-closed 回归测试：
  - 如果 `start_native_terminal_failure_review()` 调用 DatasetGraph 后，图没有停在
    native `terminal_failure` interrupt，gateway 会在读取 native snapshot 前报错；
  - canonical `graph_state.json` 不会被污染为 terminal-failure triage 或 native
    interrupt metadata。

当前边界：

- 抽出 action contract 不代表 DatasetGraph 成为产品状态写入者。失败执行记录和正式
  triage 仍由 GraphGateway 写入 canonical state。
- 这仍是 internal pilot guard，不是公开 UI/API 行为变更。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_start_fails_closed_without_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_requires_canonical_terminal_interrupt tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_resume_rejects_study_interrupt_before_native_resume tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_terminal_failure_review_interrupt_can_resume -v
Ran 5 tests in 0.452s - OK

python -B -m compileall -q src tests

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 297 tests in 28.550s - OK (skipped=2)

git diff --check
Exited 0; CRLF warnings only.
```

### 2026-06-01 - LG2.2 Native Dataset Product Loop 试点切片

已完成：

- 新增内部执行模式 `graph_product_full_loop`。
- 新增 `GraphGateway.start_native_dataset_product_loop()`，覆盖最短的安全 dataset
  native 产品路径：
  - 准备 product context；
  - 使用已批准的 `input_spec` 或已批准的 draft spec；
  - 生成需要审核的 R code；
  - 停在 DatasetGraph 原生 `code_review` interrupt。
- 新增 `GraphGateway.resume_native_dataset_product_loop()`：
  - 恢复 native code-review checkpoint；
  - 将 native human command 桥回现有正式 `review_code_from_command()` 路径，
    因此 code-review JSON、hash、canonical state 校验仍然会执行；
  - approve 后可选调用现有 `execute_approved_code()`。
- 增加回归测试：
  - approve path：native loop 写正式 code review，然后通过现有 Gateway execution
    path 记录执行；
  - reject path：拒绝 code review 不会执行，并让 dataset 留在 code-review gate。

当前边界：

- 这是 internal pilot，不是公开 UI/API 默认路径。
- 当前只覆盖 input-spec-ready 路径。缺 spec 的 draft-spec review、approved-draft-spec
  continuation、repair、revise-spec、多 dataset StudyGraph 编排仍是后续切片。
- DatasetGraph 仍不直接写产品状态。review artifact 和 execution 的正式状态转换仍由
  GraphGateway 拥有。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_roundtrip_persists_formal_review_artifact tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_code_review_reject_roundtrip_keeps_execution_locked -v
Ran 4 tests in 0.794s - OK

python -B -m unittest tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_graph_does_not_include_legacy_stub_nodes tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_product_generate_code_uses_input_spec_and_stops_for_review tests.test_graph_smoke.GraphSmokeTests.test_dataset_graph_native_code_review_interrupt_can_resume -v
Ran 3 tests in 0.141s - OK

python -B -m compileall -q src tests
```

### 2026-06-01 - LG2.2 Native Dataset Loop 缺 Spec 切片

已完成：

- 扩展内部 `graph_product_full_loop` 试点，让它先走 spec 闸门，再走 code 闸门：
  - 如果有 approved input spec 或 approved draft spec，继续到 native
    `code_review`；
  - 如果没有 approved spec，先生成需要审核的 draft spec，并停在 native
    `draft_spec_review`。
- 新增 `GraphGateway.resume_native_dataset_product_loop_draft_spec()`：
  - 通过现有正式 draft-spec review artifact flow 恢复 native draft-spec
    interrupt；
  - 审核通过后，重新进入 Gateway 拥有的 native dataset 产品路径，并继续到
    native `code_review`；
  - 审核拒绝时，不生成 R code。
- 保持边界不变：这仍然是 internal pilot；draft review、code review 和 execution
  的正式状态仍由 GraphGateway 统一拥有。

当前边界：

- 公开 FastAPI/UI 仍使用现有 split-flow endpoints。
- 新 continuation 只覆盖 draft-spec approval 到 code review。code review approval
  和可选执行继续通过 `resume_native_dataset_product_loop()`。
- repair/revise-spec 自动路由和 StudyGraph 多 dataset native 编排仍是后续切片。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_missing_spec_stops_at_draft_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_draft_reject_does_not_generate_code -v
Ran 3 tests in 0.533s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_reject_does_not_execute -v
Ran 2 tests in 0.413s - OK
```

### 2026-06-01 - LG2.2 Native Loop Terminal-Failure Follow-Up 加固切片

已完成：

- 为 internal native dataset loop 补充 terminal-failure follow-up focused
  coverage：
  - `repair_code` 允许 native loop 重新生成 code，并停在 native `code_review`；
  - `revise_spec` 会先回到 draft-spec generation/review，不能继续复用旧 code。
- 收紧 `record_draft_spec_generation()`：当 terminal-failure `revise_spec`
  follow-up 被消费时，旧的 code state 会标记为 `stale`，避免旧 approved code
  看起来仍然可执行。

当前边界：

- 这只是 graph-state hardening，不新增公开 UI/API 操作，也不做自动 repair routing。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_respects_repair_code_terminal_followup tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_routes_revise_spec_followup_to_draft_review tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_with_approved_draft_spec_generates_new_draft_and_clears_old_review tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_revise_spec_requires_finalize_before_regenerating_code -v
Ran 4 tests in 1.550s - OK
```

### 2026-06-01 - LG2.3 Native Study Product Loop 分发试点切片

已完成：

- 新增内部 `GraphGateway.start_native_study_product_loop()`：
  - 先刷新 graph-owned dependency plan；
  - 只分发当前 dependency gate 允许继续的 runnable datasets；
  - 每个 dataset 仍调用现有 `start_native_dataset_product_loop()`，所以 draft-spec
    review、code review、artifact/hash 校验和状态写入仍走同一条 Gateway 边界；
  - 不自动批准 draft spec，不自动批准 code，也不自动执行 R。
- 新增 `GraphGatewayNativeStudyLoopResult`，返回：
  - `started_datasets`
  - per-dataset loop result
  - `blocked_datasets`
  - graph-owned `review_queue`
- 重复启动时会跳过已有产品进度的 dataset，避免覆盖已经打开的 draft/code review。
- 收紧 dependency warning 分流：
  - 真正的 dependency conflict/warning 仍会阻断 product step；
  - “某 dataset 缺少 input_spec，需要后续 draft-spec/code review 验证”的 spec-gap
    warning 不再挡住其他 dataset，也不挡住该 dataset 进入 draft-spec review。
  - spec-gap warning 现在有结构化 code：`input_spec_gap_no_default_dependency`，
    Gateway 只按 code/dataset 判定，不再靠英文文案片段决定安全行为。
- 根据子 agent NO-GO 审查补强：
  - spec-gap warning 被转交给 dataset review gate 后，会清掉 study-level
    dependency-review interrupt，避免后续 code/draft review 被 study interrupt 卡住；
  - 新增测试验证 mixed spec-gap 场景下，启动后可以继续 resume `code_review` 和
    `draft_spec_review`；
  - 补充 restart/re-entry 测试：当同一个 run 已经有 draft/code review 进度时，
    重新启动 study loop 不会重新留下 study-level dependency interrupt；
  - warning 分流改为 fail closed：只要 warning 缺少结构化 record、record 数量不一致、
    code 未知或 spec-gap record 缺 dataset，就继续阻断，不静默进入产品步骤；
  - 真正 dependency warning/conflict 仍由现有测试证明会阻断 product step。
- 第二轮子 agent 审查结论为 GO；它提出的 message/record 文案一致性、更细的
  unknown-code 参数化测试属于后续加固项，不阻塞该 internal pilot 切片。

当前边界：

- 这是 internal pilot，还没有暴露为公开 FastAPI/UI 主入口。
- 多 dataset 现在可以被一次启动到各自的 review gate，但后续审核和执行仍通过现有
  per-dataset resume/execute 路径推进。
- 依赖缺失仍 fail closed：如果下游需要未提供/未批准生成的上游 ADaM，study loop
  不会静默启动目标 dataset。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_preserves_mixed_spec_gates tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_does_not_start_dependency_blocked_targets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart -v
Ran 4 tests in 0.705s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_preserves_mixed_spec_gates tests.test_graph_smoke.GraphSmokeTests.test_input_spec_present_but_missing_target_spec_warns_without_adsl_fallback tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_dependency_warning -v
Ran 3 tests in 0.489s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_finalize_inputs_blocks_dependency_warning tests.test_graph_gateway.GraphGatewayTests.test_gateway_finalize_inputs_records_review_required_draft_spec -v
Ran 2 tests in 0.192s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_input_spec_executes_after_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_missing_spec_stops_at_draft_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_draft_approval_continues_to_code_review -v
Ran 3 tests in 0.605s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 310 tests in 32.907s - OK (skipped=2)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume Queue 结构化 Read-Model 切片

已完成：

- 在 `native_resume` read-model 中新增结构化字段：
  - `has_queue_items`
  - `queue_item_count`
- 这两个字段完全从 `interrupt_queue` 派生，用于告诉 UI/自动化客户端当前是否有可展示的
  dataset review/resume 队列项，避免依赖 `message` 文案做判断。
- 新增/更新测试覆盖：
  - 默认 memory backend 且无 queue：`available == false`，
    `has_queue_items == false`，count 为 0；
  - 默认 memory backend 但有 code-review queue：`available == false`，
    `has_queue_items == true`，count 为 1，且 queue item
    `can_resume == false`、`resume_endpoint == null`；
  - durable read-model 被 study-level gate 或 stale plan 阻挡：
    `available == true`，但 `has_queue_items == false`，count 为 0；
  - 可选 SQLite checkpointer 如果可用，resumable queue 分支也断言 count 为 1。

边界：

- 这是 read-model 字段补充，不改变 native resume endpoint、split-flow review
  endpoint、GraphGateway 状态转换、LLM/R execution 或 repair/spec-revision。
- `available` 继续表示 durable native resume 能力是否启用；`has_queue_items` 和
  `queue_item_count` 只表示当前 progress 中是否有 visible queue items。单个 item 仍通过
  `can_resume` 和 `resume_endpoint` 表示是否能调用 native resume endpoint。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，第一次只读审查结论：GO，但指出
  `actionable_queue_available/actionable_queue_count` 字段名可能让人误以为 native
  endpoint 一定可调用。
- 已吸收建议：改名为 `has_queue_items` / `queue_item_count`。
- 复审结论：GO。
- 它确认：
  - `has_queue_items == true` 只说明 `interrupt_queue` 有可展示项，不暗示 native
    endpoint 可调用；
  - 是否能走 native resume 仍由 top-level `available` 和 item-level
    `can_resume/resume_endpoint` 决定；
  - `RunProgressResponse.native_resume: dict[str, Any]` 对该 read-model 扩展足够，
    不需要 schema 改动。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume tests.test_graph_gateway.GraphGatewayTests.test_progress_durable_native_resume_queue_still_respects_study_gate tests.test_graph_gateway.GraphGatewayTests.test_progress_durable_native_resume_queue_still_respects_stale_plan tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_queue_as_resumable_when_package_available -v
Ran 6 tests in 0.210s - OK (skipped=2)

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 283 tests in 29.520s - OK (skipped=4)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Durable Native Resume Queue 文案与覆盖切片

已完成：

- 收紧 durable native resume 可用但 queue 为空时的 read-model 文案：
  - 旧文案只说 “no dataset gate is waiting”；
  - 新文案说明 “no actionable dataset gate is waiting”，并提示先处理 visible
    study-level 或 dependency gate。
- 新增不依赖可选 SQLite 包的 durable read-model 回归测试：
  - 通过 `runtime_persistence_extra` 模拟 `native_interrupt_resume == true`；
  - study-level `dependency_review` open 时，`native_resume.available == true`，
    但 `interrupt_queue == []`；
  - dependency plan stale 时，dataset `code_review` 不进入 `interrupt_queue`，
    dataset available actions 为空；
  - message 明确提醒先处理 study/dependency gate。

边界：

- 这是 read-model 文案和测试覆盖切片，不启用真实 SQLite/Postgres
  checkpointer，不新增 UI 按钮，不改变 split-flow review endpoint，不改变 LLM/R
  execution 或 repair/spec-revision。
- `runtime_persistence_extra` 只用于测试 durable read-model 分支，避免本地环境是否安装
  `langgraph-checkpoint-sqlite` 影响这个边界测试。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 产品代码只改 durable 可用但 queue 为空时的提示语，语义比旧文案更准确；
  - `runtime_persistence_extra` 只验证已持久化 graph state 的 progress read-model
    投影，不会改变 service 层真实 checkpointer 构造；
  - 默认 memory 的 `/native-resume` endpoint 不会因此变可用；
  - 现有 SQLite optional 测试已经覆盖真实 durable checkpointer 的 resumable queue
    分支，本切片不需要依赖可选包。
- 非阻断建议：未来如果 UI 直接消费该文案，可把 “available” 和 “actionable queue
  empty” 拆成结构化字段，避免前端用 message 做判断。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_progress_native_resume_queue_respects_study_level_gate tests.test_graph_gateway.GraphGatewayTests.test_progress_native_resume_queue_respects_blocked_dataset_gate tests.test_graph_gateway.GraphGatewayTests.test_progress_durable_native_resume_queue_still_respects_study_gate tests.test_graph_gateway.GraphGatewayTests.test_progress_durable_native_resume_queue_still_respects_stale_plan tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume -v
Ran 5 tests in 0.225s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 283 tests in 29.980s - OK (skipped=4)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.8 Dataset Artifact Read-Model Guard 切片

已完成：

- 收紧 graph-owned run 的 dataset 级 read model：
  - 只要 `graph_state.json` 存在，`review-summary` 的 dataset 列表只来自 canonical
    graph state，不再从 `outputs/*.csv`、旧 manifest 或 workflow projection 反推
    额外 dataset；
  - generated table/download 之外，code、validation report、compare report、
    diagnostics 和 parsed LLM response 这些 dataset artifact 也必须先在 canonical
    dataset state 中存在，才会被 review/download/read endpoint 暴露；
  - 没有 `graph_state.json` 的 legacy/artifact-only run 仍保留旧 fallback 行为。
- 收紧 JSON artifact 读取：
  - `/datasets/{dataset}/validation` 和 `/artifacts/read` 读取
    `validation/*_validation_report.json`、`diagnostics/*_failure_report.json`、
    `compare/*_compare_report.json`、`llm/*_parsed_response.json` 时，如果 run 已经有
    canonical graph state，就必须能在对应 dataset state 中找到该 artifact 或
    path；
  - graph-owned run 中不存在于 canonical state 的 stale dataset artifact 会 fail
    closed。
- `advanced_artifacts` 在 graph-owned run 中改为从 study/dataset graph artifacts
  构建，不再扫描 validation/diagnostics/llm 目录把陈旧文件显示成当前产物。
- code generation 的 assumptions、risk points、used inputs 和 expected outputs
  现在会写入 canonical `code_state`，这样 review-summary 可以从 graph state 恢复
  审核材料，而不是偷读旧的 parsed-response JSON。

边界：

- 这个切片只修 graph-owned read-model ownership，不改变真实 LLM 生成、R 执行、
  compare 算法或 UI 布局。
- Legacy run 仍可以使用 artifact fallback；但一旦 run 有 canonical
  `graph_state.json`，产品级 dataset artifact 必须由 graph state 授权。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_graph_state_blocks_generated_reads_for_dataset_absent_from_canonical_state tests.test_api_phase8.Phase8ApiTests.test_graph_state_allows_recorded_validation_artifact_read tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_validation_over_stale_validation_file tests.test_api_phase8.Phase8ApiTests.test_review_summary_ignores_graph_state_output_path_outside_run_dir tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection -v
Ran 5 tests in 0.726s - OK

python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
Ran 268 tests in 27.534s - OK (skipped=3)

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.8 Run-Level Artifact Read-Model Guard 切片

已完成：

- 收紧 graph-owned run 的 study/run 级 artifact read model：
  - 只要 run 已经有 canonical `graph_state.json`，Advanced artifacts 不再展示目录里
    偶然存在的 `planning/dependency_plan.json`、`planning/dependency_review.md`
    或其他 run-level 文件；
  - run-level artifact 必须先登记在 canonical `StudyRunState.artifacts`，才会被
    review-summary 的 Advanced 面板展示；
  - `/runs/{run_id}/artifacts/read` 在 graph-owned run 中也改为 fail closed：未登记在
    `StudyRunState.artifacts` 或对应 dataset state 中的 JSON artifact 不能被读取。
- `GraphGateway._persist_graph_state()` 现在会把 dependency plan 和 dependency
  review markdown 作为 graph-owned planning artifacts 写入 `runs/{run_id}/planning/`，
  并登记到 canonical study artifacts。
- Legacy/artifact-only run 仍保留旧的目录 fallback；这个收紧只在
  `graph_state.json` 已存在时生效。

边界：

- 这个切片不改变 dependency planning 逻辑、真实 LLM 生成、R 执行、compare 算法或
  UI 布局。
- 它只解决“目录里有文件”和“当前 graph state 授权该文件”之间的权威边界，防止旧
  planning/audit 文件被误展示成当前运行事实。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_graph_state_advanced_artifacts_ignore_unrecorded_run_files tests.test_api_phase8.Phase8ApiTests.test_graph_state_records_dependency_plan_as_run_artifact -v
Ran 2 tests in 0.243s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection -v
Ran 1 test in 0.046s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_planning_artifacts_are_upserted tests.test_graph_gateway.GraphGatewayTests.test_gateway_dependency_plan_writes_consistent_workflow_projection -v
Ran 2 tests in 0.082s - OK

python -B -m unittest tests.test_api_phase8 tests.test_graph_gateway -v
Ran 271 tests in 28.959s - OK (skipped=3)

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.8 Review Summary 损坏 Graph Fail-Closed 切片

已完成：

- 收紧 `/runs/{run_id}/review-summary` 的 read-model 权威来源。
- 如果 `graph_state.json` 不存在，review-summary 仍可为旧 compatibility run 使用
  `workflow_state.json` fallback。
- 如果 `graph_state.json` 已存在但无法解析或无法通过 schema 校验，review-summary
  现在会 fail closed，不再静默回退到 `workflow_state.json`。
- 这样可以防止过期 compatibility projection 掩盖损坏的 canonical graph state。

当前边界：

- 这是 read-model authority 加固切片。
- 不修改 graph state、workflow projection、compare artifacts 或 R execution。
- 旧 run-to-completion compatibility summaries 在没有 canonical graph state 时仍然可用。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。

子 agent 审查前验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_review_summary_fails_closed_when_existing_graph_state_is_corrupt -v
Ran 1 test in 0.062s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_review_summary_surfaces_not_real_quality_from_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_state_without_workflow_projection tests.test_api_phase8.Phase8ApiTests.test_review_summary_prefers_graph_validation_over_stale_validation_file -v
Ran 3 tests in 0.282s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_layer_reads_graph_state_only_for_explicit_read_models tests.test_api_phase8.Phase8ApiTests.test_review_summary_read_model_helpers_do_not_record_compare -v
Ran 2 tests in 0.044s - OK
```

### 2026-06-01 - LG2.7 缺失 Progress Target Fail-Closed 切片

已完成：

- 收紧 UI 主动作可用性：当 graph progress 已经加载，但当前已纳入计划的 target
  没有对应 dataset progress 行时，UI 不再回落到浏览器本地缓存。
- 以下按钮在该状态下都会 fail closed：
  - `Finalize Inputs / Draft Spec`
  - `Approve Draft Spec`
  - `Generate R Code`
  - `Approve Code`
  - `Run Approved Code`
- UI 会明确提示：
  `Graph progress has no dataset step for {target}. Refresh graph state or
  prepare the dependency plan again before continuing.`
- 只有在没有 graph progress 的旧前置状态下，才保留本地 fallback，避免破坏
  pre-progress 浏览器流程。

当前边界：

- 这是 UI read-model 加固切片。
- 不改变 FastAPI endpoints、`GraphGateway` 状态转换、dependency planning 或 R
  execution。
- `Start Runnable Datasets` 仍是 study-level 动作，继续从 selected targets 和 graph
  progress rows 判断是否可启动。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 五个预期 per-target 动作都受 `graphProgressMissingTarget` 保护；
  - legacy fallback 只在 `state.runProgress` 缺失时保留；
  - `Start Runnable Datasets` 仍是 study-level；
  - 点击处理函数会在 API 调用前重新检查 `actionAvailability()`，所以该 mismatch
    状态会同时拦住按钮可用性和直接 handler 执行。

子 agent 审查前验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_fail_closed_when_progress_lacks_active_target tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state -v
Ran 4 tests in 0.244s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_next_action_text_ignores_local_cache_when_progress_loaded tests.test_api_phase8.Phase8ApiTests.test_index_terminal_failure_panel_requires_graph_owned_actions -v
Ran 2 tests in 0.160s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 138 tests in 17.622s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.3 Native Study Loop 运行时依赖输出闸门切片

已完成：

- 收紧 `GraphGateway.start_native_study_product_loop()` 的分发逻辑：当用户批准
  “由系统生成上游依赖”时，native study loop 只会先启动上游 dataset；在同一轮里
  不会把下游 ADaM 也直接启动。下游必须等 canonical graph state 中出现真实、
  graph-authorized 的上游 run output artifact。
- 新增 native study-loop helper：
  - 从 canonical `StudyRunState.dependency_plan` 读取目标 dataset 的依赖关系；
  - 从 canonical `StudyRunState.dependency_resolution` 判断是否已有可用 runtime
    dependency artifact。
- 分发闸门现在要求 dependency record 必须是 `run_output`，且同时带有 artifact
  path 和 artifact hash，才允许启动下游 product work。它还会校验上游 dataset
  的 canonical graph state：
  - output quality 必须允许作为 runtime dependency；
  - dataset 不能是 `completed_stub`、terminal failure、not-real derivation，也不能
    被标记为 partial output unusable；
  - graph artifacts 中必须有匹配的 `output_adam` artifact；
  - artifact path/hash 必须和 dependency record 一致，并且文件仍然存在。
  这样即使 dependency_resolution stale、残缺、被错误写成 available，或只有非运行时
  证据，也会 fail closed。
- Reference ADaM 仍不能满足 runtime dependency：它可以作为 compare、输出形态、
  依赖可用性证据，但不能被当成下游 ADaM 代码运行时真正需要的上游生成结果。
- `skipped_datasets` read model 现在会解释“可运行但正在等待上游运行时输出”的
  dataset：
  - `reason`: `waiting_for_runtime_dependency_output`
  - `next_action`: `complete_dependency_output`
  - `blocked_by`: 逗号分隔的上游 ADaM datasets
- 新增回归测试证明两步行为：
  - 第一次 native study loop 请求 `ADAE`，并批准系统生成 `ADSL` 时，只启动
    `ADSL`，`ADAE` 显示为等待 runtime dependency output；
  - 当 graph state 中已经有 completed、非 stub 的 ADSL output artifact 后，
    第二次 native study loop 才启动 `ADAE`，并把 ADSL 的 `run_output` artifact
    传入 ADAE code-generation context。
  - 缺少 artifact hash 的残缺 dependency record 不能打开下游分发；
  - `reference_adam`、`completed_stub`、terminal failure、缺少 graph `output_adam`
    artifact，以及 not-real/mock output quality 都会被 runtime output gate 拒绝。

当前边界：

- 这个切片只改变 native study-loop dispatch，不改变通用 dependency planner、
  legacy stub graph，也不改变显式 per-dataset review/execute 闸门。
- native study loop 仍然停在人工审核闸门；它不会自动批准 draft spec、不会自动
  批准 generated code，也不会自动执行 R。
- 用户允许系统生成上游依赖，只代表允许启动上游 dataset；这不是“上游运行时输出
  已经存在”的证明。

验证：

```text
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 273 tests in 29.974s - OK (skipped=3)
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，第一次只读审查结论：NO-GO。
- 问题：第一版实现过度相信 `dependency_resolution`。如果有 stale/manual 的
  `run_output available` 记录，可能在没有证明上游 dataset canonical graph state
  是真实、非 stub、非 terminal、且有匹配 graph-owned artifact 的情况下打开下游
  dispatch。
- 已吸收修复：
  - runtime dependency gate 现在会把 dependency resolution 与
    `state.datasets[required_dataset]` 交叉校验；
  - 使用 `dataset_output_quality(...).runtime_dependency_eligible` 判断上游输出质量；
  - 要求 `execution_state.output_path`、graph `output_adam` artifact path/hash 和磁盘
    文件都匹配；
  - 测试覆盖缺 hash、`reference_adam`、`completed_stub`、terminal failure、缺少
    graph output artifact。
- 复审结论：GO。
- 非阻断建议：
  - 后续可补 run-relative path 和 absolute artifact path 等价性的覆盖；
  - 后续可把 `skipped_datasets.blocked_by` 细化为只列仍缺失的依赖，而不是所有声明依赖。

### 2026-06-01 - LG2.3 Runtime Dependency Artifact 路径等价性切片

已完成：

- 吸收上一段 runtime dependency gate 切片里子 agent 的非阻断建议。
- native study-loop runtime dependency gate 现在会先基于 graph run directory 解析
  dependency artifact path，再比较路径。
- 当以下形式都指向同一个 `runs/{run_id}` 下的文件时，系统会把它们当成同一个
  artifact：
  - graph artifact 中记录的绝对路径；
  - `outputs/{dataset}.csv`；
  - `runs/{run_id}/outputs/{dataset}.csv`。
- 如果路径解析到 run directory 外、文件不存在、hash 不匹配，或者 graph-owned
  `output_adam` artifact 缺失，闸门仍然 fail closed。
- 新增 focused regression，证明 run-relative dependency record 可以匹配绝对路径
  graph-owned `output_adam` artifact；错误的相对路径仍会被拒绝。
- 新增 outside-path regression，证明 dependency record 即使指向另一个目录下相同
  `runs/{run_id}` 后缀，或试图用相对路径逃出 graph run directory，也不能满足
  runtime dependency gate。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_downstream_after_graph_output_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_requires_run_output_artifact_hash tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_treats_run_relative_and_absolute_paths_as_same -v
Ran 3 tests in 0.256s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_treats_run_relative_and_absolute_paths_as_same tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_rejects_paths_outside_graph_run_dir -v
Ran 2 tests in 0.012s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 275 tests in 29.197s - OK (skipped=3)
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 本切片已吸收非阻断建议：补 outside-path regression，锁住 run-directory 边界。
- 剩余非阻断想法：如果后续要继续加固，可以把真实 `study_dir/runs/{run_id}` 传入
  runtime dependency gate，而不是完全从 graph artifact path 推导。

### 2026-06-01 - LG2.3 显式 Run-Dir Runtime Dependency Gate 切片

已完成：

- 吸收上一段审查剩余的非阻断加固建议。
- `GraphGateway.start_native_study_product_loop()` 现在一次性计算真实
  `study_dir/runs/{run_id}`，并传入：
  - native study-loop dispatch ordering；
  - per-dataset start eligibility；
  - runtime dependency output checks；
  - `skipped_datasets` read model 构造。
- runtime dependency gate 不再依赖从 artifact path 反推出 run directory。artifact
  path 仍会被归一化，但可信边界现在是 gateway 传入的 graph-owned run directory。
- 现有 path-equivalence 和 outside-path 回归测试现在覆盖的也是产品路径使用的显式
  `run_dir` 边界。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_downstream_after_graph_output_dependency tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_treats_run_relative_and_absolute_paths_as_same tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_rejects_paths_outside_graph_run_dir -v
Ran 3 tests in 0.259s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK，仅有预期内 CRLF working-copy warnings

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 275 tests in 30.141s - OK (skipped=3)
```

子 agent 审查：

- GO。
- 非阻断建议：后续可把 `_resolve_run_artifact_path()` 的 path equivalence 和
  containment 行为拆成一个独立 helper 单测矩阵，覆盖 Windows 反斜杠、
  run-relative path、output-relative path、absolute path 和 `..` 逃逸尝试。

### 2026-06-01 - LG2.3 Run Artifact Path Helper Matrix 切片

已完成：

- 为 `_resolve_run_artifact_path()` 增加 helper 级 focused tests，不再只依赖
  native study-loop 集成测试兜底。
- 覆盖等价的当前 run artifact path：
  - `outputs/adsl.csv`；
  - Windows 风格 `outputs\adsl.csv`；
  - `runs/{run_id}/outputs/adsl.csv`；
  - Windows 风格 `runs\{run_id}\outputs\adsl.csv`；
  - graph-owned run directory 内部的 absolute path。
- 覆盖应拒绝的路径：
  - 空值；
  - `..` 逃逸尝试；
  - graph-owned run directory 外部的 absolute path；
  - 显式 `runs/{other_run_id}/...` path；
  - 带前缀的 `.../runs/{run_id}/...` path。
- 新测试矩阵暴露了一个语义边界：`runs/{other_run_id}/...` 之前会被当成当前
  run directory 下的普通相对路径。`_resolve_run_artifact_path()` 现在 fail
  closed：只要相对 artifact path 显式包含 `runs/...` marker，该 run id 必须
  等于当前 graph run directory，并且 `runs/...` marker 必须是路径的第一段。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_resolve_run_artifact_path_normalizes_equivalent_run_scoped_paths tests.test_graph_gateway.GraphGatewayTests.test_resolve_run_artifact_path_rejects_empty_or_outside_run_paths tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_treats_run_relative_and_absolute_paths_as_same tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_loop_dependency_output_gate_rejects_paths_outside_graph_run_dir -v
Ran 4 tests in 0.019s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK，仅有预期内 CRLF working-copy warnings

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 277 tests in 30.106s - OK (skipped=3)
```

子 agent 审查：

- GO。
- 提交前吸收非阻断建议：带前缀的 `.../runs/{run_id}/...` path 现在 fail
  closed，不再裁剪到内部 run marker。

### 2026-06-01 - LG2.7 Terminal-Failure Action Read-Model 切片

已完成：

- 收紧本地 UI 的 terminal-failure triage panel：后续动作按钮只从 graph-owned
  `available_actions` 渲染。
- 移除 `terminalFailurePanel()` 里的浏览器本地默认 action fallback。现在即使浏览器
  里还有旧的 execution failure cache，只要 graph progress 不可用，就不能自己打开
  retry / repair / revise / skip 等按钮。
- 如果 graph progress 表明 dataset 正在等待 `review_terminal_failure`，但
  `available_actions` 还没有加载，panel 只显示刷新/read-model 提示，不创建本地动作按钮。
- 后端 `GraphGateway` 的 terminal-failure action contract 仍是唯一动作来源。

边界：

- 本切片只改变 UI read-model gate 和测试。
- 不改变 terminal-failure 后端路由、repair/spec-revision 行为、native resume 或
  本地 R 执行。
- 自动 repair/spec-revision 闭环仍是后续工作。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认浏览器本地 terminal-failure cache 不再打开动作按钮，graph-owned
  `available_actions` 是唯一动作来源，human review queue 对这些动作仍只是展示。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions tests.test_api_phase8.Phase8ApiTests.test_index_terminal_failure_panel_requires_graph_owned_actions -v
Ran 2 tests in 0.122s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 137 tests in 17.296s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.3 Native Study Loop 保留进度解释切片

已完成：

- `GraphGateway.start_native_study_product_loop()` 现在会把“本次没有重新启动，但已有
  graph progress 的 dataset”写入 `skipped_datasets` read model。
- `runtime_persistence.native_study_product_loop` 和
  `GET /runs/{run_id}/progress` 的 `study_loop_result` 都会携带这份解释。
- `POST /runs/native-study-loop` 响应新增 `skipped_datasets` 字段，避免用户第二次点击
  `Start Runnable Datasets` 时只看到“没有启动新 dataset”，却不知道是因为已有进度被保留。
- 支持“部分保留、部分新启动”：例如同一 run 先启动 ADAE，随后把 target set 扩展到
  ADAE + ADCM 时，ADAE 会进入 `skipped_datasets`，ADCM 会进入
  `started_datasets`。
- dataset start failure 的 runtime metadata 会排除本轮已 started 的 dataset，避免
  一个 dataset 同时被记录为 started 和 skipped。
- UI 的 Study Loop Result 增加 `Preserved` 行：
  - 显示 dataset 已有 graph progress；
  - 显示当前状态和下一步动作；
  - 避免同一个 skipped dataset 再从 review queue 重复渲染一遍；
  - 不改变任何 dispatch、review 或 execution 行为。

边界：

- `skipped_datasets` 是 graph-owned read model，只解释 native study-loop dispatch
  行为，不参与 dependency planning、dataset routing、审核批准或 R 执行。
- 这个切片没有把 active-dataset review/execute 改成全自动批处理，也没有启用 durable
  native resume。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets -v
Ran 2 tests in 0.451s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_new_target_while_preserving_existing_progress tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets -v
Ran 3 tests in 0.644s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_reports_preserved_progress_on_restart tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary -v
Ran 3 tests in 0.467s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - `skipped_datasets` 只解释 read model，不改变 dispatch；
  - blocked/non-runnable dataset 不会被误标为 skipped；
  - API 字段是 additive，不破坏旧调用；
  - UI 文案没有把 preserved 说成生成成功，也保留了“不审批、不执行 R”的说明。
- 非阻断建议：避免 preserved dataset 同时从 review queue 重复出现；已在提交前吸收。

### 2026-06-01 - LG2.7 Graph-State Read-Model Apply Boundary 切片

已完成：

- 收紧本地 UI 的 `applyGraphState(graph)` 语义：
  - 函数自身负责把 backend graph-state read model 写入 `state.graphState`；
  - `refreshGraphState()` 不再在调用前手动写同一字段；
  - graph-state 和 progress read-model 应用入口现在保持对称。
- 增加 UI contract 测试：
  - `refreshGraphState()` 必须通过 `applyGraphState(graph)`；
  - `applyGraphState(null)` 只清空 `state.graphState`；
  - null apply 不会改变现有 target、planning selection 或 plan，也不会调用
    `renderPlan()`。

当前边界：

- 这是浏览器 read-model 应用边界清理，不改变 API、GraphGateway、graph state
  transition 或产品按钮。
- backend graph state 仍然是事实来源；UI 只是保存和渲染读取到的 projection。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_refresh_graph_state_uses_apply_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_apply_graph_state_null_clears_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_does_not_default_target_selection_to_adae -v
Ran 3 tests in 0.153s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 131 tests in 16.782s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Progress Read-Model Apply Boundary 切片

已完成：

- 收紧本地 UI 的 `applyRunProgress(progress)` 语义：
  - 函数自身负责把 backend progress projection 写入 `state.runProgress`；
  - `refreshRunProgress()` 不再在调用前手动写同一字段；
  - 这样 direct apply、refresh apply 和测试 harness 使用同一条 read-model 应用路径。
- 增加 UI contract 测试：
  - `refreshRunProgress()` 必须通过 `applyRunProgress(progress)`；
  - `applyRunProgress(null)` 会清空 `state.runProgress`；
  - direct apply 后 native resume status 仍能作为状态文本渲染，不暴露 endpoint 或按钮。

当前边界：

- 这是浏览器 read-model 应用边界清理，不改变 API、GraphGateway、graph state
  transition 或任何产品按钮。

子 agent 审查：

- 2026-06-01，Heisenberg，`gpt-5.5`，只读审查结论：GO。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_resume_available_as_status_not_action tests.test_api_phase8.Phase8ApiTests.test_index_apply_run_progress_null_clears_progress_state -v
Ran 3 tests in 0.203s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 129 tests in 16.692s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume UI Boundary Guard 切片

已完成：

- 为本地 UI 增加合同测试，确认首页不会暴露
  `native_resume.explicit_resume_endpoint`，也不会出现 `/native-resume`
  调用路径。
- 增加 render harness 测试，确认 `native_resume.available == true` 时 UI 只显示
  “durable native resume available”的状态说明，不生成按钮或 endpoint 调用。
- 锁住当前产品边界：
  - UI 可以展示 `Recovery` 说明；
  - 默认产品操作仍通过可见的 split-flow review buttons；
  - native resume endpoint 仍是显式 API，不是默认 UI 动作。

当前边界：

- 本切片不新增按钮、不新增 API、不改变 graph state 或状态转换。
- 等 durable checkpointer/native resume 真正成为产品默认能力后，才能重新设计 UI
  入口。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_resume_available_as_status_not_action -v
```

### 2026-06-01 - LG2.1 Service Checkpointer Backend Fail-Closed API 切片

已完成：

- 为显式 service checkpointer backend 配置失败增加 API 层回归测试。
- 当 `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND=sqlite`，但可选
  `langgraph.checkpoint.sqlite` 包不可用时，新增测试证明 `POST /runs/prepare`
  会在写入任何 graph state 或 workflow projection 前以 HTTP 400 失败。
- 当 `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND=postgres`，但可选
  `langgraph.checkpoint.postgres` 包不可用时，新增同等 fail-closed API 覆盖。
- 测试断言不会留下半截产品状态：
  - 没有 `graph_state.json`；
  - 没有 `workflow_state.json`；
  - 没有 `langgraph_checkpoints.sqlite`。
- optional dependency probe 的 mock 只针对对应 checkpointer package name，
  不屏蔽其他 import。

当前边界：

- 本切片不为当前环境新增 SQLite/Postgres 支持。
- 本切片不宣称默认已经支持 persistent native resume。
- 它只锁定一种产品安全行为：如果 operator 显式启用了不可用的 checkpointer
  backend，API 必须尽早失败、说明缺失依赖，并且不留下半创建的 product run。

子 agent 审查：

- 2026-06-01，Fermat，`gpt-5.5`，只读审查结论：GO。
- 它确认测试走的是真实 `/runs/prepare` route，并经过
  `prepare_run_plan()`、`_open_graph_gateway()`、`_new_graph_gateway()` 和
  `GraphGateway(... checkpointer_backend=...)`。
- 它确认本切片不需要修改 production code。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_sqlite_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_postgres_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend -v
Ran 4 tests in 0.079s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_sqlite_checkpointer_unavailable tests.test_api_phase8.Phase8ApiTests.test_prepare_endpoint_fails_closed_when_postgres_checkpointer_unavailable -v
Ran 2 tests in 0.075s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 126 tests in 16.288s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.1 Service Gateway Lifecycle Boundary 切片

已完成：

- 新增 `_open_graph_gateway(study_dir=None, run_id=None)`，作为 service 层
  `GraphGateway` 的生命周期边界。
- 该 helper 仍通过 `_new_graph_gateway()` 构造 gateway，但会在 `finally` 中
  保证调用 `gateway.close()`。
- 把 service 层 gateway 使用迁移到 context helper，覆盖：
  - upload invalidation；
  - legacy `/runs` shim；
  - dependency planning；
  - native study product loop start；
  - dependency、draft-spec、code、terminal-failure review gates；
  - finalize inputs、draft spec generation、code generation、approved-code
    execution；
  - graph-state、progress、compare、review-summary read models。
- 增加 service-layer contract tests：
  - 除 `_open_graph_gateway()` 外，service helper 不能直接调用
    `_new_graph_gateway()`；
  - `_open_graph_gateway()` 使用结束后必须关闭 gateway。

当前边界：

- 本切片不改变公开 API/UI 默认 backend。没有显式设置
  `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND` 时，仍使用 memory。
- 本切片本身不宣称完整产品流已经具备 graph-native resume。
- 目的只是让 service 层适配前一切片已经加入的 run-scoped SQLite
  checkpointer path：每次请求打开的 gateway 用完就关闭资源。
- `GraphGateway` 仍然拥有 workflow state transitions；FastAPI service 层仍只做
  request validation、response shaping 和 read-model plumbing。

子 agent 审查：

- 2026-06-01，Dewey，`gpt-5.5`，只读审查结论：GO。
- 它确认 service 层 gateway 使用都经过 `_open_graph_gateway()`；默认 memory
  行为和 run-scoped sqlite 行为不变；异常路径会关闭资源；没有发现 double-close
  或 result 生命周期太短的风险。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory tests.test_api_phase8.Phase8ApiTests.test_service_layer_opens_graph_gateway_through_context_helper tests.test_api_phase8.Phase8ApiTests.test_open_graph_gateway_closes_gateway_after_use tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_defaults_to_memory_backend tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_falls_back_to_memory_without_run_context tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend -v
Ran 7 tests in 0.096s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 124 tests in 15.848s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.1 Service Checkpointer Backend Config 切片

已完成：

- 将 `_new_graph_gateway(study_dir=None, run_id=None)` 扩展为 service 层唯一的
  runtime checkpointer/backend 配置入口。
- 默认行为不变：
  - 没有环境变量时仍返回普通 `GraphGateway()`；
  - 公开 API/UI 默认仍使用 memory checkpointer。
- 新增显式环境变量 `ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND`：
  - `memory`：返回默认 gateway；
  - `sqlite`：当有 `study_dir` 和 `run_id` 时，使用当前 run 专属
    `runs/{run_id}/langgraph_checkpoints.sqlite`；
  - `sqlite` 但没有 run context 时，回退到 memory，用于 upload invalidation
    这类 study-wide 操作；
  - `postgres`：委托给 `GraphGateway`，在 lifecycle management 接好前 fail closed；
  - 未知 backend：用 `ApiServiceError` fail closed。
- 保持 runtime checkpoint store 和 product audit ledger 分离：
  `langgraph_checkpoints.sqlite` 不是 `graph_checkpoints.sqlite`。
- 增加测试覆盖：
  - 默认 memory；
  - run-scoped sqlite path；
  - 无 run context 时回退 memory；
  - 未知 backend 拒绝；
  - service helper 仍只能通过 factory 构造 `GraphGateway`。

当前边界：

- 本切片只是提供配置入口，不宣称完整产品流已经变成 persistent native
  LangGraph run。
- SQLite 仍需要可选 `langgraph-checkpoint-sqlite` 依赖。
- 完整 product interrupt/resume、automatic repair loops、生产级 Postgres
  lifecycle management 仍是后续工作。

子 agent 审查：

- 2026-06-01，Peirce，`gpt-5.5`，第一次只读审查结论：NO-GO。
- 阻断问题：当时的 sqlite path env override 可能让多个 run 共享同一个
  checkpoint DB，也可能让没有 run context 的 study-wide 操作误用 sqlite。
- 已修复：
  - 删除 service-level sqlite path override；
  - sqlite 现在只在 run context 存在时，用
    `default_sqlite_checkpointer_path(study_dir, run_id)` 生成路径；
  - sqlite 但无 run context 时回退 memory。
- 2026-06-01，Einstein，`gpt-5.5`，只读复核结论：GO。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_defaults_to_memory_backend tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_uses_run_scoped_sqlite_path_when_enabled tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_falls_back_to_memory_without_run_context tests.test_api_phase8.Phase8ApiTests.test_service_gateway_factory_rejects_unknown_backend tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory -v
Ran 5 tests in 0.040s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 122 tests in 15.958s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.1 Service Gateway Factory Boundary 切片

已完成：

- 在 `api/service.py` 中新增 `_new_graph_gateway()`，作为 service 层唯一的
  `GraphGateway` 构造边界。
- 把 service helpers 里的直接 `GraphGateway()` 构造改为调用这个 factory，覆盖：
  - upload invalidation；
  - legacy run shim；
  - prepare；
  - native study loop；
  - dependency/draft/code/terminal-failure review gates；
  - approved-code execution；
  - progress、graph-state、compare、review-summary read models。
- 增加 service-layer contract test：除 `_new_graph_gateway()` 外，service helper
  不能直接构造 `GraphGateway`。
- 当前运行行为不变：factory 仍返回默认 `GraphGateway()`，所以公开 API/UI 默认仍是
  memory checkpointer 行为。

当前边界：

- 本切片不启用 SQLite/Postgres checkpointer persistence。
- 不改变 FastAPI routes、workflow state transitions、review gates、LLM 生成、
  R execution 或 artifact 写入。
- 目的只是把后续 checkpointer/backend 配置收敛到一个 service 边界，避免每个
  endpoint 各自选择。

子 agent 审查：

- 2026-06-01，Averroes，`gpt-5.5`，只读审查结论：GO。
- 它确认没有业务行为变化，service 层除 factory 外没有残留直接
  `GraphGateway()` 调用，且现有测试仍可 patch
  `adam_agent.api.service.GraphGateway`。
- 非阻断建议：AST guard 挡不住 `Gateway = GraphGateway` 这类别名绕法；已在
  factory 附近增加约定注释。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_service_layer_constructs_graph_gateway_only_through_factory tests.test_api_phase8.Phase8ApiTests.test_product_service_wrappers_delegate_state_changes_to_gateway_methods tests.test_api_phase8.Phase8ApiTests.test_service_layer_no_longer_writes_workflow_state_directly tests.test_api_phase8.Phase8ApiTests.test_service_layer_reads_graph_state_only_for_explicit_read_models tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model -v
Ran 5 tests in 0.120s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 118 tests in 15.949s - OK

python -B -m compileall -q src tests
OK
```

### 2026-06-01 - LG2.7 Code Approval 与 Execution UI 拆分切片

已完成：

- 把浏览器里旧的合并动作拆成两个明确的 graph 动作：
  - `Approve Code`：只记录人工 code review 决策；
  - `Run Approved Code`：只执行已经批准过的代码。
- 更新 graph-aware UI gate 映射：
  - `review_code` 只启用 `Approve Code`；
  - `execute_approved_code` 和 `retry_approved_execution` 只启用
    `Run Approved Code`。
- 移除浏览器里的隐藏 approve-then-execute 路径：
  - UI 不再由同一个按钮连续调用 `/code-review` 和
    `/execute-approved-code`；
  - 用户必须先批准代码，再显式运行本地 R。
- 后端边界不变：
  - service 和 Gateway 仍然负责真正的 approval / execution gate；
  - UI 只按 graph progress read model 展示下一步动作。
- 增加 UI contract 和 Node harness 覆盖：
  - 旧的 `Approve And Run Locally` 文案已经消失；
  - approval 只调用 `/code-review`；
  - execution 只调用 `/execute-approved-code`；
  - next-action matrix 把 approval 和 execution 拆开验证。

当前边界：

- 这是 UI/gate alignment 切片，不改变 dependency planning、LLM 生成、生成
  R 代码内容、R execution 内部逻辑或 compare 行为。
- graph progress read model 仍然是下一步动作的权威来源。浏览器里的 review cache
  只是展示状态；如果没有 graph-owned code approval，后端仍会拒绝执行。

子 agent 审查：

- 2026-06-01，Lovelace，`gpt-5.5`，只读审查结论：GO。
- 它确认没有隐藏 approve+execute UI 路径残留，后端 review 和 execution gate
  仍然分离。
- 非阻断建议：execution 可用性也可以额外看浏览器本地 review cache，让 disabled
  reason 更清晰。本切片未采用，因为 graph `next_action` 是权威 workflow state，
  且后端在缺少 approval 时已经 fail-closed。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_code_approval_and_execution_are_separate_ui_actions -v
Ran 5 tests in 0.292s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 117 tests in 15.741s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Review Gate Action Read-Model 切片

已完成：

- `GraphGateway.progress_summary()` 现在会在 dataset progress 中暴露
  graph-owned `available_actions`：
  - `draft_spec_review`：`approve` / `reject`；
  - `code_review`：`approve` / `reject`；
  - `terminal_failure`：保留既有 triage actions。
- `available_actions` 只是 read-model hint，不执行任何动作：
  - draft spec 审核仍必须走现有 draft-spec review gateway flow；
  - code review 仍必须走现有 code-review gateway flow；
  - R execution 仍必须走显式 approved-code execution。
- 当 dataset 被 dependency/stale/study-level gate block 时，
  `available_actions` 会清空，避免 UI 暗示可以绕过上游 gate。
- terminal failure 已经被人工 triage 后，不再继续暴露 terminal triage actions。
- human review queue 现在把同一份 `available_actions` 投影给 UI，并只作为文本提示
  展示“Available graph actions”，不是可点击命令。
- 根据子 agent 非阻断建议，补充了 `draft_spec_review` action hints 的显式回归测试。

当前边界：

- 本切片不新增任何 approve/reject endpoint。
- 本切片不改变 dependency planning、draft/code review 校验、LLM generation、
  R execution、compare、repair/spec revision 或 native checkpointer 行为。
- UI 展示的是 graph read model，不是新的浏览器端 workflow state machine。

子 agent 审查：

- 2026-06-01，Poincare，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - `available_actions` 仍停留在 read model，没有新增执行路径；
  - blocked/stale/study-level dependency gate 场景会清空 dataset actions；
  - terminal failure triage 后不会继续显示 triage actions；
  - UI action hints 只是 escape 后的文本，不是按钮，也没有绑定 fetch/click handler；
  - dataset progress 和 review queue 在已覆盖的 code-review 路径上保持一致。
- 它提出的非阻断建议是补充 `draft_spec_review` action hints 覆盖；已在提交前完成。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_review_gate_actions_when_unblocked tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_draft_spec_review_actions tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_exposes_terminal_failure_actions_until_reviewed -v
Ran 3 tests in 0.161s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_human_review_queue_from_graph_state tests.test_api_phase8.Phase8ApiTests.test_index_exposes_terminal_failure_triage_actions -v
Ran 2 tests in 0.078s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 236 tests in 25.074s - OK (skipped=2)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Stale Study Loop Result Guard 切片

已完成：

- 收紧 graph-owned progress read model：当 dependency plan 已经过期时，
  `study_loop_result` 不再投影给 UI：
  - `dependency_plan.plan_stale == true`；或
  - `dependency_review_status == "stale"`。
- 这样可以避免 study input 文件变化后，UI 还继续展示上一轮 native study-loop start
  的结果。
- 这个改动刻意放在 read-model 层：
  - 不删除 `runtime_persistence.native_study_product_loop` 历史元数据；
  - 不改变 dependency planning；
  - 不 approve review；
  - 不 generate code；
  - 不 run R。
- 新增 gateway 回归测试：
  - 先为 ADAE/ADCM 启动 native study loop；
  - 确认 `study_loop_result` 正常投影；
  - 修改 `input_sdtm/ae.csv`；
  - 标记 inputs changed；
  - 确认 progress 已 stale，且 `study_loop_result == {}`。

子 agent 审查：

- 2026-06-01，Linnaeus，`gpt-5.5`，只读审查结论：GO。
- 它确认该 guard 不会隐藏正常的 dataset review queue，因为 `review_queue`
  仍然独立暴露；并同意 read-model 边界比删除 runtime persistence metadata 更稳妥。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_hides_study_loop_result_after_inputs_change tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_prioritizes_stale_plan_replan -v
Ran 4 tests in 0.675s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_clears_stale_study_loop_result_when_progress_has_none tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_uses_graph_gateway_progress_read_model tests.test_api_phase8.Phase8ApiTests.test_upload_marks_existing_graph_product_state_stale_and_blocks_generation -v
Ran 4 tests in 0.384s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 118 tests in 8.436s - OK (skipped=2)
```

### 2026-06-01 - LG2.7 Study Loop Result Progress Read-Model 切片

已完成：

- 在 graph-owned run progress read model 中新增 `study_loop_result`。
- `GraphGateway.progress_summary()` 现在从 canonical graph state metadata 投影最近一次
  native study-loop 摘要：
  - `runtime_persistence.native_study_product_loop.started_datasets`；
  - `runtime_persistence.native_study_product_loop.blocked_datasets`；
  - graph-owned `review_queue`。
- `RunProgressResponse` 现在暴露 `study_loop_result`。
- UI 在 refresh 或重新加载 progress 时，会从 `progress.study_loop_result` 恢复
  `Study Loop Result` 面板。
- 之前 `/runs/native-study-loop` 的 command response 仍然只作为展示 fallback：
  - 如果 refresh 后已经拿到 `source: graph_progress` 的结果；
  - command response 不会再覆盖 graph progress 恢复出来的结果。
- 当 progress 中恢复出的 study-loop result 没有新鲜 `dataset_results` 时，UI 会使用
  graph `review_queue` 还原 started dataset 对应的具体 review gate 标签。

当前边界：

- `study_loop_result` 只是 read model，不参与 dependency planning、dataset dispatch、
  draft/code approval、LLM generation 或 R execution。
- `runtime_persistence` 仍然是 native loop/checkpoint 边界元数据；UI 不会写入它。

子 agent 审查：

- 2026-06-01，Erdos，`gpt-5.5`，第一次只读审查结论：NO-GO。
- 它指出 `startNativeStudyLoop()` 在刷新 graph progress 后，又用 command response
  覆盖了已经恢复出的 `graph_progress` 结果。
- 已修复：
  - command response 现在只在没有 graph-progress result 时才作为 fallback 写入；
  - 新增 Node harness 测试覆盖这个优先级问题。
- 2026-06-01，Erdos，`gpt-5.5`，第二次只读审查结论：GO。
- 2026-06-01，Euclid，`gpt-5.5`，中文文档同步后的只读审查结论：GO。
  它提出的非阻断建议已在提交前吸收：
  - 空的 `progress.study_loop_result` 现在会清理旧浏览器展示态，避免切换 run
    后继续显示上一轮面板；
  - API 覆盖现在同时验证 ADAE 和 ADCM 都出现在嵌套的
    `study_loop_result.review_queue` 中。
- 2026-06-01，Raman，`gpt-5.5`，吸收建议后的最终只读复核结论：GO。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_index_start_study_loop_keeps_graph_progress_result_over_command_response -v
Ran 7 tests in 0.758s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_summary_reports_graph_owned_next_actions tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress -v
Ran 5 tests in 0.823s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 116 tests in 15.760s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 117 tests in 8.797s - OK (skipped=2)

python -B -m unittest tests.test_graph_smoke -v
Ran 84 tests in 7.517s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Study Loop Result Read-Model 切片

已完成：

- 在本地 UI dashboard 中新增 `Study Loop Result` 面板。
- 该面板只展示 `/runs/native-study-loop` 返回值和 graph progress read model 中已有
  的事实：
  - 本次 study-level start 启动了哪些 datasets；
  - 哪些 datasets 仍因为 dependency/user action 被阻断；
  - graph review queue 中还有哪些审核门打开。
- 继续明确产品边界：
  - `Start Runnable Datasets` 只把 dataset 推进到 review gate；
  - 不自动 approve draft spec；
  - 不自动 approve generated code；
  - 不执行 R。
- 浏览器新增 `lastStudyLoopResult` 仅作为最近一次命令返回的展示缓存，不会写回后端，
  也不参与 graph 决策。
- 根据子 agent 的非阻断建议，新增 Node harness 测试，直接执行
  `renderStudyLoopResult()`，验证 started、blocked、review queue 三类信息都能实际
  渲染出来，而不只是检查 HTML 字符串存在。

当前边界：

- 这是 UI read-model 改进，不改变 dependency planning、Gateway dispatch、approval
  gates、LLM generation 或 R execution。
- 面板总结的是当前浏览器会话中最近一次 study-loop start；canonical workflow truth
  仍然是 graph state 和 graph progress。

子 agent 审查：

- 2026-06-01，Anscombe，`gpt-5.5`，只读审查结论：GO。
- 它确认本切片没有新增隐藏 approve/run 路径，也没有把浏览器本地状态变成独立 workflow
  state machine。
- 它提出的非阻断建议是补一个 Node harness render test；已在提交前完成。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary -v
Ran 2 tests in 0.120s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_serves_local_web_ui tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_does_not_start_dependency_blocked_targets -v
Ran 4 tests in 0.407s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Dependency Assumption UI Wording 切片

已完成：

- 收紧 `no_dependency_evidence` 和 Start Runnable Datasets 相关 UI 文案：
  - “可以进入 review gate”不再被描述成依赖已经被证明；
  - 明确提示没有上游 ADaM evidence 时，仍必须在 spec/code review 中确认；
  - Start Runnable Datasets 的按钮说明和运行中提示都写明：
    `This is not dependency proof. R will not run.`
- 只改用户可见文案和 UI contract tests，不改 dependency planning、graph state、
  Gateway gate、review gate 或 R execution 行为。
- 补充 UI contract tests，锁住这些说明，防止后续 UI 文案重新把 review gate
  误写成 dependency proof。

子 agent 审查：

- 2026-06-01，Tesla，`gpt-5.5`，只读审查结论：GO。
- 它确认本切片没有改 graph/dependency/execution 逻辑，文案与 Reference ADaM
  policy 和 per-dataset human approval 执行边界一致。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_marks_review_only_outputs_without_runtime_language tests.test_api_phase8.Phase8ApiTests.test_index_dependency_map_does_not_treat_reference_adam_as_runtime_dependency tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
Ran 3 tests in 0.119s - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 111 tests in 15.240s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume Queue Read-Model 切片

已完成：

- 在 graph-owned `native_resume` progress read model 中新增
  `interrupt_queue`。
- 这个队列只列出显式 native dataset resume endpoint 能处理的 dataset
  interrupt：
  - `draft_spec_review`；
  - `code_review`；
  - `terminal_failure`。
- 每个 item 包含 dataset、interrupt name、dataset status、可用人工动作、
  默认 split-flow review path，以及当前 run 是否真的能通过 durable native
  LangGraph checkpointer resume。
- 默认 memory checkpointer 下，队列可以展示正在等待的 dataset gate，但每个 item
  都是 `can_resume == false`，也没有 resume endpoint。这保持当前产品路径诚实：
  durable native resume 没有显式可用时，用户应继续使用 split-flow review
  endpoints。
- 当可选 SQLite checkpointer 可用时，同一个队列会标记为 resumable，并暴露显式
  native resume endpoint。
- `study_loop_result` 现在也通过 `native_resume_interrupts` 带上同一队列，使
  study-loop result 和顶层 progress read model 保持一致。

当前边界：

- 这是 read-model 和 API contract 切片。
- 不新增 UI 按钮，不批准/拒绝任何内容，不执行 R，也不改变默认 memory
  checkpointer fail-closed 行为。
- `dependency_user_action_required` 不进入该队列，因为 native dataset resume
  endpoint 不处理这个 interrupt。
- 经审查后，queue 现在会跳过 available actions 为空的 interrupt。这样已经
  triage 过的 terminal-failure 状态不会被继续展示成新的 native resume gate。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_when_package_available tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_queue_as_resumable_when_package_available -v
Ran 4 tests in 0.168s - OK (skipped=2)

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_queue_as_resumable_when_package_available tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets -v
Ran 3 tests in 0.387s - OK (skipped=1)

python -B -m compileall -q src tests
OK

git diff --check
OK，仅有预期内 CRLF working-copy warnings

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 279 tests in 29.955s - OK (skipped=4)
```

子 agent 审查：

- 初审：NO-GO。
- 必修问题：已经 triage 过的 terminal-failure interrupt 在 durable checkpointer
  下仍可能出现在 queue 中并显示 `can_resume=true`，因为原实现只按 interrupt
  name 过滤，没有确认该 gate 是否仍有可用动作。
- 已修复：`_native_resume_interrupt_queue()` 在
  `_available_dataset_actions(dataset_state)` 为空时跳过该 item。
- 已补回归：native terminal-failure review 在 triage 前进入 queue；记录
  `terminal_failure_review` 后从 queue 消失。

补充验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets -v
Ran 3 tests in 0.522s - OK
```

### 2026-06-01 - LG2.7 Native Resume Boundary Read-Model 切片

已完成：

- 在 graph-owned progress 的 `study_loop_result` 中增加 native resume 边界字段：
  - `native_resume_available`
  - `native_resume_scope`
  - `resume_boundary`
- 字段只从 `runtime_persistence` 投影，不参与 dependency planning、dataset
  dispatch、draft/code review、LLM generation 或 R execution。
- 默认 memory checkpointer 下，`study_loop_result` 会明确报告：
  - `native_resume_available == false`
  - `native_resume_scope == "none"`
  - `resume_boundary == "graph_state_projection_only"`
- UI 的 Study Loop Result 说明现在会告诉用户：
  - 默认恢复依赖保存的 graph state；
  - 该 run 没有启用 durable LangGraph checkpoint resume；
  - Start Runnable Datasets 仍然不会 approve draft spec、approve code 或 run R。
- 根据子 agent 的非阻断建议，UI 不再把 `native_pilot_interrupts_only`
  这类内部枚举直接展示给用户，而是映射成普通文字
  “pilot graph interrupts only”。

当前边界：

- 本切片是 read-model/UX 边界收口，不新增 approve/reject endpoint。
- 本切片不把默认 product flow 声称为完整 native LangGraph
  interrupt/checkpointer resume。
- 公开 split-flow review/execute endpoints 仍然是当前默认产品路径；native
  study loop 仍是 dispatch 到 review gate 的 pilot。

子 agent 审查：

- 2026-06-01，Hubble，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 新字段准确来自 `runtime_persistence`；
  - 没有新增 workflow state machine、approve/reject path、service 状态分叉或
    execution-path change；
  - UI 默认路径文案清楚说明是 saved graph state recovery，不是 durable
    LangGraph checkpoint resume；
  - API progress 和 UI contract 测试覆盖了关键边界。
- 非阻断建议：不要在 true-case UI 文案中直接展示内部 scope 枚举；已吸收。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets -v
Ran 5 tests in 0.717s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 120 tests in 9.482s - OK (skipped=2)

python -B -m unittest tests.test_api_phase8 -v
Ran 126 tests in 17.299s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Primary Action Availability Writer 切片

已完成：

- 移除了 primary workflow 按钮在 graph-aware availability renderer 之外的本地
  `button.disabled = ...` 直接赋值：
  - `Finalize Inputs / Draft Spec`
  - `Start Runnable Datasets`
  - `Approve Draft Spec`
  - `Generate R Code`
  - `Approve Code`
  - `Run Approved Code`
- 这些按钮现在统一通过 `renderActionAvailability()` 和
  `setButtonAvailability()` 写入可用状态；可用性和 disabled reason 都来自
  `actionAvailability()`。
- `renderTargetButtons()`、`resetActiveDatasetView()`、`finalizeInputsForDraftSpec()`、
  `renderDraftSpecPane()`、`renderDraftSpecReviewTable()` 和 `loadReviewSummary()`
  不再用浏览器本地缓存猜测 primary workflow 按钮状态。
- 新增 UI contract 测试，防止这些 primary buttons 以后又退回到分散的
  `.disabled` 直接写入。

当前边界：

- 这是浏览器 UI read-model 边界清理。
- 不改变 FastAPI endpoints、GraphGateway 状态转换、graph progress payload，也不改变
  后端 review/execute 行为。
- graph-owned 的 `actionAvailability()` / `graphActionGate()` 路径仍是 primary
  workflow action 是否可用的权威来源。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_action_buttons_use_single_availability_writer tests.test_api_phase8.Phase8ApiTests.test_index_explains_disabled_actions_from_existing_state tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_code_approval_and_execution_are_separate_ui_actions tests.test_api_phase8.Phase8ApiTests.test_index_load_review_summary_recovers_generated_code_for_graph_review_gate tests.test_api_phase8.Phase8ApiTests.test_index_review_summary_without_graph_gate_cannot_enable_code_approval tests.test_api_phase8.Phase8ApiTests.test_index_review_summary_can_recover_code_when_graph_state_succeeds_without_progress -v
Ran 7 tests in 0.532s - OK
```

### 2026-06-01 - LG2.7 Native Resume Progress Read-Model 切片

已完成：

- 在 `GET /runs/{run_id}/progress` 中新增稳定的 `native_resume` 对象。
- 该字段只从 canonical `graph_state.runtime_persistence` 派生：
  - `available`
  - `scope`
  - `boundary`
  - `explicit_resume_endpoint`
  - `default_review_path`
  - `restart_recovery_source`
  - `message`
- 默认 memory checkpointer 路径会报告：
  - `available == false`
  - `scope == "none"`
  - `boundary == "graph_state_projection_only"`
  - `default_review_path == "split_flow_review_endpoints"`
- UI 的 Study Progress 面板新增一个清晰的 `Recovery` 步骤：
  - native resume 不可用时，提示用户使用可见 review buttons，并说明重启恢复读取
    已保存的 graph state；
  - durable native resume 可用时，用用户可读措辞说明可用范围。

当前边界：

- 这是 read-model 和 UI clarity 切片。
- 不新增按钮、不新增 workflow command、不新增状态转换路径。
- 显式 native resume endpoint 在默认 memory checkpointer 下仍然 fail closed。

后续清理：

- `study_loop_result.native_resume_available/native_resume_scope/resume_boundary`
  现在复用同一个 `native_resume` helper，不再各自重新计算字段。
- 将 `native_resume.endpoint` 改名为 `native_resume.explicit_resume_endpoint`，
  避免自动化客户端误把它理解成默认 review path。
- 增加断言，证明 `study_loop_result` 和顶层 `native_resume` 保持一致。

子 agent 审查：

- 2026-06-01，Harvey，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - `native_resume` 和 `runtime_persistence` 保持一致；
  - UI 文案没有暗示默认 native resume 已可用；
  - 没有引入新的 product workflow path，也没有绕过 GraphGateway；
  - 测试覆盖了默认不可用路径。
- 非阻断建议：为可选 SQLite checkpointer 的正向 progress read-model 分支增加测试。
  已吸收为 skip-if-unavailable 测试，不伪造当前本地 durable resume 支持。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary tests.test_graph_gateway.GraphGatewayTests.test_sqlite_progress_marks_native_resume_when_package_available tests.test_api_phase8.Phase8ApiTests.test_index_exposes_graph_owned_progress_panel tests.test_api_phase8.Phase8ApiTests.test_progress_endpoint_reports_graph_owned_next_actions -v
Ran 4 tests in 0.253s - OK (skipped=1)

python -B -m unittest tests.test_graph_gateway -v
Ran 122 tests in 9.583s - OK (skipped=3)

python -B -m unittest tests.test_api_phase8 -v
Ran 127 tests in 17.797s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.1/LG2.8 Native Resume Endpoint Fail-Closed 切片

已完成：

- 新增显式 API：
  `POST /runs/{run_id}/datasets/{dataset}/native-resume`。
- 新增 `NativeDatasetResumeRequest` / `NativeDatasetResumeResponse`，请求模型只保留
  native resume 真正需要的字段：
  - `study_dir`
  - `decision`
  - `reviewer`
  - `notes`
  - `execute_after_approval`
  - `rscript_path`
- `GraphGateway.resume_native_dataset_interrupt()` 现在是统一 native dataset
  interrupt resume 入口，但第一步会检查 durable native resume 是否可用。
- 默认 memory checkpointer 下，该 endpoint 会 fail closed：
  - HTTP 400；
  - 明确提示 native LangGraph interrupt resume 未启用；
  - 不回落到普通 code-review/draft-spec-review path；
  - 不写 `*_code_review.json` review artifact。
- 该入口按 interrupt 类型分发到已有 native resume 方法：
  - `draft_spec_review`
  - `code_review`
  - `terminal_failure`
  但只有 durable checkpointer 可用时才会进入这些路径。

当前边界：

- 这是为后续 durable checkpointer/native resume 预留的公开接口，不改变当前默认
  split-flow 产品路径。
- 当前本地环境没有 `langgraph-checkpoint-sqlite`，所以正向 durable resume 路径
  没有启用，也没有伪造通过。
- 本切片不新增 UI 按钮，不让用户误以为默认产品流已经完整 native resume。

子 agent 审查：

- 2026-06-01，Sartre，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 默认 API path 使用 memory backend 并返回 400；
  - `GraphGateway.resume_native_dataset_interrupt()` 在 load state 或调用具体 resume
    方法前先检查 durable native resume；
  - 不支持 native resume 时不会写 `*_code_review.json`；
  - service 层只是委托 gateway 和格式化响应，没有新增状态分叉。
- 非阻断建议：移除 request model 中未使用的 LLM/config 字段，避免暗示该 endpoint
  会继续 draft-spec-to-code generation；已吸收。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer tests.test_api_phase8.Phase8ApiTests.test_native_resume_endpoint_fails_closed_without_durable_checkpointer tests.test_api_phase8.Phase8ApiTests.test_service_layer_reads_graph_state_only_for_explicit_read_models -v
Ran 3 tests in 0.319s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 121 tests in 9.261s - OK (skipped=2)

python -B -m unittest tests.test_api_phase8 -v
Ran 127 tests in 17.334s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Review Summary 恢复 Code Review 材料切片

已完成：

- UI 的 `loadReviewSummary()` 现在会把 `/review-summary` 返回的
  `dataset_reviews[].generated_code`、`generated_code_path`、assumptions、
  risk points 和 warnings 合并进 `state.generatedByDataset`。
- 这样页面刷新、切换 target 或从 graph progress 恢复后，只要 graph 下一步是
  `review_code`，UI 可以从持久 review-summary artifact 重新显示代码审核材料。
- 恢复有 graph gate：只有 graph progress 或 graph state 已经证明该 dataset
  处于 code-review / approved-code execution / code artifact 状态时，review-summary
  才能补齐代码文本。
- 该恢复只补齐浏览器 read model，不写 graph state、不批准代码、不执行 R。
- 如果 graph 仍要求 `review_code` 且 review-summary 显示 dataset 仍是
  `needs_review`，UI 会清掉本地 `reviewByDataset` 中可能残留的旧 approval，
  防止浏览器缓存误导按钮状态。

边界：

- graph progress 的 `next_action` 仍然是按钮可用性的权威来源。
- review-summary 只是 artifact/read-model 恢复通道，不能单独让 code approval
  或 execution 越过 graph gate。
- graph 已经批准代码、下一步是 execution 时，review-summary 的旧 `needs_review`
  状态不会清除 graph-approved 的本地显示缓存。
- 该切片不新增 endpoint，不改变 code-review / execute-approved-code 后端行为。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_load_review_summary_recovers_generated_code_for_graph_review_gate -v
Ran 1 test in 0.098s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_action_availability_next_action_matrix tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_code_approval_and_execution_are_separate_ui_actions -v
Ran 3 tests in 0.232s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，第一次只读审查结论：NO-GO。
- 它指出：如果 review-summary 先恢复代码，而 graph/progress refresh 失败，浏览器可能
  在没有 graph gate 的情况下通过 fallback 点亮 code approval。
- 已修复：
  - `loadReviewSummary()` 现在先刷新 graph read models，再合并 review-summary；
  - `applyReviewSummaryDataset()` 只有在 graph progress/state 已证明 code-review、
    approved-code execution 或 code artifact 状态时才恢复代码文本；
  - 新增负向测试，证明 graph/progress 不可用时 review-summary 不能恢复 generated
    code、不能点亮 approve、也不会 POST `/code-review`。
- 复审结论：GO。
- 非阻断建议已吸收：新增 graph-state 成功但 progress 失败时的恢复测试，明确
  graph_state 可以作为 workflow truth；同时保留 graph/progress 都不可用时的负向测试，
  防止 review-summary artifact 单独开门。

补充验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_load_review_summary_recovers_generated_code_for_graph_review_gate tests.test_api_phase8.Phase8ApiTests.test_index_review_summary_without_graph_gate_cannot_enable_code_approval tests.test_api_phase8.Phase8ApiTests.test_index_review_summary_can_recover_code_when_graph_state_succeeds_without_progress -v
Ran 3 tests in 0.300s - OK
```

### 2026-06-01 - LG2.3 Native Study Product Loop API/UI 接线切片

已完成：

- 新增 `POST /runs/native-study-loop`，把上一切片的 internal
  `GraphGateway.start_native_study_product_loop()` 暴露给本地 UI：
  - API 只接收 study/run/target/config/LLM exposure 信息；
  - service 层只做 request validation、provider/exposure resolution 和 response shaping；
  - 真实状态转换仍委托 `GraphGateway`。
- UI 新增 `Start Runnable Datasets` 主动作：
  - 使用 planning selection，而不是当前 active detail target；
  - 调用 `/runs/native-study-loop` 后刷新 graph state、progress read model 和 review
    summary；
  - 不把浏览器本地状态写回 graph state。
- 明确产品边界：
  - study-level start 只把当前 runnable selected datasets 推进到
    `draft_spec_review` 或 `code_review`；
  - 不自动 approve draft spec；
  - 不自动 approve generated code；
  - 不执行 R；
  - 后续审核、批准和执行仍走现有 per-dataset review/execute 路径。
- 修复 native checkpoint state 序列化边界：
  - 移除 `DatasetGraphState` 中的 `llm_client_builder` 和
    `target_context_builder`；
  - `GraphGateway` 不再把 Python function object 放入 LangGraph state payload；
  - 运行时依赖注入改为 `compile_dataset_graph(...)` 编译边界上的闭包参数；
  - 因此本地 fake LLM/测试注入仍可用，但 checkpoint state 只保留可序列化业务数据。
- 新增 API/UI 回归测试：
  - endpoint 可以一次启动多个 runnable datasets；
  - dependency-blocked target 不会被启动；
  - UI 暴露 `Start Runnable Datasets`、`/runs/native-study-loop` 和
    `startNativeStudyLoop()`；
  - planning selection 和 active detail target 继续分离。

当前边界：

- 这是 native study loop 的第一版本地产品入口，不是完整自动批处理执行器。
- `no_dependency_evidence` 这类 review-required dependency evidence 会被带入
  dataset review gate，而不是直接当作硬失败；这符合当前 Gateway 设计，但 UI
  文案后续应继续说明“进入 review gate 不等于依赖已经被生产级证明”。
- R 执行仍必须由用户在 per-dataset code review 之后显式触发。

子 agent 审查：

- 2026-06-01，Rawls，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - `/runs/native-study-loop` 不调用 approve 或 R execution；
  - dependency-blocked targets 不会 dispatch；
  - UI 使用 multi-target plan selection，并刷新 graph-owned read models；
  - builder functions 不再进入 `DatasetGraphState`，而是通过 graph compile boundary
    注入；
  - 现有单 dataset approve/execute 路径仍是独立显式调用。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_keeps_planning_selection_separate_from_active_target_view -v
Ran 1 test in 0.039s - OK

python -B -m unittest tests.test_graph_gateway tests.test_graph_smoke tests.test_api_phase8 -v
Ran 312 tests in 32.118s - OK (skipped=2)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Native Resume Queue Gate 对齐切片

已完成：

- 收紧 `native_resume.interrupt_queue` 的 read-model：
  - 如果 study-level interrupt 仍 open，例如 `dependency_review`，不再展示任何
    dataset native-resume gate；
  - dataset gate 在进入 native-resume queue 前复用 graph progress 的 blocked
    reason 计算，确保 stale dependency plan、blocked dependency、review-required
    dependency source 等状态不会被展示成 native resume 任务。
- 该切片不改变 split-flow review endpoint。默认 memory checkpointer 下，用户仍应通过
  可见 review buttons 处理审核；native resume endpoint 仍是 durable checkpointer
  可用时的显式入口。
- 新增回归测试：
  - study-level `dependency_review` gate open 时，`review_queue` 保留 study gate，
    `native_resume.interrupt_queue` 为空；
  - dependency plan stale 时，dataset card 可显示原始下一步为 `review_code`，但
    blocked flag 和 available actions 会关闭，native resume queue 也为空。

边界：

- 这是 read-model consistency hardening，不新增 UI 按钮、不改变 API endpoint、不执行
  R、不改变 LLM generation、repair/spec-revision 或 dependency planning。
- 它让 native resume queue 与 graph-owned progress 的“能不能继续”判断保持一致，
  避免 UI 或自动化客户端把被 study gate / stale plan 拦住的 dataset gate 理解成
  可恢复任务。

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - study-level open interrupt 下 queue 为空，和 `review_queue` 先处理 study gate
    的语义一致；
  - dataset gate 复用 `_blocked_dataset_progress_reason()` 后不会越过 stale
    dependency plan 或 blocked dependency；
  - 默认 memory checkpointer 仍只展示 read-model，不暴露可调用 endpoint；
  - 本切片没有新增 UI 按钮、API workflow command、LLM/R execution 或 repair
    路径变化。
- 非阻断建议：后续可补 durable SQLite 场景下 study-level gate 仍不暴露 queue
  endpoint 的测试；当前同一函数路径已覆盖，暂不扩大本切片范围。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_progress_reports_native_resume_queue_without_enabling_memory_resume tests.test_graph_gateway.GraphGatewayTests.test_progress_native_resume_queue_respects_study_level_gate tests.test_graph_gateway.GraphGatewayTests.test_progress_native_resume_queue_respects_blocked_dataset_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_terminal_failure_review_roundtrip_persists_triage -v
Ran 4 tests in 0.309s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 281 tests in 29.429s - OK (skipped=4)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

### 2026-06-01 - LG2.7 Study Loop Native Resume Queue Count Mirror 切片

已完成：

- 将 top-level native resume queue read-model 字段同步镜像到
  `study_loop_result`：
  - `native_resume_has_queue_items`
  - `native_resume_queue_item_count`
- 增加 gateway/API 覆盖，确保 native study loop 一次启动多个 runnable datasets
  并停在 dataset review gates 时，`study_loop_result` 不会和 top-level
  `native_resume` 摘要发生字段漂移。
- 字段语义保持不变：
  - `has_queue_items` / `queue_item_count` 只表示存在可见的 native-resume
    queue entries；
  - 它们不表示默认 memory checkpointer 下可以调用 native resume endpoint；
  - 每个 queue item 上的 `can_resume` 仍然是“能否调用 resume”的判断字段。

边界：

- 这是 read-model consistency hardening，不新增 endpoint、不新增 UI action、不改变
  LLM/R execution，也不改变默认 memory checkpointer fail-closed 边界。
- 目标是让 UI 和自动化客户端从 `study_loop_result` 读取一致的 study loop
  摘要，不需要自行重新计算 top-level native resume counts。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_progress_reports_runtime_persistence_boundary -v
Ran 3 tests in 0.579s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 字段来自同一次 `_native_resume_progress(state)` 结果镜像，不新增第二个事实源；
  - 镜像字段只表示存在可见 queue entries；
  - 能否调用 native resume 仍由 `native_resume_available` 和每个 queue item 的
    `can_resume` / `resume_endpoint` 判断；
  - 测试保留了默认 memory fail-closed 语义，也就是 queue 可见但 native resume
    不可调用。

### 2026-06-01 - LG2.7 Study Loop Native Resume Queue UI Status 切片

已完成：

- 更新本地 Study Loop Result 面板，让它消费
  `study_loop_result.native_resume_has_queue_items` 和
  `study_loop_result.native_resume_queue_item_count`。
- 面板现在会告诉用户 native resume queue 里有多少可见 review gates，同时保持边界：
  - 默认 memory mode：继续使用可见 review buttons；native resume 不可调用；
  - durable mode：该面板仍只是状态展示，不创建 resume 控件。
- 新增 UI contract/render 测试，覆盖 memory-mode queue 可见性和 durable-native-resume
  状态可见性。

边界：

- 这是 read-model display 切片。
- 不新增 endpoint、不新增 native-resume button、不改变 LLM/R 行为，也不改变 workflow
  state transition。
- queue count 仍只是 visibility signal，不是 callable-resume signal。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_exposes_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_study_loop_result_summary tests.test_api_phase8.Phase8ApiTests.test_index_renders_native_resume_available_as_status_not_action tests.test_api_phase8.Phase8ApiTests.test_index_recovers_study_loop_result_from_progress -v
Ran 4 tests in 0.279s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认 UI 只读取 study-loop native resume queue 字段，并用
  `native_resume_available` 区分 memory 和 durable 文案；没有新增 button、endpoint
  call、GraphGateway write、LLM 路径或 R execution 行为。
- 它也确认测试覆盖了主要误导风险：渲染结果不会暴露 `native-resume`、
  `explicit_resume_endpoint` 或 `<button`。

### 2026-06-01 - LG2.7 Study Loop Native Resume Queue Command Response 切片

已完成：

- 将 `/runs/native-study-loop` 命令响应补齐为和 graph progress read-model
  一致的 native resume 状态字段：
  - `native_resume_available`
  - `native_resume_scope`
  - `resume_boundary`
  - `native_resume_interrupts`
  - `native_resume_has_queue_items`
  - `native_resume_queue_item_count`
- `GraphGatewayNativeStudyLoopResult` 现在从 gateway progress summary 读取这些字段，
  因此命令响应和 `progress.study_loop_result` 使用同一份 graph-owned read-model
  值。
- `start_native_study_product_loop()` 只负责把 gateway result 映射成 API response，
  不在 service 层重新计算 native resume 状态。
- 增加 gateway/API 覆盖，证明多个 runnable datasets 停在 code-review gates 时，
  命令响应和 progress read-model 在 native resume 可用性、边界、queue 可见性和
  queue count 上保持一致。

边界：

- 这是 read-model/API contract 对齐切片。
- 不新增 native resume button、不改变 endpoint 行为、不新增 approval path、不改变
  LLM generation、不改变 R execution，也不启用 durable checkpointer。
- queue 可见性仍只表示“存在可见 review gates”；它不表示默认 memory checkpointer
  下 native resume 可调用。客户端仍必须用 `native_resume_available` 和每个 item 的
  `can_resume` / `resume_endpoint` 判断是否真的可调用 native resume。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets -v
Ran 2 tests in 0.352s - OK

python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 -v
Ran 283 tests in 29.848s - OK (skipped=4)

python -B -m compileall -q src tests
OK

git diff --check
OK; Windows LF/CRLF warnings only.
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - response 字段来自 `progress_summary()` 既有的 `native_resume`
    read-model；
  - `service.py` 只映射 gateway 字段，不重新计算 workflow 或 native resume
    状态；
  - 默认 memory 行为仍清楚：`native_resume_available=false`，但可见 queue items
    的 `can_resume=false`；
  - 测试覆盖 `/runs/native-study-loop` response 字段和
    `progress.study_loop_result` 保持一致；
  - 文档正确说明这是 read-model/API contract 对齐，不是新增 native resume 行为。

### 2026-06-01 - LG2.8 LangGraph-v2 Closeout Audit Document 切片

已完成：

- 新增 `docs/langgraph_v2_closeout_audit.md`，作为当前 LangGraph-v2 的交接审计文档。
- 审计文档按 LG2.0-LG2.8 记录当前状态：
  - graph-state ownership 和 compatibility migration 基本完成；
  - full native product runtime 和 durable checkpointer resume 仍未完成；
  - approvals 后的 multi-dataset execution、repair/spec-revision 闭环、
    standards retrieval、specialist-agent 深化、production sandboxing、legacy
    cleanup 仍属于下一阶段。
- 建议下一阶段命名为 `LG3.0 Native Product Run And Durable Resume`，先在
  `GraphGateway` 后面实现 native runtime，再考虑继续加 UI 控件。

边界：

- 这是 documentation/handoff 切片。
- 不改变 API、UI、graph state transition、checkpointer、LLM call、static rule、
  R execution 或 compatibility shim 行为。

验证：

```text
git diff --check -- docs/langgraph_v2_closeout_audit.md
OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 审计没有夸大 LangGraph-v2 完成度；
  - 正确区分 graph-owned split flow 和真正 durable native LangGraph product
    runtime；
  - 已覆盖 durable full-run resume、multi-dataset execution、repair/spec-revision
    routing、specialist agents、CDISC/P21/company rule assets、production
    sandboxing、legacy stub cleanup 等主要剩余工作。
- 已吸收非阻断措辞建议：R1 现在说明未完成的是 product-default、end-to-end
  native interrupts，同时承认已有 pilot/native-resume work。

## LG3.0 - Native Product Run And Durable Resume

### 2026-06-01 - LG3.0 单 Dataset Native Full-Run 后端契约切片

已完成：

- 新增明确的 `GraphGateway.start_native_dataset_full_run()` 单 dataset 后端契约。
- 新增对应的 `GraphGateway.resume_native_dataset_full_run()` 人工 code-review
  resume 入口。
- 这条契约目前覆盖：
  - 用户提供的 approved input spec 或已批准 draft spec；
  - DatasetGraph 生成 R code；
  - native `code_review` interrupt；
  - 人工 approve/reject；
  - approve 后通过现有 graph-owned R boundary 执行 approved code。
- runtime metadata 现在会记录 `native_dataset_full_run`：
  - `contract=single_dataset_spec_code_review_execute`；
  - `boundary=lg3_backend_contract`；
  - 当前阶段和当前 interrupt；
  - 是否在 approval 后执行。
- 写入 LG3 metadata 时会保留已有 native dataset-loop metadata，避免新的
  contract 记录覆盖底层 LangGraph interrupt 证据。

边界：

- 这是 backend-only 切片。没有新增浏览器 UI 路由，也没有新增 FastAPI endpoint。
- 它还不宣称已经是完整 durable product runtime。默认本地 memory checkpointer
  下，native resume 仍然不是可持久恢复能力。
- 图仍然会停在人工审核 gate。只有明确批准 code review 后，才允许执行 R。
- R 执行仍要求 formal review artifact 和 canonical graph approval 同时存在。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute -v
Ran 3 tests in 0.539s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - 本切片没有宣称 durable full native runtime；
  - 默认 memory mode 仍然没有被描述成可 durable native resume；
  - R 执行仍要求 formal review artifact 和 canonical graph approval；
  - reject 路径不会执行 R，并继续停在 review gate；
  - LG3 metadata 会保留底层 native dataset-loop interrupt 证据；
  - 这是 backend runtime 的推进，不是 UI/read-model polish。
- 非阻断后续建议：
  - 增加 `execute_after_approval=false` 测试，明确 approved but not executed
    的 phase 语义；
  - 增加 approved draft spec 起点测试。

### 2026-06-01 - LG3.0 Native Full-Run Contract 覆盖硬化切片

已完成：

- 按上一轮子 agent 的两个非阻断建议补充聚焦测试：
  - 当 code review 被 approve，但 `execute_after_approval=false` 时，系统记录
    phase 为 `reviewed`，execution 为空，并且不会调用 R execution；
  - 当 LG3 full-run contract 从 graph-approved draft spec 起步时，系统使用
    `spec_source=approved_draft_spec` 并进入 `code_review`。
- 没有修改产品代码。现有 Gateway 和 DatasetGraph 行为已经满足这两类 contract
  场景。
- approved draft spec 测试通过正常 draft-generation 和 draft-review 路径种子状态，
  没有绕过 input-fingerprint 防过期规则。

边界：

- 这是测试和文档硬化切片。
- 不新增 UI/API surface，不改变 runtime 行为，也不放宽 stale draft-spec 保护。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec -v
Ran 5 tests in 0.877s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - approve-but-not-execute 测试确认不会调用 R execution，phase 为
    `reviewed`，code 已批准，execution state 为空，并且没有暗示 completed
    output；
  - approved-draft-spec 测试先走正常 draft generation 和 review 路径，包括
    path/hash/fingerprint 校验，然后才启动 LG3 contract；
  - 文档准确描述这是测试/文档硬化切片。

### 2026-06-01 - LG3.0 Native Resume 保留 Full-Run Contract 切片

已完成：

- 更新 `GraphGateway.resume_native_dataset_interrupt()`：当 code-review interrupt
  属于 LG3 full-run contract 时，改走 `resume_native_dataset_full_run()`。
- 非 LG3 的 native dataset-loop resume 仍然走原有
  `resume_native_dataset_product_loop()` 路径。
- `GraphGatewayNativeDatasetFullRunResult` 现在携带 review `decision`，因此
  explicit native-resume response 可以保持同一套 response shape。
- 新增聚焦测试，证明 explicit native-resume 入口会保留
  `native_dataset_full_run` metadata，记录 phase=`reviewed`，保留 code approval，
  并且在 `execute_after_approval=false` 时不会执行 R。

边界：

- 这不会在默认 memory checkpointer 下启用 native resume。公开 fail-closed gate
  不变。
- 测试里 patch native-resume availability 只是为了覆盖分支，不是 durable restart
  测试。
- 没有新增 UI 或 FastAPI route。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer -v
Ran 7 tests in 1.158s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - explicit resume method 会先检查 `native_interrupt_resume_available()`，所以
    memory mode 仍然 fail closed；
  - LG3 dispatch 只接受 dict metadata、`boundary=lg3_backend_contract` 和
    dataset 匹配；
  - full-run result 增加 `decision` 字段与 native resume response shape 兼容；
  - `execute_after_approval=false` 仍然不会执行 R，并记录 phase=`reviewed`；
  - 文档没有夸大 durable resume 或 UI/API 暴露。

### 2026-06-01 - LG3.0 Full-Run Draft-Spec 续跑切片

已完成：

- 扩展 `resume_native_dataset_full_run()`，让 LG3 单 dataset contract 可以从
  `draft_spec_review` 或 `code_review` 两种 gate 继续。
- 当第一站是 `draft_spec_review` 时，批准后会先写入正式 draft-spec review
  artifact，再继续 dataset product loop，并停在下一站 `code_review` interrupt。
- draft-spec review 被拒绝时仍停在 draft-spec gate，不生成 R 代码。
- native resume API request 现在可以携带和 native study-loop start 相同的 LLM
  provider/exposure overrides，方便后续 durable resume 在 draft approval 后继续
  code generation，而不是让 service 层自己决定 workflow。
- LG3 full-run metadata 只保存可审计且不含密钥的 LLM provider payload。它会记录
  是否提供过 API key，但不会把 key 写入 `graph_state.json`。

边界：

- 这仍然不会在默认 memory checkpointer 下启用 native resume。fail-closed gate
  保持不变。
- 这是 backend/API contract 工作，没有修改 UI flow。
- 除非显式配置 persistent LangGraph checkpointer，否则不宣称 durable restart
  recovery。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_requires_llm_config tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer -v
Ran 9 tests in 1.604s - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_native_resume_endpoint_passes_llm_config_for_draft_continuation tests.test_api_phase8.Phase8ApiTests.test_native_resume_endpoint_fails_closed_without_durable_checkpointer -v
Ran 2 tests in 0.208s - OK

python -B -m compileall -q src tests
OK
```

子 agent 审查：

- 2026-06-01，Gibbs，`gpt-5.5`，只读审查结论：GO。
- 它确认：
  - memory checkpointer 模式仍会在 native resume dispatch 之前 fail closed；
  - draft-spec 续跑仍然经过正式 draft-spec review artifact、hash 和
    input-fingerprint 检查，然后才生成代码；
  - `native_dataset_full_run` metadata 会跨 draft-to-code 和 code-review resume
    保留；
  - provider metadata 只持久化脱敏后的 secret-presence 字段，不写 raw API key；
  - 文档没有在缺少 persistent checkpointer 时宣称 durable resume。
- 根据审查建议已补两点：
  - service native-resume 现在会先检查 gateway native-resume capability，再解析
    LLM config；
  - provider audit redaction 现在覆盖常见 key/token/secret 字段，不只处理
    `api_key`。

### 2026-06-01 - LG3.0 Full-Run Terminal-Failure 边界切片

已完成：

- 扩展 `resume_native_dataset_full_run()` 的 code-review approval 路径：如果批准后
  R 执行返回 terminal failure，LG3 full-run result 和
  `native_dataset_full_run` metadata 都记录 phase=`terminal_failure`。
- 当 terminal failure review 被正式 triage 后，`review_terminal_failure()` 会保留
  LG3 full-run contract metadata，并把 phase 更新为
  `terminal_failure_triaged`。
- triage metadata 会记录：
  - `boundary=lg3_backend_contract`；
  - `last_interrupt=terminal_failure`；
  - 人工 decision，例如 `repair_code`；
  - 下一步受控动作，例如 `repair_generated_code`；
  - `terminal_failure=true`。
- 新增聚焦测试覆盖从 LG3 full-run code approval 到 R terminal failure，再到正式
  terminal-failure review 的完整 metadata 边界。

边界：

- 这不是 native terminal-failure durable resume，也不是 repair loop。
- LG3 full-run execution failure 当前仍通过 Gateway-owned terminal-failure review
  记录正式人工 triage。
- 后续若要实现真正闭环，需要继续做 repair-code / revise-spec 的 native loop。
- 没有新增 UI/API surface。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_terminal_failure_records_contract_boundary -v
Ran 1 test in 0.276s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_requires_llm_config tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_terminal_failure_records_contract_boundary tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer -v
Ran 10 tests in 1.801s - OK
```

子 agent 审查：

- 2026-06-01，Gibbs，只读审查结论：GO。
- 它确认：
  - LG3 terminal-failure metadata 受 `_has_native_dataset_full_run_contract()`
    限制，不污染普通 native dataset loop；
  - formal triage 仍先通过既有 `record_terminal_failure_review()` 路径写入，
    LG3 metadata 只是保留/更新 full-run contract 审计信息；
  - 文档没有把本切片说成 repair loop 或 native terminal-failure durable
    resume。
- 已吸收它的非阻断建议：普通非 LG3 `review_terminal_failure()` 回归测试现在断言
  不会创建 `native_dataset_full_run` metadata。

### 2026-06-01 - LG3.0 Repair/Revise Follow-Up Contract 连续性切片

已完成：

- 修复 `start_native_dataset_full_run()` 在 terminal-failure triage 后重新进入
  full-run contract 时重写 metadata 的问题。
- 新增 `_native_dataset_full_run_metadata()` helper：
  - 只在同 dataset 且 `boundary=lg3_backend_contract` 时继承旧 LG3 metadata；
  - 保留上一轮 terminal-failure triage 的 `last_interrupt`、`decision`、
    `terminal_failure` 等审计字段；
  - 覆盖当前阶段的 `phase`、`current_interrupt`、provider/exposure audit payload。
- 新增 continuation guard：只有旧 LG3 metadata 明确来自 terminal failure，且人工
  decision 是 `repair_code` 或 `revise_spec` 时，才标记
  `repair_or_revision_continued=true`。
- `repair_code` follow-up 现在可以再次进入 LG3 full-run，并停在
  `code_review`，同时保留 `native_dataset_full_run` contract continuity。
- `revise_spec` follow-up 现在可以再次进入 LG3 full-run，并停在
  `draft_spec_review`，同时把旧 code 标记为 stale，并保留 contract continuity。
- 普通 LG2 native dataset loop 不受影响，仍使用既有
  `native_dataset_product_loop_*` metadata。

边界：

- 这不是自动修复执行，也不是自动 spec revision 完成。
- 本切片只保证用户已经 triage 为 `repair_code` 或 `revise_spec` 后，再次进入
  LG3 full-run 时不会丢失上一轮 full-run 审计边界。
- 它不改变默认 memory checkpointer 的 fail-closed durable-resume 边界。
- 没有新增 UI/API surface。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_metadata_ignores_foreign_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_followup_preserves_contract -v
Ran 3 tests in 1.155s - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_respects_repair_code_terminal_followup tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_routes_revise_spec_followup_to_draft_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_requires_llm_config tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_terminal_failure_records_contract_boundary tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_metadata_ignores_foreign_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer -v
Ran 15 tests in 3.441s - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 149 tests in 14.410s - OK, skipped=4 optional SQLite checkpointer tests

python -B -m unittest tests.test_api_phase8 -v
Ran 148 tests in 21.153s - OK

python -B -m compileall -q src tests
OK

git diff --check
OK, CRLF warnings only
```

子 agent 审查：

- 2026-06-01，Gibbs，只读审查结论：GO，无 P0/P1/P2。
- 它确认：
  - `native_dataset_full_run` metadata 只在同 dataset 且
    `boundary=lg3_backend_contract` 时继承；
  - `repair_code` 会回到 `code_review`，`revise_spec` 会回到
    `draft_spec_review`，且旧 code 会 stale；
  - 文档没有把本切片说成自动 repair execution、完成 spec revision 或 durable
    native resume。
- 已吸收非阻断建议：
  - 新增回归测试，证明其他 dataset 的 LG3 metadata 或非 LG3 boundary metadata
    不会被当前 dataset 继承；
  - 同一测试确认 follow-up re-entry 的 provider audit 不持久化 raw API key。

### 2026-06-01 - LG3.0 Follow-Up Context 贯穿 Resume/Execution 切片

已完成：

- 新增 `_native_dataset_full_run_terminal_followup_context()`，把最初
  terminal failure 后用户选择 `repair_code` 或 `revise_spec` 的原因保存成受控
  audit context。
- `start_native_dataset_full_run()` 在 terminal-failure triage 后重新进入
  full-run 时，会记录 `terminal_failure_followup`。
- `resume_native_dataset_full_run()` 不再手写一份新的
  `native_dataset_full_run` metadata，而是统一通过
  `_native_dataset_full_run_metadata()` 生成。这样 repair/revise 后继续到下一次
  人工 gate，或修复后成功执行 R，都不会丢失最初为什么进入 follow-up 的上下文。
- 新增测试覆盖：
  - `repair_code` follow-up -> `code_review` -> 批准修复代码并成功执行；
  - `revise_spec` follow-up -> `draft_spec_review` -> 批准 draft spec 后进入下一站
    `code_review`。

边界：

- 这仍然只是后端 contract continuity。
- 它不会自动执行 repair，不会自动完成 spec revision，也不会在默认 memory
  checkpointer 下启用 durable restart recovery。
- `terminal_failure_followup` 是历史上下文，不能替代当前的 `phase`、
  `current_interrupt` 或当前 `next_action`。
- 已修复审查发现的 stale action 风险：当 full-run 已执行完成且没有当前
  interrupt 时，LG3 metadata 顶层会清掉 `next_action`；旧的 repair/revision
  动作只保留在 `terminal_failure_followup` 里作为审计上下文。
- 没有新增 UI/API surface。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_execution_keeps_followup_context tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_draft_review_keeps_followup_context tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_followup_preserves_contract -v
Ran 4 tests - OK

python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_respects_repair_code_terminal_followup tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_product_loop_routes_revise_spec_followup_to_draft_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_starts_at_code_review_gate tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_executes tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_approval_can_pause_before_execution tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_resume_entrypoint_preserves_full_run_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_reject_does_not_execute tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_uses_approved_draft_spec tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_continues_to_code_review tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_dataset_full_run_draft_approval_requires_llm_config tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_terminal_failure_records_contract_boundary tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_followup_preserves_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_metadata_ignores_foreign_contract tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_repair_execution_keeps_followup_context tests.test_graph_gateway.GraphGatewayTests.test_gateway_lg3_native_full_run_revise_draft_review_keeps_followup_context tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_dataset_resume_fails_closed_without_durable_checkpointer -v
Ran 17 tests - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 151 tests - OK, skipped=4 optional SQLite checkpointer tests

python -B -m unittest tests.test_api_phase8 -v
Ran 148 tests - OK

python -B -m compileall -q src tests
OK
```

子 agent 审查：

- 2026-06-01，Mencius，`gpt-5.5`，只读审查先发现一个 P1：
  修复代码成功执行后，`native_dataset_full_run.next_action` 顶层字段可能保留旧的
  `code_review`，误导直接读取 contract metadata 的调用方。
- 已修复：`_native_dataset_full_run_metadata()` 在 `current_interrupt is None` 时会
  清掉顶层 `next_action`。
- 已补回归断言：修复后 executed 状态不再保留顶层 `next_action`，但历史
  `terminal_failure_followup.next_action` 仍作为审计上下文保留。
- 复审结论：GO。Mencius 确认 P1/P2 已关闭，没有发现 metadata 污染、当前动作
  误导、provider 脱敏、durable-resume 措辞或 LG3 contract 边界上的剩余阻断问题。

### 2026-06-01 - LG3.0 单 Dataset Full-Run API 入口切片

已完成：

- 为已有的 LG3 单 dataset full-run 后端 contract 增加一个很窄的 FastAPI/service
  入口：
  `POST /runs/{run_id}/datasets/{dataset}/native-full-run`。
- 新增 `NativeDatasetFullRunStartRequest` 和
  `NativeDatasetFullRunStartResponse`。
- 新 route 通过现有 service gateway factory/context helper 委托
  `GraphGateway.start_native_dataset_full_run()`，不在 service 层自建工作流状态。
- response 只报告第一站 graph-owned 人工 gate：
  - 当前 interrupt；
  - 下一步动作；
  - generated code path；
  - static-check path；
  - 如有需要，draft-spec path；
  - graph/workflow projection 路径。
- 新增 API 测试，证明 endpoint 会把 ADAE 启动到 `code_review`，写入
  `native_dataset_full_run` contract metadata，并且不会执行 R。
- 新增 service 测试，证明 LLM provider/exposure overrides 会传入 gateway，而
  service 不会分叉出另一条 workflow 路径。

边界：

- 这不是浏览器 UI 改动。
- 这不是 durable native resume 启用；默认 memory checkpointer 下
  `native-resume` 的 fail-closed 行为不变。
- 这不会批准代码、执行 R，也不会自动完成 repair/revision。
- 它只是把已经实现的 LG3 后端 contract 暴露成一个窄 API 边界，方便后续 UI
  调用一个 graph-owned 产品入口，而不是继续在浏览器里重建 split-flow 编排。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_native_full_run_endpoint_starts_single_dataset_at_code_review tests.test_api_phase8.Phase8ApiTests.test_native_full_run_service_delegates_to_gateway_with_llm_config -v
Ran 2 tests - OK

python -B -m unittest tests.test_api_phase8 -v
Ran 150 tests - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 151 tests - OK, skipped=4 optional SQLite checkpointer tests

python -B -m compileall -q src tests
OK
```

子 agent 审查：

- 2026-06-01，Confucius，`gpt-5.5`，只读审查结论：GO。
- 没有 P0/P1 阻断问题。
- 没有阻断性 P2；唯一小建议是 service 单测使用 `SimpleNamespace` fake，只能证明
  service 提取字段和 gateway 委托，canonical state 的更强证明来自 endpoint
  integration test 和既有 gateway tests。
- 审查确认：
  - 新 endpoint 是窄委托，没有在 service 层自建 workflow；
  - 没有 durable-resume、auto-approval 或 auto-execution 误导；
  - response 字段来自 gateway-owned state/projection；
  - 测试覆盖 contract metadata、无 R 执行和 LLM override 传递；
  - 没有新增 UI 暴露。

### 2026-06-01 - LG3.1 UI Generate 接入 Native Full-Run 切片

已完成：

- 浏览器里的 `Generate R Code` 按钮现在优先调用
  `POST /runs/{run_id}/datasets/{dataset}/native-full-run`。
- 用户看到的按钮语义保持不变：生成后仍停在人工 review gate，不自动批准、不执行 R。
- UI 根据 native full-run 返回的当前 interrupt 分两种展示：
  - `draft_spec_review`：显示 draft spec review pane，等待人工批准 draft spec；
  - `code_review`：刷新 graph state/progress/review-summary，加载生成的 R code，等待人工批准 code。
- `draft_spec_review` 现在被当作 graph gate，而不是浏览器本地 generated-code cache
  状态。只要当前 graph state/progress 说明目标 dataset 处在 draft-spec review，
  UI 就会清掉该 target 的旧 generated-code cache，并阻止 review-summary 把旧 code
  恢复成可审核代码。
- 保留 `revise_approved_spec` 场景下的旧 `draft-spec` 入口，因为它是 terminal-failure
  后要求重新起草 spec 的独立人工闸门。
- 旧 `generate-code` API 没有删除，仍作为 backend compatibility 和回归测试入口存在；
  但主 UI 的普通生成动作已经不再直接调用它。

边界：

- 这不是 UI 自动运行整个 dataset。
- 这不是 durable native resume 启用；默认 memory checkpointer 下 native resume 仍 fail closed。
- 这不会批准 draft spec、批准 code、执行 R 或自动 repair。
- 这只是把单 dataset 的第一个产品入口从浏览器 split-flow generate step 移到
  LG3 graph-owned full-run contract。

验证：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_index_primary_actions_follow_graph_progress_next_action tests.test_api_phase8.Phase8ApiTests.test_index_generate_button_starts_native_full_run tests.test_api_phase8.Phase8ApiTests.test_index_native_full_run_clears_stale_code_when_new_artifact_text_missing tests.test_api_phase8.Phase8ApiTests.test_index_generate_button_handles_native_full_run_draft_gate tests.test_api_phase8.Phase8ApiTests.test_index_native_full_run_draft_gate_clears_stale_generated_code tests.test_api_phase8.Phase8ApiTests.test_index_native_full_run_draft_gate_points_to_draft_review -v
Ran 6 tests - OK
```

子 agent 审查：

- Peirce 第一次只读审查返回 NO-GO，发现一个 P2：当 native full-run 返回
  `draft_spec_review` 且没有 `code_path` 时，浏览器里旧的
  `state.generatedByDataset[target]` 可能残留，导致 UI 继续显示 code-review pane。
- 已修复：让 `draft_spec_review` 成为 `applyGraphState()`、
  `graphAllowsReviewSummaryCodeRecovery()` 和 `renderPane()` 都必须服从的 graph gate。
  新增回归测试覆盖 native full-run 返回 draft review、浏览器已有旧 generated-code
  cache、review-summary 也含旧 code 的场景。
- Dewey 后续只读复审返回 GO，没有 P1/P2 阻断问题。

### 2026-06-02 - LG3.1 Study Loop 接入 Native Full-Run Dispatch 切片

已完成：

- study-level 的 `start_native_study_product_loop()` 现在会把每个 runnable
  dataset 交给 `start_native_dataset_full_run()`，不再从 study-loop 直接走低层
  native dataset product-loop 入口。
- 用户侧 API 不变：`/runs/native-study-loop` 仍然返回每个 dataset 当前需要处理的
  人工闸门，例如 `review_draft_spec` 或 `review_code`。
- study-loop 的 runtime metadata 现在记录
  `boundary=study_lg3_full_run_dispatch`，并增加 `full_run_datasets` 摘要。
  这样多 dataset dispatch 有 study-level 的 LG3 合同记录。
- 单 dataset 的 `native_dataset_full_run` metadata 仍然表示当前或最后启动的
  dataset 合同；它不是 study-level 多 dataset dispatch 的唯一依据。
- 重新启动 study-loop 时，如果某个 dataset 已经停在 review gate，系统仍然会跳过它，
  不会重新生成或覆盖人工审核中的进度。

边界：

- 这不是自动完成整个批处理。
- 这不会自动批准 draft spec、批准 code、执行 R、repair code 或 revise spec。
- 这不会在默认 memory checkpointer 下启用 durable native resume。
- 旧的低层 native dataset product-loop 入口仍作为 backend regression/pilot 边界保留；
  但 study-level 产品入口已经改为通过 LG3 dispatch。

验证：

```text
python -B -m unittest tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_preserves_mixed_spec_gates tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_restart_preserves_spec_gap_review_resume tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_skips_existing_review_progress_on_restart tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_new_target_while_preserving_existing_progress tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_downstream_after_graph_output_dependency -v
Ran 6 tests - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_starts_multiple_runnable_datasets tests.test_api_phase8.Phase8ApiTests.test_native_full_run_endpoint_starts_single_dataset_at_code_review tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_reports_preserved_progress_on_restart tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_does_not_start_dependency_blocked_targets -v
Ran 4 tests - OK

python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_native_study_loop_endpoint_preserves_full_run_draft_warnings tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_does_not_start_dependency_blocked_targets tests.test_graph_gateway.GraphGatewayTests.test_gateway_native_study_product_loop_starts_multiple_runnable_datasets -v
Ran 3 tests - OK

python -B -m unittest tests.test_graph_gateway -v
Ran 151 tests - OK, skipped=4 optional SQLite checkpointer tests

python -B -m unittest tests.test_api_phase8 -v
Ran 155 tests - OK
```

子 agent 审查：

- 2026-06-02，Leibniz，`gpt-5.5`，只读审查先发现一个 P1：
  study-loop API result 可能丢掉 full-run warnings，因为
  `GraphGatewayNativeDatasetFullRunResult` 没有携带低层 warnings/dependency warnings。
- 已修复：LG3 full-run result 现在携带 `warnings` 和 `dependency_warnings`；
  新增 API 回归测试覆盖 missing-spec draft warnings 不丢失。
- 同时修复两个相关 P2：
  - API path 字段会把空字符串归一成 `None`；
  - study-loop metadata 新增 `dispatch_status`，dependency-blocked 且没有启动 dataset
    的场景会明确记录为 `no_dataset_dispatched`。
