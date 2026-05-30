# LangGraph-2 第二阶段施工方案

这份文档是 ADaM Agent Studio 在 Phase 0-8 MVP 之后的第二阶段施工方案。它把目前已经发现的架构偏差和未完成事项，整理成一套系统性工程计划。

LangGraph-2 的核心原则：

> 不要在现有 FastAPI service flow 旁边再叠一层补丁。要把真实业务流程迁入一个由 LangGraph 控制的状态机，让 FastAPI/UI 变成这个状态机的入口和展示层。

## 1. 当前基础

当前已经有价值的资产：

- `src/adam_agent/api/service.py` 里有目前最完整的真实流程：依赖计划、确认输入、draft spec、draft spec 审核、生成代码、代码审核、执行已审核 R、验证、比较、下载。
- `src/adam_agent/graph/study_graph.py` 已有 study 级依赖计划、dataset 批次、分发、汇总和 study audit manifest。
- `src/adam_agent/graph/dataset_graph.py` 已有 dataset 图骨架、LLM downstream 执行模式、失败路由和结果汇总。
- `src/adam_agent/graph/workflow_state.py` 目前仍会持久化兼容用的
  `workflow_state.json` read model 和 SQLite sidecar history，供当前 UI/API
  展示层使用。
- `src/adam_agent/llm/`、`src/adam_agent/downstream/`、`src/adam_agent/tools/` 已经形成了可复用的工具边界。
- ADSL 已修正为和其他 AD target 一样走统一 ADaM 产品流程。旧的 `src/adam_agent/adsl/` 只作为 legacy/regression 保留。

当前架构偏差：

- 真实 human-in-the-loop 流程主要还在 FastAPI service 函数里，不在 LangGraph `interrupt` 节点里。
- `workflow_state.json` 仍作为 UI/API 兼容 read model 存在，但产品事实来源正在
  收敛到 canonical `graph_state.json`；剩余的直接兼容写入都应视为要移除或隔离
  的 legacy surface。
- `DatasetGraph` 里仍有早期 `*_stub` 节点，图还不是完整产品工作流。
- 当前产品更像“受控流水线 + LLM 调用”，还不是明确专家角色分工的多智能体图。
- UI 的 dataset card 和 target 切换仍更像单 target 控制器，不像多个持久 dataset run 的图状态视图。
- Static ADaM/CDISC 检查现在已有有限范围的 policy-driven gate，但还不是完整规则引擎。
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
  - service wrapper 仍会把 graph state 映射回现有 UI response model 和 `workflow_state.json` projection，以保持 UI 兼容。
- 仍待完成：
  - 从 `code_review` graph-native resume 到 R execution。
  - graph-native validation、compare、terminal failure routing 和 repair。
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

加入 standards-aware 检查边界，但不伪装成生产完整。静态检查必须是通用
policy 检查，不能写成针对 demo 或某个 ADaM 数据集的补丁规则。

设计原则：

- 静态检查是 policy/rule-pack 层，不是不断追加 PSY201、ADAE、ADSL 或某个
  demo 变量特例的补丁清单。
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
- 给未来每个 static-check PR 增加 rule-design review checklist：
  - 这条规则检查的是哪个已经声明的 contract？
  - 规则权威来自哪里：system contract、approved spec、user policy，还是
    versioned rule pack？
  - 这条规则是否不依赖 demo-study 名和某个文件观察？
  - 如果它能 block run，authority_type、source、version、scope、severity、
    evidence 记录在哪？
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
- dependency-plan gate 仍由 compatibility service 在调用 gateway product method
  前显式触发。后续切片应把这个 gate 移入 GraphGateway composite entrypoints，
  让 service wrappers 继续变薄。
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

当前边界：

- 本切片不改变 compare algorithm。它仍然只是初始 CSV 结构和 sampled-cell
  comparison，不是 clinical conformance validator。
- Reference ADaM 仍然只是 compare/output-shape evidence。本切片不把 reference
  ADaM 变成 derivation authority。
- 静态检查继续只做 generic contract/rule-pack checks。本切片不增加任何
  clinical、dataset-specific、study-specific 或 demo-specific rule。

Focused verification：

```text
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_generate_review_execute_split_flow tests.test_api_phase8.Phase8ApiTests.test_compare_does_not_create_graph_state_without_prepared_run tests.test_api_phase8.Phase8ApiTests.test_review_summary_updates_graph_compare_when_reference_disappears tests.test_graph_gateway.GraphGatewayTests.test_gateway_records_compare_summary_in_canonical_state tests.test_graph_gateway.GraphGatewayTests.test_gateway_writes_compare_report_artifact_when_requested tests.test_graph_gateway.GraphGatewayTests.test_gateway_compare_requires_existing_graph_state -v
python -B -m unittest tests.test_graph_gateway tests.test_api_phase8 tests.test_graph_smoke tests.test_static_rules tests.test_reference_store -v
```

结果：6 focused compare tests passed；178 related gateway/API/graph/static/
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
- dependency-plan gate 本切片仍留在 service compatibility wrapper。
- `finalize-inputs` 仍由 service 直接调用 `DatasetGraph`，它是剩下的主要
  dataset product step migration。
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
- dependency-plan gate 本切片仍留在 service compatibility wrapper。
- 独立 `/draft-spec` endpoint 仍保留自己的 draft generation service flow，并通过
  `GraphGateway` 记录结果；为了保持本切片范围可控，暂未迁移它。
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
- 增加 terminal-failure retry 正向回归：人工选择 `retry_execution` 后，
  `/execute-approved-code` 可以进入 graph-owned `graph_product_execute` 路径，
  并在 graph state 中记录 retry follow-up 已由 execute 消费。

当前边界：

- static-rule guard 只针对 generic engine。dataset 名和 standards 术语仍可以
  出现在测试或未来带版本的 rule-pack fixtures 中。
- retry 回归使用 mocked DatasetGraph return value，证明的是 compatibility
  wrapper gate 和 graph-state recording path，不是真实 R 执行。

验证：

```text
python -B -m unittest tests.test_static_rules -v
python -B -m unittest tests.test_api_phase8.Phase8ApiTests.test_execute_requires_terminal_failure_review_before_retry tests.test_api_phase8.Phase8ApiTests.test_retry_execution_review_allows_approved_code_execution_path tests.test_api_phase8.Phase8ApiTests.test_terminal_failure_retry_execution_does_not_unlock_code_regeneration -v
python -B -m unittest tests.test_agents_contract tests.test_llm_context tests.test_prompt_compaction tests.test_downstream_runner tests.test_graph_smoke tests.test_api_phase8 tests.test_graph_gateway tests.test_state_schemas tests.test_llm_generated_code tests.test_static_rules tests.test_sandbox -v
```

结果：14 static-rule tests passed；3 focused retry tests passed；205 core tests
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
