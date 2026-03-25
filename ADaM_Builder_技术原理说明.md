# ADaM Builder 技术原理说明文档

## 1. 项目概述

ADaM Builder 是一个基于 R Shiny 的交互式 ADaM 自动化生成平台，目标是将用户上传的 SDTM 域数据与 Analysis Specification 结合，通过大语言模型辅助生成可执行的 R 代码，并在人工审阅确认后执行，输出目标 ADaM 数据集。

从系统定位上看，它不是单纯的“代码生成器”，而是一个围绕临床数据编程工作流构建的交互式工作台，覆盖了输入准备、规格解析、模型生成、人工审阅、结果执行与输出展示等多个环节。

项目主入口位于 [app.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\app.R)，前端界面定义在 [ui.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\ui.R)，后端主流程位于 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R)。

## 2. 项目目标与使用场景

该系统解决的问题主要包括：

- ADaM 构建过程依赖人工理解 Spec 和 SDTM 结构，重复劳动较多
- 不同项目中的 Spec 格式可能不统一，增加了前处理成本
- LLM 生成代码时若缺乏结构化输入和风险提示，结果不可控
- 代码生成、审查、执行与结果查看往往分散在不同工具中

为此，系统提供以下能力：

- 接收多个 SDTM 域 CSV 文件
- 接收一个或多个 Analysis Spec CSV 文件
- 自动识别 Spec 列结构并生成解析报告
- 将已确认的 Spec 与 SDTM 数据摘要共同送入 LLM
- 输出结构化风险日志和可编辑 R 代码
- 支持人工审阅后执行代码并提取 ADaM 数据集
- 动态展示和下载生成结果

## 3. 项目结构与模块职责

项目采用较为紧凑的单目录结构，核心逻辑集中在根目录若干 R 文件中。

### 3.1 应用入口

[app.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\app.R)

职责包括：

- 自动检查并安装缺失依赖
- 加载运行所需 R 包
- `source()` 引入各功能模块
- 调用 `shinyApp()` 启动应用

该文件还在 `onStart` 阶段输出 LLM 当前模式信息，用于提示是 Mock 模式还是真实 API 模式。

### 3.2 前端界面模块

[ui.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\ui.R)

职责包括：

- 定义整体页面布局
- 提供 SDTM 文件上传区域
- 提供 Analysis Spec 上传区域
- 提供 LLM API 配置区域
- 提供流水线状态展示
- 提供三大主页面签：
  - 运行与风险日志
  - 代码审查与回档
  - 输出数据集

前端采用 `bslib` 深色主题，并使用 `DT`、`shinyAce`、`shinyjs` 等组件实现交互能力。

### 3.3 后端主控模块

[server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R)

这是系统的核心编排层，负责：

- 管理响应式状态池
- 接收文件上传事件
- 解析 Spec CSV
- 校验生成前置条件
- 调用 LLM 引擎
- 将生成代码注入编辑器
- 接收用户执行请求
- 在独立环境中运行代码
- 提取并渲染 ADaM 结果数据集

### 3.4 数据工具模块

[data_utils.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\data_utils.R)

职责包括：

- 统一读取 SDTM CSV
- 清洗日期列中的 Excel 前导单引号
- 生成适合传给 LLM 的摘要文本
- 从已解析 Spec 中推断所需 SDTM 域
- 提供 Study Day 计算辅助函数

### 3.5 LLM 适配模块

[llm_api.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\llm_api.R)

职责包括：

- 根据模型名推断提供商
- 统一构建 Prompt
- 发起实际 HTTP API 请求
- 支持多提供商故障转移
- 解析 LLM JSON 返回结果
- 在 Mock 模式下返回演示用代码与风险日志

### 3.6 配置注册表模块

[provider_registry.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\provider_registry.R)  
[domain_registry.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\domain_registry.R)

其中：

- `provider_registry.R` 维护 LLM 提供商配置
- `domain_registry.R` 维护支持的 SDTM 域定义、分组与必选属性

这类设计使系统具有明显的“配置驱动”特征。

## 4. 系统整体架构

从运行时视角，系统可以划分为五层：

1. 展示层  
   由 `ui.R` 定义，负责接收用户输入并展示状态、代码与结果。

2. 编排层  
   由 `server.R` 驱动，负责状态管理和工作流推进。

3. 数据处理层  
   由 `data_utils.R` 提供数据读取、清洗、摘要与域推断能力。

4. LLM 接入层  
   由 `llm_api.R` 与 `provider_registry.R` 提供 Prompt 构建、模型路由和故障转移。

5. 配置驱动层  
   由 `domain_registry.R` 和 Provider 注册表共同提供动态 UI 和动态执行所需的元信息。

可用如下逻辑图概括：

```text
用户上传文件/配置参数
        |
        v
Shiny UI
        |
        v
Server 响应式编排
        |
        +-- Spec 解析
        +-- SDTM 数据读取与摘要
        +-- LLM 代码生成
        +-- 人工审阅
        +-- 执行与结果提取
```

## 5. 主业务流程

系统的主业务流程如下：

1. 用户选择需要使用的 SDTM 域
2. 用户上传对应 SDTM CSV 文件
3. 用户上传一个或多个 Analysis Spec CSV
4. 系统自动进行 Spec 列识别与解析报告生成
5. 用户确认解析结果
6. 用户选择模型并填写 API Key 或本地推理参数
7. 用户点击“生成 ADaM 与代码”
8. 系统读取 SDTM 数据并生成摘要
9. 系统将 Spec 与数据摘要发送给 LLM
10. LLM 返回结构化 JSON，包括 `r_code` 和 `risk_logs`
11. 系统展示风险日志并把代码填入编辑器
12. 用户人工审查代码并决定是否执行
13. 系统执行代码并提取目标数据集
14. 系统在输出页签中展示并支持下载结果

这一流程体现出明显的“生成前确认”和“执行前确认”双层控制。

## 6. 响应式状态模型

在 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R) 中，系统使用 `reactiveValues` 保存全局运行时状态。核心状态包括：

- `sdtm`：已读取的 SDTM 数据列表
- `file_meta`：各上传文件的元信息
- `specs`：已上传 Spec 文件及其解析结果
- `spec_confirmed`：Spec 是否已被用户确认
- `llm_result`：LLM 返回结果
- `risk_logs_df`：结构化风险日志数据框
- `original_code`：原始 LLM 生成代码
- `adam_datasets`：最终提取出的 ADaM 数据集列表
- `step_parse`、`step_load`、`step_llm`、`step_review`、`step_run`：各阶段状态
- `log_lines`：实时日志文本

这表明系统的核心不是多个独立函数，而是一个由状态驱动的前后端交互流程。

## 7. Spec CSV 智能解析机制

Spec CSV 解析是当前版本最关键的增强功能之一。

### 7.1 目标

系统不再要求用户预先准备严格格式的 JSON 规格文件，而是允许上传一个或多个 CSV 规格文件，并自动识别列名结构。

### 7.2 标准字段

系统内部期望的标准字段包括：

- `variable`
- `label`
- `type`
- `source`
- `derivation`
- `dataset`

其中前五项可以视为核心变量级元数据，`dataset` 用于标识目标数据集名称。

### 7.3 解析过程

解析流程由 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R) 中以下逻辑组成：

- `observeEvent(input$file_spec, ...)`  
  监听用户上传的一个或多个 Spec CSV。
- `.heuristic_col_match()`  
  使用正则关键词对 CSV 列名进行启发式匹配。
- `.call_llm_spec_parser()`  
  对低置信字段进行 LLM 辅助补全。
- `.check_row_completeness()`  
  检查逐行完整性，识别缺失的 `source` 或 `derivation` 等字段。
- `.build_multi_parse_modal()`  
  构造多文件解析报告 Modal。
- `observeEvent(input$btn_confirm_spec, ...)`  
  在用户确认后将解析结果固化为内部标准对象。

### 7.4 解析报告内容

解析报告包含以下信息：

- 文件级匹配状态
- 每个标准字段对应的实际 CSV 列名
- 匹配置信度
- LLM 补全建议
- 行级风险
- 前 3 行数据预览
- 低置信字段警告

因此，该解析过程不是黑盒式自动映射，而是带有人工审核入口的半自动机制。

## 8. SDTM 数据读取与预处理

[data_utils.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\data_utils.R) 提供了统一的数据处理逻辑。

### 8.1 统一字符型读取

`load_sdtm_data()` 将所有列按字符型读取，以避免 `readr` 或 `read.csv` 的自动类型推断对临床日期、编码和字符字段造成歧义。

### 8.2 日期字段清洗

系统通过 `strip_excel_apos()` 去除日期字符串前的单引号，例如：

- `'2023-01-15` -> `2023-01-15`

这一处理针对 Excel 导出常见问题。

### 8.3 Study Day 计算辅助

`dy_char()` 按 CDISC 规则计算研究日：

```text
Study Day = (目标日期 - 参考日期) + 1
```

### 8.4 数据摘要生成

`summarize_sdtm()` 会将 SDTM 列表转换为 LLM 可消费的摘要文本，包括：

- 域名
- 行数
- 列名
- 前 3 行预览

此外，Prompt 构建时还会附加前 5 行的 CSV 预览，用于辅助字段推断。

## 9. 动态 SDTM 域管理机制

[domain_registry.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\domain_registry.R) 定义了当前支持的 SDTM 域及其分组。

### 9.1 域分组

系统将域分为三类：

- `core`
- `basic`
- `extended`

例如：

- `dm`、`ex`、`ae` 属于核心域
- `vs`、`lb`、`cm`、`mh`、`sv` 属于基础域
- `eg`、`pe`、`tu`、`rs`、`mb` 属于扩展域

### 9.2 动态 UI

在 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R) 中：

- `output$sdtm_domain_selector` 动态生成域选择控件
- `output$sdtm_upload_panel` 动态生成上传控件
- `rv$active_domains` 决定当前活跃域
- `.get_required_domain_ids()` 确保必选域始终存在

这意味着系统在新增域支持时具有较好的扩展性。

## 10. LLM Prompt 设计与输出协议

### 10.1 Prompt 输入结构

[llm_api.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\llm_api.R) 中的 `.build_prompts()` 由三部分构成用户上下文：

1. ADaM 变量规格
2. 各 SDTM 域前 5 行样本预览
3. SDTM 数据摘要

这是一种“规格 + 样本 + 摘要”的三段式输入方式。

### 10.2 系统级约束

Prompt 明确要求模型：

- 生成完整可运行的 R 代码
- 必须生成指定名称的数据框对象
- 使用既定包和既定日期规范
- 将所有推断写入 `risk_logs`
- 只能输出 JSON，不得输出额外文字

### 10.3 输出 JSON 协议

系统要求模型只返回以下结构：

```json
{
  "r_code": "完整可运行的 R 代码字符串",
  "risk_logs": [
    {
      "level": "WARNING 或 INFO 或 ERROR",
      "variable": "变量名",
      "description": "推断说明",
      "assumption": "需人工确认的假设"
    }
  ]
}
```

该协议是后续执行链条的基础。

## 11. 多提供商接入与故障转移

[provider_registry.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\provider_registry.R) 定义了 7 类提供商：

- OpenAI
- Anthropic
- DeepSeek
- Kimi
- Qwen
- Ollama
- vLLM

### 11.1 提供商推断

`.infer_provider()` 根据模型名称前缀推断对应提供商。

### 11.2 统一调用路径

`.call_real_api()` 根据 Provider 配置决定：

- base URL
- 认证方式
- 是否支持 JSON mode
- 是否为 Anthropic 风格消息结构

### 11.3 故障转移

`call_llm_engine_with_failover()` 支持按顺序尝试主提供商和备用提供商，直到成功或全部失败。

### 11.4 错误翻译

`.handle_http_error()` 对以下问题给出更可读提示：

- 本地服务不可连接
- API Key 无效
- 速率限制
- 请求格式错误
- 超时

这说明系统在 LLM 接入上具有一定工程化健壮性。

## 12. 风险日志机制

系统将 LLM 的不确定性显式转为结构化风险日志。

风险日志包含：

- 级别：`ERROR`、`WARNING`、`INFO`
- 变量名
- 推断描述
- 需确认的假设

在 UI 中，风险日志会以表格形式展示，并支持按级别筛选。这一设计使 LLM 的“隐含推断”转变为用户可审阅的显式对象。

## 13. 代码审查与回档机制

生成后的代码不会立即执行，而是先进入 `Ace Editor` 编辑器。

用户可以：

- 阅读模型生成代码
- 手动修改代码
- 点击“重置”恢复为原始 LLM 版本
- 确认后再执行

这一阶段构成了系统的重要人工把关点，也是该平台区别于“直接自动执行”的关键设计。

## 14. 受控执行机制

### 14.1 执行环境

在 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R) 中，系统使用 `new.env(parent=baseenv())` 创建独立执行环境。

### 14.2 环境注入

执行前会将以下内容注入执行环境：

- 所有已加载 SDTM 域数据
- 常用 R 包函数
- 辅助函数，如 `strip_excel_apos()`、`dy_char()`

### 14.3 包调用控制

系统会：

- 屏蔽 `library()` 和 `require()`
- 屏蔽 `install.packages()`
- 扫描代码中的额外包依赖并尽可能提前注入

### 14.4 结果提取

执行成功后，系统会根据目标数据集名称，使用 `get()` 从执行环境中提取数据框对象，并写入 `rv$adam_datasets`。

这一机制可以被描述为“受控执行”，但不应表述为严格安全沙箱。

## 15. 输出数据集展示机制

系统最终输出由 `rv$adam_datasets` 承载，而不是仅依赖固定的 `adsl`、`adae` 两个对象。

在 [server.R](C:\Research\Projects\ADaM_Shiny-ADaM_Shiny_experimental\server.R) 中：

- `output$output_dataset_tabs` 动态创建结果页签
- 每个结果集动态注册：
  - 行数徽标
  - 表格输出
  - 下载按钮

这使系统具备支持多个目标 ADaM 数据集的能力。

## 16. 交互式可审计设计

系统的可审计性主要体现在三个环节：

1. Spec 解析可视化  
   列映射、置信度、补全建议、预览和行级风险均可见。

2. LLM 推断显式化  
   所有模型推断需要以 `risk_logs` 形式输出。

3. 执行前人工确认  
   生成代码必须先审查后执行。

因此，该系统本质上是“半自动化、可审阅、可追踪”的 AI 辅助工作流。

## 17. 当前限制与待确认项

基于静态代码分析，当前可确认的限制包括：

- 未见自动化测试目录或测试脚本
- 未见任务或审计结果持久化机制
- 动态代码执行仍存在安全边界问题
- LLM 生成质量高度依赖 Prompt 与输入数据质量
- 是否用于正式生产环境，当前材料不足以确认

以下判断应保留为待确认：

- 当前 Prompt 是否已覆盖复杂 ADaM 场景
- 实际业务中是否广泛使用非 `ADSL/ADAE` 数据集
- 当前代码执行策略是否满足更高等级安全要求
- 是否有外部数据库、身份认证或更大范围部署机制

## 18. 总结

ADaM Builder 构建了一个围绕临床数据编程任务的交互式 AI 工作流平台。其核心价值不在于“让模型生成一段代码”，而在于把以下多个环节整合进统一界面和统一流程中：

- 输入数据准备
- Spec 标准化解析
- 数据摘要生成
- LLM 结构化代码生成
- 风险提示
- 人工审查
- 结果执行与展示

从架构上看，该项目已经体现出较明显的工程化设计思路，包括配置驱动、统一协议、故障转移和动态结果承载等能力。后续若继续演进，可重点加强测试、审计、执行安全和规则模板化能力。
