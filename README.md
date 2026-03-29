# ADaM_Shiny — 临床数据自动化 ADaM 生成平台

> ⚗️ **当前分支：`experimental-v3`（第三代实验性开发分支）**
>
> 本分支在第二代实验分支（`experimental-v2`）基础上继续迭代，重点增强了端到端可验证性（`tests/`）、校验后自动修复链路，以及更完整的 Derivation Plan / Spec 一致性处理。代码仍处于实验阶段，如需稳定版本请切换至 `main` 分支。

> 基于 R Shiny + 大语言模型（LLM）的 CDISC ADaM 数据集自动生成工具

---

## 实验性分支说明

`experimental-v3` 是继 `experimental-v2` 之后的第三代实验性扩展分支，主要用于：

- ✅ **端到端回归能力补齐**：新增 `tests/test_e2e_pipeline.R` 与 `tests/test_static_checks.R`，可独立跑通“读入→生成→执行→校验”
- ♻️ **校验后自动修复增强**：在 `server.R` 中增加基于校验结果提取缺失变量并回灌 LLM 的修复流程（带轮次/收敛终止条件，而非无限循环）
- 🧩 **Plan/Spec 对齐更严格**：`validation_utils.R` 与 `derivation_plan_utils.R` 增强了 plan 覆盖、角色推断与缺失场景兜底
- 🗂️ **样例数据结构标准化**：新增 `demo-data/` 目录，便于测试脚本和应用共用同一组基准数据

> ⚠️ **注意**：本分支功能尚未经过全面回归测试，不建议在生产/验证环境中直接使用。

---

## 分支演进关系

```
main（稳定版，长期维护）
  └── ADaM_Shiny_experimental（v1：首次实验化重构）
        └── experimental-v2（v2：引入验证层与静态检查）
              └── experimental-v3（本分支：测试与自动修复增强）
                    └── → main（验证稳定后合入主分支）
```

实验分支的定位是"功能孵化器"——在此验证架构变更和新模型后，经过充分回归测试再合并回 `main`。

---

## 与各分支的主要差异

| 维度 | `main`（稳定版） | `ADaM_Shiny_experimental`（v1 实验） | `experimental-v2`（v2 实验） | `experimental-v3`（本分支） |
|------|-----------------|--------------------------------------|-------------------------------|------------------------------|
| Spec 解析流程 | 直接读入后进入生成 | 新增启发式列名识别 + 解析确认 Modal | 完整多文件解析状态机（`step_parse` / `spec_confirmed`） | 延续 v2，并增加会话级 LLM 辅助字段映射 |
| 生成前质量闸门 | 基本输入检查 | 基础参数与输入完整性检查 | 新增 `run_code_static_checks()` + Derivation Plan 对齐检查 | 在 v2 基础上继续强化，校验结果可触发自动修复循环 |
| 执行与隔离 | 共享执行环境，按 ADSL→ADAE 顺序 | 改为更严格隔离环境（`baseenv()` + 预注入函数） | 延续隔离执行，并引入阶段化流水线状态追踪 | 延续并增强“生成→校验→修复→再校验”闭环 |
| 结果校验能力 | 以预览为主 | 初步增强 | 引入 `validation_utils.R`（结构、主键、日期、Plan 覆盖） | 校验逻辑进一步细化（Plan 映射收集、更明确的缺失/额外变量语义） |
| 自动化测试 | 无 | 无 | 无（以手工验证为主） | **新增 `tests/`：E2E 管线测试 + 静态检查测试** |
| 示例数据组织 | `demo/` | 部分分支未保留完整 demo | 主要依赖交互上传 | **新增 `demo-data/` 用于脚本化回归与可复现实验** |
| 分支状态 | 稳定，已验证 | 实验性，已归档 | 实验性，持续迭代 | 实验性，面向合并前验证 |

### 核心架构变化说明

分支功能演进的关键在于“**生成前后控制链路**”不断补齐，而不只是 Provider 调用方式变化：

1. **main**：上传 → 生成 → 执行 → 预览（流程最短，闸门最少）
2. **v1 (`ADaM_Shiny_experimental`)**：加入 Spec 解析识别与确认环节，避免“列名猜错直接入模”
3. **v2 (`experimental-v2`)**：加入静态检查、Derivation Plan 规范化、输出校验，形成“可解释可追踪”的中间层
4. **v3 (`experimental-v3`)**：在 v2 基础上补齐自动化测试和“校验失败→自动修复→再校验”闭环，提高回归稳定性

```r
# experimental-v3（server.R）伪代码示意：从校验结果中抽取可修复问题并驱动修复轮次
repair <- .collect_repair_candidates(validation_res, repairable_checks, specs)
if (isTRUE(repair$triggered)) {
  # 构造修复提示并回灌 LLM，随后重新执行与校验（达到最大修复轮次或问题签名不再变化时终止）
}
```

> 上述差异均来自跨分支代码行为对比（`server.R` / `validation_utils.R` / `derivation_plan_utils.R` / `code_static_checks.R` / `tests/`），而非仅模型路由配置。

---

## 项目简介

**ADaM_Shiny** 是一款面向临床数据编程人员的 Web 应用，旨在将 SDTM（研究数据制表模型）源数据自动转化为符合 CDISC ADaM 标准的分析数据集（如 ADSL、ADAE）。

用户只需上传 SDTM CSV 文件和 ADaM Specification CSV 文件，应用即可调用大语言模型（支持 Kimi、DeepSeek、OpenAI GPT-4、通义千问）自动生成 R 转化代码，并在应用内直接执行、预览结果，最终支持一键下载生成的数据集与 R 脚本。

---

## 主要功能

- 📂 **多文件上传**：同时上传多个 SDTM 域 CSV 和多个 ADaM Spec CSV
- 🤖 **多模型支持**：支持四大 LLM 提供商（Kimi / DeepSeek / OpenAI / 通义千问），用户只需输入 API Key 即可切换
- ⚙️ **自动代码生成**：LLM 根据 SDTM 列名与 Spec 元数据生成完整、可运行的 R 转化代码
- 🔗 **ADSL → ADAE 依赖处理**：自动保证先生成 ADSL，再生成依赖 ADSL 的 ADAE
- 📊 **在线预览**：生成后可直接在浏览器中预览 ADSL / ADAE 数据集（前 200 行）
- ⬇️ **一键下载**：支持下载 ADSL CSV、ADAE CSV 及完整 R 脚本
- 🗑️ **全局清空**：一键重置所有上传文件与生成结果，方便重新开始

---

## 项目结构

```
ADaM_Shiny/
├── app.R                   # 入口文件：安装依赖、加载模块、启动 Shiny 应用
├── ui.R                    # UI 定义（解析状态、流水线状态、结果展示）
├── server.R                # 服务端主流程（解析→生成→静态检查→执行→校验→修复）
├── llm_api.R               # LLM 路由与故障转移链
├── provider_registry.R     # Provider 配置注册表（含本地/云端提供商）
├── data_utils.R            # 数据工具函数（SDTM 读取、域推断、摘要）
├── code_static_checks.R    # 代码静态检查
├── validation_utils.R      # 输出数据集质量校验
├── derivation_plan_utils.R # Derivation Plan 标准化与一致性检查
├── domain_registry.R       # SDTM 域注册及分组
├── demo-data/              # v3 示例数据（供测试脚本/回归复现）
│   ├── dm.csv              # SDTM DM 域示例
│   ├── ex.csv              # SDTM EX 域示例
│   ├── ae.csv              # SDTM AE 域示例
│   ├── ads_adsl_full.csv   # ADSL Specification 示例
│   └── ads_adae_full.csv   # ADAE Specification 示例
└── tests/                  # v3 新增测试脚本（E2E + 静态检查）
```

---

## 快速开始

### 环境要求

- R ≥ 4.1.0
- 互联网连接（首次运行自动安装依赖包；调用 LLM API 时需要）

### 运行方式

```r
# 在 R Console 中运行
shiny::runApp(".")
```

```bash
# 或在终端中运行
Rscript app.R
```

首次运行时，`app.R` 会自动检测并安装以下依赖包（无需手动安装）：

`shiny`, `bslib`, `bsicons`, `DT`, `shinyAce`, `dplyr`, `readr`, `lubridate`, `stringr`, `tidyr`, `jsonlite`, `httr2`, `shinyjs`

### 使用步骤

1. **上传 SDTM 数据**：在侧边栏"数据准备"面板中上传 SDTM CSV 文件（可多选，如 `dm.csv`、`ex.csv`、`ae.csv`）
2. **上传 Specification**：上传 ADaM Spec CSV 文件（可多选，如 `ads_adsl_full.csv`、`ads_adae_full.csv`）
3. **配置 AI 引擎**：在"AI 引擎配置"面板中选择 LLM 提供商并输入 API Key（模型名称栏会自动填入推荐值，可手动修改）
4. **生成数据集**：点击"🚀 生成 ADaM 与代码"按钮，等待 LLM 生成并执行 R 代码
5. **预览与下载**：在主面板的各 Tab 中查看生成的代码、执行日志、数据集预览，并下载结果文件

> 💡 **提示**：`experimental-v3` 使用 `demo-data/` 目录作为示例数据；`main` 分支仍使用 `demo/`。切换分支时请同步调整示例数据路径。

---

## 支持的 LLM 提供商

| 提供商 | 标识符 | experimental-v3 UI 默认模型 | Provider 配置回退模型 |
|--------|--------|----------------------------|----------------------|
| Kimi (Moonshot AI) | `kimi` | `kimi-k2.5` ⚗️ | `moonshot-v1-8k` |
| DeepSeek | `deepseek` | `deepseek-chat` | `deepseek-chat` |
| OpenAI | `openai` | `gpt-4` | `gpt-4` |
| 通义千问 (Qwen) | `qwen` | `qwen-plus` | `qwen-plus` |

> - **UI 默认模型**：切换 Provider 时自动填入模型输入框的建议值（可手动修改）
> - **Provider 配置回退模型**：若用户清空模型输入框，实际请求时使用的回退值
> - API Key 仅在当前会话中使用，不会被持久化存储

---

## 实验性特性详细说明

### 1. 分层模型默认值管理

`experimental-v3` 继承了 v2 的“分层默认值”设计，同时将重点放在“可验证、可回归、可修复”的工程链路：

| 层级 | 文件 | 职责 | 修改场景 |
|------|------|------|---------|
| **UI 展示层** | `server.R` | 控制 Provider 切换时界面显示的推荐模型名 | 更换推荐模型、A/B 测试不同模型 |
| **Provider 配置层** | `llm_api.R` | 作为 API 请求的回退默认值，保持稳定 | 新增/删除 Provider |
| **质量控制层** | `code_static_checks.R` + `validation_utils.R` | 生成前后风险识别与质量判定 | 增加规则、降低执行风险 |
| **回归保障层** | `tests/` | E2E + 静态检查脚本化验证 | 分支合并前回归 |

这种解耦使得：
- 可以在不修改 `llm_api.R` 的情况下，随时调整 UI 推荐的模型名称
- Provider 的 API 端点配置保持稳定，降低引入 API 兼容性问题的风险

### 2. 自动修复与回归测试增强

除模型推荐值策略外，v3 更关键的是把“失败后怎么办”系统化：

- 当校验发现缺失变量/Plan 覆盖问题时，可提取问题签名并驱动下一轮修复生成
- 新增 `tests/test_e2e_pipeline.R`，可在 Mock 或真实 API 模式下复现实验流程
- 新增 `tests/test_static_checks.R`，用于验证静态检查器对高风险模式的识别

这部分能力直接影响“能否稳定回归”，也是 v3 与前序分支最核心的差异。

---

## 参与开发 / 贡献实验特性

如果你希望在本实验分支基础上贡献新功能或模型适配，请：

1. 基于 `experimental-v3` 分支创建你的功能分支
2. **新增 Provider**：在 `provider_registry.R` 注册 Provider 配置，并在 `llm_api.R` 路由逻辑中确认兼容（如 OpenAI-compatible 或 Anthropic 格式）；同时在 `ui.R` 的模型选项与 `server.R` 的默认映射中补齐对应项
3. **调整模型推荐值**：直接修改 `server.R` 中 `defaults` 列表对应的模型名称，无需改动 `llm_api.R`
4. 通过 Pull Request 提交，并附上实验结果与测试截图

---

## 技术栈

- **前端框架**：R Shiny + bslib (Bootstrap 5)
- **HTTP 客户端**：httr2
- **数据处理**：dplyr, readr, lubridate, stringr, tidyr
- **LLM 接口**：OpenAI 兼容 Chat Completion API

---

## 许可证

本项目仅供学习和研究使用。
