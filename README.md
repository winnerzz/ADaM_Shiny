# ADaM_Shiny — 临床数据自动化 ADaM 生成平台

> ⚗️ **当前分支：`experimental-v2`（第二代实验性开发分支）**
>
> 本分支在第一代实验分支（`ADaM_Shiny_experimental`）基础上进一步迭代，引入了更灵活的模型配置机制与增强的 UI 默认值管理。代码处于持续迭代状态，可能存在不稳定因素。如需稳定版本，请切换至 `main` 分支。

> 基于 R Shiny + 大语言模型（LLM）的 CDISC ADaM 数据集自动生成工具

---

## 实验性分支说明

`experimental-v2` 是继 `ADaM_Shiny_experimental` 之后的第二代实验性扩展分支，主要用于：

- 🧪 **前沿模型接入**：持续跟进最新 LLM 模型（如 Kimi K2.5），在稳定验证后同步至主分支
- 🔬 **UI 默认值优化**：将模型默认值的管理从 `llm_api.R` Provider 配置层迁移至 `server.R` 动态响应层，使 UI 模型建议与 Provider 底层配置解耦
- 🛠️ **兼容性增强**：允许用户在不修改 Provider 配置的情况下，通过 UI 输入框覆盖模型名称
- 📐 **架构预研**：验证"Provider 配置稳定 + UI 默认值灵活"的分层配置模式，收集反馈后再合入主分支

> ⚠️ **注意**：本分支功能尚未经过全面回归测试，不建议在生产/验证环境中直接使用。

---

## 分支演进关系

```
main（稳定版，长期维护）
  └── ADaM_Shiny_experimental（第一代实验分支，已归档）
        └── experimental-v2（本分支，第二代实验分支）
              └── → main（验证稳定后合入主分支）
```

实验分支的定位是"功能孵化器"——在此验证架构变更和新模型后，经过充分回归测试再合并回 `main`。

---

## 与各分支的主要差异

| 特性 | `main`（稳定版） | `ADaM_Shiny_experimental`（v1 实验） | `experimental-v2`（本分支） |
|------|-----------------|--------------------------------------|------------------------------|
| Kimi LLM_PROVIDERS 模型 | `moonshot-v1-8k` | `kimi-k2.5` | `moonshot-v1-8k`（Provider 配置保持稳定） |
| UI 切换 Provider 时默认填充模型 | `moonshot-v1-8k` | `kimi-k2.5` | **`kimi-k2.5`**（由 `server.R` 动态注入） |
| 模型默认值管理层 | `llm_api.R` | `llm_api.R` | **`server.R`**（解耦 UI 默认与 Provider 配置） |
| 分支状态 | 稳定，已验证 | 实验性，已归档 | 实验性，持续迭代 |

### 核心架构变化说明

**v1 实验分支**的做法是直接修改 `llm_api.R` 中 `LLM_PROVIDERS` 的 `model` 字段，但这会使 Provider 底层配置与 UI 展示逻辑耦合，不利于后续维护。

**experimental-v2** 将 UI 模型默认值移至 `server.R` 的 `observeEvent(input$llm_provider)` 回调中集中管理：

```r
# server.R — 切换 Provider 时自动更新 UI 模型输入框（实际代码）
observeEvent(input$llm_provider, {
  defaults <- list(
    kimi     = "kimi-k2.5",    # UI 建议值，可被用户覆盖
    deepseek = "deepseek-chat",
    openai   = "gpt-4",
    qwen     = "qwen-plus"
  )
  val <- defaults[[input$llm_provider]]
  updateTextInput(session, "model_name",
                  value = if (is.null(val)) "" else val)
})
```

> 注：以上为 `server.R` 的实际代码。`defaults` 列表仅在用户切换 Provider 时才会执行，开销可忽略不计。

这样 `llm_api.R` 中的 `LLM_PROVIDERS$model` 作为"回退默认值"保持稳定，而 UI 展示的默认模型名称则由 `server.R` 灵活控制，两者职责分离。

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
├── app.R           # 入口文件：安装依赖、加载模块、启动 Shiny 应用
├── ui.R            # UI 定义（bslib 页面布局，侧边栏 + 多 Tab 主面板）
├── server.R        # 服务器逻辑（响应式状态管理、LLM 调用流水线、代码执行）
│                   #   ↑ experimental-v2 新增：在此集中管理 UI 模型默认值
├── llm_api.R       # LLM API 路由模块（多 Provider 支持，OpenAI 兼容格式）
├── data_utils.R    # 数据工具函数（SDTM 加载、Spec 解析、Study Day 计算等）
└── demo/           # 示例数据
    ├── dm.csv              # SDTM DM 域示例
    ├── ex.csv              # SDTM EX 域示例
    ├── ae.csv              # SDTM AE 域示例
    ├── ads_adsl_full.csv   # ADSL Specification 示例
    └── ads_adae_full.csv   # ADAE Specification 示例
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

> 💡 **提示**：`demo/` 目录中提供了完整的示例数据，可直接用于快速体验。

---

## 支持的 LLM 提供商

| 提供商 | 标识符 | experimental-v2 UI 默认模型 | Provider 配置回退模型 |
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

`experimental-v2` 的核心架构改进在于将模型默认值管理分为两层：

| 层级 | 文件 | 职责 | 修改场景 |
|------|------|------|---------|
| **UI 展示层** | `server.R` | 控制 Provider 切换时界面显示的推荐模型名 | 更换推荐模型、A/B 测试不同模型 |
| **Provider 配置层** | `llm_api.R` | 作为 API 请求的回退默认值，保持稳定 | 新增/删除 Provider |

这种解耦使得：
- 可以在不修改 `llm_api.R` 的情况下，随时调整 UI 推荐的模型名称
- Provider 的 API 端点配置保持稳定，降低引入 API 兼容性问题的风险

### 2. Kimi K2.5 推荐接入

本分支将 Kimi 的 UI 推荐模型从 `moonshot-v1-8k` 更新为 **`kimi-k2.5`**。

**背景**：Kimi K2.5 是 Moonshot AI 发布的新一代大语言模型，在代码生成、逻辑推理方面相比 `moonshot-v1-8k` 有显著提升，预期可以生成更高质量的 ADaM R 转化代码。

**启用方式**：在"AI 引擎配置"面板中选择 **Kimi (Moonshot AI)**，模型名称栏默认已填入 `kimi-k2.5`，直接输入 API Key 即可使用。

**如需切换回旧模型**：在模型名称输入框中手动将 `kimi-k2.5` 改为 `moonshot-v1-8k` 或其他 Kimi 支持的模型名称。

---

## 参与开发 / 贡献实验特性

如果你希望在本实验分支基础上贡献新功能或模型适配，请：

1. 基于 `experimental-v2` 分支创建你的功能分支
2. **新增 Provider**：在 `llm_api.R` 的 `LLM_PROVIDERS` 列表中添加新条目（需兼容 OpenAI Chat Completion API 格式），并在 `server.R` 的 `defaults` 列表中添加对应 UI 默认模型名称，以及在 `ui.R` 的 `selectInput` 中添加显示名称与标识符
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
