# ADaM_Shiny — 临床数据自动化 ADaM 生成平台

> ⚗️ **当前分支：`ADaM_Shiny_experimental`（实验性开发分支）**
>
> 本分支用于验证新特性与前沿模型集成，代码处于持续迭代状态，可能存在不稳定因素。如需稳定版本，请切换至 `main` 分支。

> 基于 R Shiny + 大语言模型（LLM）的 CDISC ADaM 数据集自动生成工具

---

## 实验性分支说明

`ADaM_Shiny_experimental` 分支是在 `main` 稳定版基础上进行的实验性扩展开发分支，主要用于：

- 🧪 **前沿模型接入**：率先测试新发布的 LLM 模型（如 Kimi K2.5），在稳定验证后同步至主分支
- 🔬 **功能预研**：提前实验新的工作流或 UI 交互，收集反馈后再合入主分支
- 🛠️ **兼容性测试**：验证不同 LLM Provider 返回格式与代码质量的差异

> ⚠️ **注意**：本分支功能尚未经过全面回归测试，不建议在生产/验证环境中直接使用。

---

## 与稳定版（main）的主要差异

| 特性 | `main`（稳定版） | `ADaM_Shiny_experimental`（本分支） |
|------|-----------------|--------------------------------------|
| Kimi 默认模型 | `moonshot-v1-8k` | **`kimi-k2.5`**（新一代模型，更强推理能力） |
| UI 默认模型填充 | `moonshot-v1-8k` | **`kimi-k2.5`** |
| 分支状态 | 稳定，已验证 | 实验性，持续迭代 |

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

---

## 项目结构

```
ADaM_Shiny/
├── app.R           # 入口文件：安装依赖、加载模块、启动 Shiny 应用
├── ui.R            # UI 定义（bslib 页面布局，侧边栏 + 多 Tab 主面板）
├── server.R        # 服务器逻辑（响应式状态管理、LLM 调用流水线、代码执行）
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
3. **配置 AI 引擎**：在"AI 引擎配置"面板中选择 LLM 提供商并输入 API Key
4. **生成数据集**：点击"🚀 生成 ADaM 与代码"按钮，等待 LLM 生成并执行 R 代码
5. **预览与下载**：在主面板的各 Tab 中查看生成的代码、执行日志、数据集预览，并下载结果文件

> 💡 **提示**：`demo/` 目录中提供了完整的示例数据，可直接用于快速体验。

---

## 支持的 LLM 提供商

| 提供商 | 标识符 | 实验分支默认模型 | 稳定版默认模型 |
|--------|--------|-----------------|---------------|
| Kimi (Moonshot AI) | `kimi` | `kimi-k2.5` ⚗️ | `moonshot-v1-8k` |
| DeepSeek | `deepseek` | `deepseek-chat` | `deepseek-chat` |
| OpenAI | `openai` | `gpt-4` | `gpt-4` |
| 通义千问 (Qwen) | `qwen` | `qwen-plus` | `qwen-plus` |

> API Key 仅在当前会话中使用，不会被持久化存储。

---

## 实验性特性详细说明

### Kimi K2.5 模型接入

本分支将 Kimi 的默认模型从 `moonshot-v1-8k` 升级为 **`kimi-k2.5`**。

**背景**：Kimi K2.5 是 Moonshot AI 发布的新一代大语言模型，在代码生成、逻辑推理方面相比 `moonshot-v1-8k` 有显著提升，预期可以生成更高质量的 ADaM R 转化代码。

**启用方式**：在"AI 引擎配置"面板中选择 **Kimi (Moonshot AI)**，模型名称栏默认已填入 `kimi-k2.5`，直接输入 API Key 即可使用。

**如需切换回旧模型**：在模型名称输入框中手动将 `kimi-k2.5` 改为 `moonshot-v1-8k` 或其他 Kimi 支持的模型名称。

---

## 参与开发 / 贡献实验特性

如果你希望在本实验分支基础上贡献新功能或模型适配，请：

1. 基于 `ADaM_Shiny_experimental` 分支创建你的功能分支
2. 在 `llm_api.R` 的 `LLM_PROVIDERS` 列表中添加新 Provider 配置（需兼容 OpenAI Chat Completion API 格式）
3. 在 `server.R` 的 `defaults` 列表中为新 Provider 设置默认模型名称
4. 在 `ui.R` 的 `selectInput` 中添加新 Provider 的显示名称与标识符
5. 通过 Pull Request 提交，并附上实验结果与测试截图

---

## 技术栈

- **前端框架**：R Shiny + bslib (Bootstrap 5)
- **HTTP 客户端**：httr2
- **数据处理**：dplyr, readr, lubridate, stringr, tidyr
- **LLM 接口**：OpenAI 兼容 Chat Completion API

---

## 许可证

本项目仅供学习和研究使用。
