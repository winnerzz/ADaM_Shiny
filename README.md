# ADaM_Shiny — 临床数据自动化 ADaM 生成平台（实验版本）

> ⚠️ **本分支为实验版本（`ADaM_Shiny_experimental`），与主分支（`main`）并行开发。**
> 实验版本包含尚在探索中的新功能与改进，可能存在不稳定因素。如需使用稳定版本，请切换至 [main 分支](../../tree/main)。

---

> 基于 R Shiny + 大语言模型（LLM）的 CDISC ADaM 数据集自动生成工具

---

## 项目简介

**ADaM_Shiny** 是一款面向临床数据编程人员的 Web 应用，旨在将 SDTM（研究数据制表模型）源数据自动转化为符合 CDISC ADaM 标准的分析数据集（如 ADSL、ADAE）。

用户只需上传 SDTM CSV 文件和 ADaM Specification CSV 文件，应用即可调用大语言模型（支持 Kimi、DeepSeek、OpenAI GPT-4、通义千问）自动生成 R 转化代码，并在应用内直接执行、预览结果，最终支持一键下载生成的数据集与 R 脚本。

## 实验版本说明

本分支（`ADaM_Shiny_experimental`）是 ADaM_Shiny 项目的**实验开发分支**，与稳定的 `main` 主分支并行维护。

| 分支 | 说明 | 状态 |
|------|------|------|
| `main` | 稳定版，面向生产使用 | ✅ 稳定 |
| `ADaM_Shiny_experimental` | 实验版，探索新功能与改进 | 🧪 实验中 |

### 实验版本与主版本的主要区别

- 扩展的 LLM Provider 支持与动态注册机制（`provider_registry.R`）
- 增强的 UI 布局与交互体验
- 更丰富的 SDTM → ADaM 转化规则支持
- 其他正在探索中的功能改进

> 实验版本中的功能在验证稳定后会合并到 `main` 分支。

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
├── app.R                # 入口文件：安装依赖、加载模块、启动 Shiny 应用
├── ui.R                 # UI 定义（bslib 页面布局，侧边栏 + 多 Tab 主面板）
├── server.R             # 服务器逻辑（响应式状态管理、LLM 调用流水线、代码执行）
├── llm_api.R            # LLM API 路由模块（多 Provider 支持，OpenAI 兼容格式）
├── provider_registry.R  # [实验] 动态 Provider 注册与管理模块
├── data_utils.R         # 数据工具函数（SDTM 加载、Spec 解析、Study Day 计算等）
└── demo/                # 示例数据（如在实验版中可用）
```

---

## 快速开始

### 环境要求

- R ≥ 4.1.0
- 互联网连接（首次运行自动安装依赖包；调用 LLM API 时需要）

### 运行方式

```r
# 方式一：在 R Console 中运行
shiny::runApp(".")

# 方式二：在终端中运行
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

| 提供商 | 标识符 | 默认模型 |
|--------|--------|----------|
| Kimi (Moonshot AI) | `kimi` | `moonshot-v1-8k` |
| DeepSeek | `deepseek` | `deepseek-chat` |
| OpenAI | `openai` | `gpt-4` |
| 通义千问 (Qwen) | `qwen` | `qwen-plus` |

> API Key 仅在当前会话中使用，不会被持久化存储。

---

## 技术栈

- **前端框架**：R Shiny + bslib (Bootstrap 5)
- **HTTP 客户端**：httr2
- **数据处理**：dplyr, readr, lubridate, stringr, tidyr
- **LLM 接口**：OpenAI 兼容 Chat Completion API

---

## 许可证

本项目仅供学习和研究使用。
