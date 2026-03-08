# =============================================================================
# provider_registry.R
# ADaM 自动化生成平台 — LLM 提供商注册表
#
# 包含 7 个提供商的配置、模型前缀映射和辅助查询函数。
# 由 llm_api.R 和 server.R 通过 source() 引入。
# =============================================================================

# =============================================================================
# 提供商注册表
# 字段说明：
#   name            — 显示名称
#   base_url        — 固定 API 端点（云端提供商）
#   base_url_default— 本地推理默认地址（needs_url=TRUE 时使用）
#   auth_scheme     — 认证方式："bearer" / "x-api-key" / "none"
#   json_mode       — 是否追加 response_format: json_object
#   anthropic_style — TRUE → 顶层 system 字段 + /v1/messages 结构
#   needs_key       — 是否需要用户提供 API Key
#   needs_url       — TRUE 时 UI 渲染本地地址输入框
#   api_key_env     — 备用：从环境变量读取 Key 的变量名
# =============================================================================
PROVIDER_REGISTRY <- list(
  openai    = list(
    name            = "OpenAI",
    base_url        = "https://api.openai.com/v1/chat/completions",
    auth_scheme     = "bearer",
    json_mode       = TRUE,
    needs_key       = TRUE,
    needs_url       = FALSE,
    anthropic_style = FALSE,
    api_key_env     = "OPENAI_API_KEY"
  ),
  anthropic = list(
    name            = "Anthropic",
    base_url        = "https://api.anthropic.com/v1/messages",
    auth_scheme     = "x-api-key",
    json_mode       = FALSE,
    needs_key       = TRUE,
    needs_url       = FALSE,
    anthropic_style = TRUE,
    api_key_env     = "ANTHROPIC_API_KEY"
  ),
  deepseek  = list(
    name            = "DeepSeek",
    base_url        = "https://api.deepseek.com/v1/chat/completions",
    auth_scheme     = "bearer",
    json_mode       = TRUE,
    needs_key       = TRUE,
    needs_url       = FALSE,
    anthropic_style = FALSE,
    api_key_env     = "DEEPSEEK_API_KEY"
  ),
  kimi      = list(
    name            = "Kimi (Moonshot)",
    base_url        = "https://api.moonshot.cn/v1/chat/completions",
    auth_scheme     = "bearer",
    json_mode       = FALSE,
    needs_key       = TRUE,
    needs_url       = FALSE,
    anthropic_style = FALSE,
    api_key_env     = "MOONSHOT_API_KEY"
  ),
  qwen      = list(
    name            = "Qwen (Alibaba)",
    base_url        = "https://dashscope.aliyuncs.com/compatible-mode/v1/chat/completions",
    auth_scheme     = "bearer",
    json_mode       = TRUE,
    needs_key       = TRUE,
    needs_url       = FALSE,
    anthropic_style = FALSE,
    api_key_env     = "DASHSCOPE_API_KEY"
  ),
  ollama    = list(
    name            = "Ollama (本地)",
    base_url        = NULL,
    base_url_default= "http://localhost:11434/v1/chat/completions",
    auth_scheme     = "none",
    json_mode       = TRUE,
    needs_key       = FALSE,
    needs_url       = TRUE,
    anthropic_style = FALSE
  ),
  vllm      = list(
    name            = "vLLM (本地)",
    base_url        = NULL,
    base_url_default= "http://localhost:8000/v1/chat/completions",
    auth_scheme     = "none",
    json_mode       = TRUE,
    needs_key       = FALSE,
    needs_url       = TRUE,
    anthropic_style = FALSE
  )
)

# =============================================================================
# 模型名前缀 → 提供商 的映射表
# 按优先级排列（较长、更精确的前缀放前面）
# =============================================================================
MODEL_PREFIX_MAP <- list(
  "claude"   = "anthropic",
  "deepseek" = "deepseek",
  "moonshot" = "kimi",
  "kimi"     = "kimi",
  "qwen"     = "qwen",
  "ollama"   = "ollama",
  "vllm"     = "vllm"
)

# =============================================================================
# 辅助函数
# =============================================================================

# 从注册表获取提供商配置，未知提供商抛出错误
.get_provider_cfg <- function(provider) {
  cfg <- PROVIDER_REGISTRY[[provider]]
  if (is.null(cfg)) stop("未知提供商：", provider)
  cfg
}

# 各提供商的默认模型（用于故障转移时填充备用模型名）
.default_model <- function(provider) {
  switch(provider,
    openai    = "gpt-4o",
    anthropic = "claude-sonnet-4-5",
    deepseek  = "deepseek-chat",
    kimi      = "moonshot-v1-8k",
    qwen      = "qwen-max",
    ollama    = "llama3.2",
    vllm      = "local-model",
    "gpt-4o"
  )
}
