# =============================================================================
# domain_registry.R
# ADaM 自动化生成平台 — SDTM 域注册表（配置驱动架构）
#
# 职责：
#   1. 定义所有支持的 SDTM 域元数据（id / label / placeholder / required / group）
#   2. 提供域分组标签映射
#   3. 提供注册表查询辅助函数
#
# 使用方式：
#   由 app.R（全局环境）和 server.R（local 环境）各自 source() 一次
# =============================================================================

SDTM_DOMAIN_REGISTRY <- list(
  dm = list(id="dm", label="DM — 人口统计", placeholder="DM — 人口统计 (.csv)", required=TRUE,  group="core"),
  ex = list(id="ex", label="EX — 用药暴露", placeholder="EX — 用药暴露 (.csv)", required=TRUE,  group="core"),
  ae = list(id="ae", label="AE — 不良事件", placeholder="AE — 不良事件 (.csv)", required=FALSE, group="core"),
  vs = list(id="vs", label="VS — 生命体征", placeholder="VS — 生命体征 (.csv)", required=FALSE, group="basic"),
  lb = list(id="lb", label="LB — 实验室",   placeholder="LB — 实验室检查 (.csv)", required=FALSE, group="basic"),
  cm = list(id="cm", label="CM — 伴随用药", placeholder="CM — 伴随用药 (.csv)", required=FALSE, group="basic"),
  mh = list(id="mh", label="MH — 既往病史", placeholder="MH — 既往病史 (.csv)", required=FALSE, group="basic"),
  sv = list(id="sv", label="SV — 访视",     placeholder="SV — 受试者访视 (.csv)", required=FALSE, group="basic"),
  eg = list(id="eg", label="EG — 心电图",   placeholder="EG — 心电图 (.csv)",     required=FALSE, group="extended"),
  pe = list(id="pe", label="PE — 体格检查", placeholder="PE — 体格检查 (.csv)",   required=FALSE, group="extended"),
  tu = list(id="tu", label="TU — 肿瘤",     placeholder="TU — 肿瘤评估 (.csv)",   required=FALSE, group="extended"),
  rs = list(id="rs", label="RS — 反应",     placeholder="RS — 疗效反应 (.csv)",   required=FALSE, group="extended"),
  mb = list(id="mb", label="MB — 微生物",   placeholder="MB — 微生物学 (.csv)",   required=FALSE, group="extended")
)

DOMAIN_GROUP_LABELS <- list(
  core     = "核心域 (Core)",
  basic    = "基础域 (Basic)",
  extended = "扩展域 (Extended)"
)

# 返回 required=TRUE 的域 ID 向量
.get_required_domain_ids <- function() {
  names(Filter(function(d) isTRUE(d$required), SDTM_DOMAIN_REGISTRY))
}

# 返回所有已注册域的 ID 向量
.get_active_domain_ids <- function() {
  names(SDTM_DOMAIN_REGISTRY)
}
