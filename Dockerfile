FROM rocker/shiny:4.3

# =============================================================================
# 系统依赖
# 涵盖 httr2、Rcpp、stringi、sass、textshaping 等 R 包的编译/运行时依赖
# =============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# 安装 renv，用于恢复精确的 R 包版本
# =============================================================================
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# =============================================================================
# 利用 Docker 层缓存：先只复制 lockfile，renv::restore() 后再复制其余代码
# 这样只有在 renv.lock 变化时才重新安装包（避免每次代码改动都重装）
# =============================================================================
WORKDIR /srv/shiny-server/adam

COPY renv.lock .

# 将所有包安装到系统 R library（/usr/local/lib/R/library）
# 避免 renv 项目级 library 在 Shiny Server 启动时未激活导致包找不到
RUN R -e "renv::restore(prompt = FALSE, \
      library = '/usr/local/lib/R/library', \
      repos   = c(CRAN = 'https://cloud.r-project.org'))"

# =============================================================================
# 复制应用代码（在包安装完成后，最大化缓存命中）
# =============================================================================
COPY . .

# =============================================================================
# 权限设置与 Shiny Server 配置
# =============================================================================
RUN chown -R shiny:shiny /srv/shiny-server/adam \
    && mkdir -p /var/log/shiny-server \
    && chown -R shiny:shiny /var/log/shiny-server

# 覆盖默认 Shiny Server 配置，适配单应用 + 长超时场景
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838

# 以非 root 用户运行（安全实践）
USER shiny

CMD ["/usr/bin/shiny-server"]
