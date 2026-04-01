FROM rocker/shiny:4.3

# =============================================================================
# 系统依赖
# 涵盖 httr2、Rcpp、stringi、sass、textshaping 等 R 包的编译/运行时依赖
# =============================================================================
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libsodium-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    pkg-config \
    curl \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# 安装 renv
# =============================================================================
RUN R -q -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# =============================================================================
# 将 renv.lock 中所有包直接安装到 site-library（第一优先级）
#
# rocker/shiny:4.3 基础镜像在 site-library 预装了旧版包（如 rlang 1.1.3），
# 与 renv.lock 要求的版本冲突。
#
# 解决方案：直接 restore 到 /usr/local/lib/R/site-library，覆盖旧版本。
# site-library 本来就是 .libPaths()[1]，无需依赖 R_LIBS_USER 或 Rprofile.site。
# =============================================================================
WORKDIR /srv/shiny-server/adam

COPY renv.lock .

RUN R -q -e "renv::consent(provided = TRUE); \
      renv::restore( \
        prompt  = FALSE, \
        library = '/usr/local/lib/R/site-library', \
        repos   = c(CRAN = 'https://cloud.r-project.org') \
      )" \
 && R -q -e "stopifnot(as.character(packageVersion('rlang')) == '1.1.7')"

# =============================================================================
# 安装认证所需包（DBI / RSQLite / sodium）
# 这三个包不在 renv.lock 顶层，单独安装到 site-library
# sodium 依赖 libsodium 系统库（rocker/shiny 基础镜像已包含）
# =============================================================================
RUN R -q -e "install.packages( \
      c('DBI', 'RSQLite', 'sodium'), \
      repos   = 'https://cloud.r-project.org', \
      lib     = '/usr/local/lib/R/site-library', \
      quiet   = TRUE \
    )"

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

# 由 root 启动 shiny-server，再按 shiny-server.conf 中的 run_as shiny
# 派生应用进程。这样对挂载卷的权限兼容性更好，避免会话初始化时因
# auth/users.db 无法创建或写入而直接 disconnected。
CMD ["/usr/bin/shiny-server"]
