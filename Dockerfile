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
# 独立高优先级 R 库路径
#
# rocker/shiny:4.3 基础镜像预装了旧版包（如 rlang 1.1.3），与 renv.lock 要求
# 的版本（rlang 1.1.7）冲突，导致 readr 等包加载时报 namespace 版本错误。
#
# 解决方案：将 renv.lock 中的包安装到独立目录 /srv/R_libs，并通过
# R_LIBS_USER 环境变量让 R 在所有其他路径之前搜索该目录。这样 renv.lock
# 的精确版本始终优先于基础镜像预装包。
# =============================================================================
ENV R_LIBS_USER=/srv/R_libs

RUN mkdir -p /srv/R_libs \
    && R -e "install.packages('renv', repos = 'https://cloud.r-project.org', lib = '/srv/R_libs')" \
    && echo '.libPaths(c("/srv/R_libs", .libPaths()))' >> /etc/R/Rprofile.site

# =============================================================================
# 利用 Docker 层缓存：先只复制 lockfile，renv::restore() 后再复制其余代码
# =============================================================================
WORKDIR /srv/shiny-server/adam

COPY renv.lock .

RUN R -e "renv::restore(prompt = FALSE, \
      library = '/srv/R_libs', \
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
