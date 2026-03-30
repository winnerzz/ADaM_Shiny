# ADaM Builder — 完整部署参考手册

> 分支：`deploy/vps-docker`
> 最后更新：2026-03-30

---

## 核心决策速查

| 决策项 | 选择 | 原因 |
|--------|------|------|
| 服务器地域 | **香港 VPS** | 无需 ICP 备案；国内外均可访问；OpenAI/DeepSeek 均可直连 |
| 架构 | Docker + Shiny Server + Nginx | 可复现、可迁移、易维护 |
| SSL | Let's Encrypt (certbot) | 免费、自动续期 |
| 默认 LLM | **DeepSeek / Qwen** | 国内可直连，香港节点延迟低；OpenAI/Anthropic 作备选 |
| 字体 | 系统字体（已修改） | Google Fonts CDN 在国内不可达 |
| CDN | 暂不使用 | Shiny 依赖 WebSocket 长连接，CDN 收益有限且增加排障复杂度 |
| 并发上限 | < 10 人团队 | 单机 Docker 足够，后续可升配 |

---

## 一、架构全貌

```
用户（国内 / 海外）
        │ HTTPS 443
        ▼
  ┌─────────────┐
  │    Nginx    │  ← SSL 终止 / WebSocket 升级 / 300s 超时
  └──────┬──────┘
         │ HTTP 3838（仅本机）
         ▼
  ┌──────────────────┐
  │  Docker 容器     │  rocker/shiny:4.3
  │  Shiny Server    │  ← app_dir 单应用
  │  R 进程 (app.R)  │
  └──────┬───────────┘
         │
         ├─ DeepSeek / Qwen API  （默认，国内可直连）
         └─ OpenAI / Anthropic    （备选，香港可直连）
```

---

## 二、国内外访问 — 关键注意事项

### 为什么选香港

| 对比项 | 国内服务器 | 香港服务器 | 美国服务器 |
|--------|-----------|-----------|-----------|
| ICP 备案 | 必须 | 不需要 | 不需要 |
| 国内访问速度 | 最快 | 较快 | 较慢 |
| OpenAI/Anthropic 直连 | 不支持 | 可以 | 可以 |
| 价格 | 低 | 中 | 低 |
| 推荐程度 | ⚠️ | ✅ 推荐 | ❌ |

### LLM API 地区限制

- **OpenAI / Anthropic**：官方不支持中国大陆。从大陆节点调用可能导致账号被封。
- **DeepSeek / Qwen**：国内服务，无地区限制，香港节点延迟低。
- **本项目策略**：默认用 DeepSeek/Qwen；OpenAI/Anthropic 在 UI 中保留为可选项。

### Google Fonts（已解决）

`ui.R` 已将 `font_google()` 改为系统字体栈，`deploy/vps-docker` 分支已包含此修改，无需额外操作。

---

## 三、第一次部署：完整步骤

> 前提：已购买香港 VPS（Ubuntu 22.04）+ 已有域名 + 代码已推送到 `deploy/vps-docker` 分支

### Step 1：VPS 初始化

```bash
# 本地 SSH 连入（Windows 用系统自带 OpenSSH 或 PuTTY）
ssh root@你的VPS公网IP

# 更新系统
apt update && apt upgrade -y
apt install -y curl git ufw ca-certificates gnupg lsb-release

# 创建普通用户（替换 deploy 为你的名字）
adduser deploy
usermod -aG sudo deploy

# 开放必要端口（顺序不能错：先放行 SSH 再 enable）
ufw allow OpenSSH
ufw allow 80/tcp
ufw allow 443/tcp
ufw enable
ufw status
```

> **注意**：云厂商控制台（阿里云/腾讯云）的"安全组"也要同步放行 22/80/443。

---

### Step 2：安装 Docker

```bash
# 清理旧版本
apt remove -y docker.io docker-compose containerd runc

# 添加官方仓库
install -m 0755 -d /etc/apt/keyrings
curl -fsSL https://download.docker.com/linux/ubuntu/gpg -o /etc/apt/keyrings/docker.asc
chmod a+r /etc/apt/keyrings/docker.asc
echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.asc] \
  https://download.docker.com/linux/ubuntu $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
  | tee /etc/apt/sources.list.d/docker.list > /dev/null

# 安装
apt update
apt install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin

# 验证
docker run hello-world
docker compose version

# 免 sudo 使用 docker
usermod -aG docker $USER && newgrp docker
```

**常见错误**：
- `permission denied while trying to connect to the Docker daemon` → 重新登录或等 `newgrp docker` 生效
- `docker compose: command not found` → compose 插件未装成功，重新执行 apt install

---

### Step 3：安装 Nginx

```bash
apt install -y nginx
systemctl enable nginx && systemctl start nginx
```

浏览器访问 `http://你的VPS公网IP`，看到 Nginx 欢迎页 = 正常。

---

### Step 4：配置域名 DNS

在域名服务商控制台添加 A 记录：

| 主机记录 | 类型 | 值 |
|---------|------|----|
| `@` 或子域名（如 `adam`） | A | VPS 公网 IP |

```bash
# 验证 DNS 生效（可能需要几分钟到几小时）
nslookup 你的域名
```

---

### Step 5：部署代码

```bash
su - deploy
mkdir -p ~/apps && cd ~/apps
git clone https://github.com/winnerzz/ADaM_Shiny.git adam-builder
cd adam-builder
git checkout deploy/vps-docker
```

> 私有仓库需先配置 deploy key 或 GitHub token。

---

### Step 6：构建并启动容器

```bash
docker compose build        # 首次约 5~15 分钟（renv::restore 安装 R 包）
docker compose up -d        # 后台启动
docker compose ps           # 确认状态为 running / healthy
docker compose logs -f      # 查看实时日志
```

**常见错误**：
- `renv::restore()` 失败 → 查看构建日志末尾，通常是网络问题或系统依赖缺失
- 容器一直 `restarting` → `docker compose logs` 查具体报错

---

### Step 7：配置 Nginx 反代

```bash
# 复制配置
sudo cp nginx/adam.conf /etc/nginx/sites-available/adam.conf

# 修改域名（将 your-domain.com 全部替换为实际域名）
sudo nano /etc/nginx/sites-available/adam.conf

# 启用并测试
sudo ln -s /etc/nginx/sites-available/adam.conf /etc/nginx/sites-enabled/
sudo rm -f /etc/nginx/sites-enabled/default
sudo nginx -t
sudo systemctl reload nginx
```

**常见错误**：
- `nginx: configuration file test failed` → 缺分号或域名写错
- 返回 `502 Bad Gateway` → 容器没起来：`docker compose ps && curl http://127.0.0.1:3838`

---

### Step 8：申请 SSL 证书

```bash
apt install -y snapd
snap install core && snap refresh core
snap install --classic certbot
ln -sf /snap/bin/certbot /usr/local/bin/certbot

# 自动申请证书并配置 Nginx（按提示输入邮箱、同意协议）
certbot --nginx -d 你的域名

# 测试自动续期（证书 90 天有效，certbot 自动续期）
certbot renew --dry-run
```

**常见错误**：
- `Timeout during connect` → 80/443 没通，检查 UFW 和云安全组
- `NXDOMAIN` → DNS 还未生效，等待后重试

---

### Step 9：验证上线

```bash
curl -I http://你的域名     # 应返回 301 → HTTPS
curl -I https://你的域名    # 应返回 200
docker compose ps           # 容器状态 healthy
```

浏览器打开 `https://你的域名`，上传 `demo-data/` 中的 CSV，走完整生成流程，确认下载正常。

---

## 四、后续迭代工作流

### 日常代码更新

```bash
# 本地：修改 → 提交 → 推送
git add .
git commit -m "your change"
git push origin deploy/vps-docker

# 服务器：拉取 → 重建 → 重启
cd ~/apps/adam-builder
git pull origin deploy/vps-docker
docker compose up -d --build
docker compose logs -f
```

> 这会有数秒短暂重启，内部团队完全可接受。

---

### R 包升级（renv.lock 变化）

```bash
# 本地：更新锁文件并提交
renv::snapshot()
git add renv.lock && git commit -m "update R packages" && git push

# 服务器：强制重建（--no-cache 避免旧包残留）
docker compose build --no-cache
docker compose up -d
```

---

### VPS 配置升级

- **同厂商升配**：控制台直接升级 CPU/内存 → 重启 → 服务自动恢复
- **换新 VPS**：在新机器重走 Step 1~9 → DNS A 记录改到新 IP → 验证正常后关旧机

---

### 后续功能扩展（本框架均支持）

| 功能 | 实现方式 |
|------|---------|
| 加登录鉴权 | Nginx Basic Auth 或应用内登录系统 |
| 绑定正式域名 | 改 nginx.conf + certbot 重申请 |
| CI/CD 自动部署 | GitHub Actions SSH 自动部署 |
| 监控告警 | Uptime Kuma / docker logs / Nginx log |
| 多用户并发增大 | 升 VPS 配置或加实例 |

---

## 五、核心概念快速理解

| 概念 | 一句话解释 |
|------|-----------|
| **IP** | 服务器在网络上的地址，如 `1.2.3.4` |
| **域名** | 人类可读的地址，如 `adam.example.com` |
| **DNS** | 把域名翻译成 IP 的系统 |
| **HTTP / HTTPS** | HTTP 明文传输；HTTPS 加密传输（必须用） |
| **SSL 证书** | 证明域名属于你，Let's Encrypt 免费提供 |
| **反向代理** | Nginx 做"前台"，用户先访问 Nginx，再转给 Shiny |
| **Docker** | 把运行环境打包，保证任何地方都能跑 |
| **端口** | 服务器的"门"：80=HTTP，443=HTTPS，3838=Shiny（不对外） |
| **ICP 备案** | 国内大陆服务器需要，香港服务器不需要 |
| **WebSocket** | Shiny 应用的长连接协议，Nginx 必须配置升级头 |

---

## 六、故障排查速查

### 按现象排查

| 现象 | 优先检查 |
|------|---------|
| 域名完全打不开 | DNS 是否生效：`nslookup 域名` |
| HTTP 能开，HTTPS 证书报错 | certbot 是否申请成功，nginx.conf 证书路径 |
| 返回 `502 Bad Gateway` | Docker 容器是否正常：`docker compose ps` |
| 页面加载但操作无响应 | WebSocket 是否断连，查应用日志 |
| 上传 CSV 失败/超时 | `client_max_body_size`（已设 100M），文件是否过大 |
| LLM 请求一直转圈 | provider 是否可达；香港访问 DeepSeek/Qwen 最稳定 |
| 字体显示异常 | 已用系统字体，应无问题 |

### 排查层级顺序

```bash
# 1. DNS 层
nslookup 你的域名

# 2. 防火墙层
ufw status

# 3. Nginx 层
nginx -t
systemctl status nginx
tail -100 /var/log/nginx/error.log

# 4. Docker 层
docker compose ps
docker compose logs -f

# 5. 应用层（直连容器验证）
curl http://127.0.0.1:3838
```

---

## 七、日常运维命令速查

```bash
# 容器管理
docker compose ps               # 查容器状态
docker compose logs -f          # 实时日志
docker compose logs --tail=200  # 最近 200 行日志
docker compose restart          # 重启容器（不重建）
docker compose up -d --build    # 重建并启动
docker compose down             # 停止并删除容器

# Nginx 管理
sudo nginx -t                   # 测试配置语法
sudo systemctl reload nginx     # 热重载配置（不中断连接）
sudo systemctl restart nginx    # 完全重启

# SSL 证书
sudo certbot renew --dry-run    # 测试续期（不实际申请）
sudo certbot certificates       # 查看当前证书状态

# 系统监控
df -h                           # 磁盘使用
free -h                         # 内存使用
sudo ss -tulpn                  # 查所有监听端口
docker stats                    # 容器资源实时占用
```

---

## 八、推荐 VPS 规格

| 资源 | 最低 | 推荐 |
|------|------|------|
| CPU | 2 核 | 4 核 |
| 内存 | 4 GB | 8 GB |
| 磁盘 | 40 GB | 80 GB SSD |
| 系统 | Ubuntu 22.04 LTS | Ubuntu 22.04 LTS |
| 地域 | 香港 | 香港 |
| 推荐服务商 | 阿里云香港 / 腾讯云香港 | 阿里云香港 / 腾讯云香港 |

> 每次"生成代码"最多占用 1 主进程 + 2 parallel worker。小团队 < 10 人并发推荐配 4 核 8G。

---

## 九、已完成的代码修改清单

| 文件 | 修改内容 | 状态 |
|------|---------|------|
| `app.R` | 禁用启动自动装包；host 改 `0.0.0.0` | ✅ 已提交 |
| `server.R` | exec_parent 沙箱加固（屏蔽网络/文件写入/环境变量） | ✅ 已提交 |
| `code_static_checks.R` | 增加网络访问和环境变量读取的静态检测规则 | ✅ 已提交 |
| `ui.R` | 移除 `font_google()`，改用系统字体栈 | ✅ 已提交 |
| `Dockerfile` | rocker/shiny:4.3，renv 预装包，curl，日志目录权限 | ✅ 新建 |
| `docker-compose.yml` | 资源限制，healthcheck，只绑本机端口 | ✅ 新建 |
| `shiny-server.conf` | `app_dir` 单应用模式，10 分钟空闲超时 | ✅ 新建 |
| `nginx/adam.conf` | HTTPS，WebSocket，300s 超时，100M 上传 | ✅ 新建 |
| `.dockerignore` | 排除 git/rsconnect/编辑器缓存 | ✅ 新建 |

---

## 十、上线前检查清单

```
部署环境
[ ] 香港 VPS 已购买（Ubuntu 22.04，4核8G 推荐）
[ ] 域名已购买并添加 A 记录指向 VPS IP
[ ] DNS 已生效（nslookup 验证）
[ ] 云控制台安全组已放行 22/80/443

VPS 配置
[ ] Docker 已安装并验证（docker run hello-world）
[ ] Nginx 已安装并启动
[ ] 防火墙 UFW 已配置

应用部署
[ ] git clone deploy/vps-docker 分支成功
[ ] docker compose build 无报错
[ ] docker compose up -d 容器状态 healthy
[ ] curl http://127.0.0.1:3838 返回正常

Nginx + SSL
[ ] nginx/adam.conf 中域名已修改
[ ] sudo nginx -t 测试通过
[ ] certbot --nginx 证书申请成功
[ ] https://你的域名 可正常访问

功能验证
[ ] 上传 demo-data/ 的 dm.csv / ex.csv / ae.csv
[ ] 输入 DeepSeek/Qwen API Key，点击生成
[ ] 代码生成成功，ADaM 数据集展示正常
[ ] 点击下载，CSV 正常导出
```
