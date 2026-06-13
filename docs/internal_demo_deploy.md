# ADaM Agent Studio 内部 Demo 部署说明

这份部署只面向内部可信 demo。它把当前本地系统放进 Docker，让其他人通过网页访问。

它不是正式公网产品：当前 R 执行仍是开发级本地 runner，只适合 VPN、内网、Basic Auth 或固定 IP 保护下试用。

## 服务器要求

- Linux 服务器
- Docker
- Docker Compose plugin
- 能拉取 Docker Hub 基础镜像，或已经配置可用的 Docker 镜像源
- 能访问外部 LLM API 的网络环境，如果要用真实模型

## 启动

在服务器上：

```bash
git clone <your-repo-url>
cd ADaM_Shiny_LangGraph
docker compose up -d --build
```

如果服务器无法连接 Docker Hub，需要先配置镜像源，或提前把基础镜像导入服务器。

打开：

```text
http://<server-ip>:8005
```

如果要改宿主机端口：

```bash
ADAM_AGENT_HOST_PORT=8080 docker compose up -d --build
```

然后访问：

```text
http://<server-ip>:8080
```

## 数据保存位置

上传文件、run 状态、生成代码、输出 ADaM 和审计文件保存在 Docker volume：

```text
adam_agent_workspace
```

容器内路径：

```text
/app/workspace/studies
```

不要把项目源码目录当作运行数据目录。

## 临时会话和自动清理

内部 demo 默认开启临时浏览器会话：

- 每次点击 `Start Upload` 会创建一个独立 session study 目录。
- 点击页面右上角 `End Session` 会删除该 session 的上传文件、生成代码、输出和审计文件。
- 浏览器离开页面时会尽量发送清理请求；如果浏览器没有送达，后端会用 TTL 兜底。
- 默认 TTL 是 1 小时，可通过 `ADAM_AGENT_SESSION_TTL_SECONDS` 调整。

Docker demo 每次容器启动时还会清空：

```text
/app/workspace/studies
/app/workspace/demo_studies
```

这适合小服务器上的内部测试，避免上传文件和 run 目录长期堆积。不要把这个 compose 配置直接当作需要长期保存审计记录的生产部署。

## 健康检查

```bash
curl http://127.0.0.1:8005/health
curl http://127.0.0.1:8005/runtime/readiness
```

`/runtime/readiness` 应该显示：

- managed workspace ready
- R execution ready
- SAS7BDAT preview ready
- offline mock LLM ready

## LLM 配置

内部 demo 默认是 mock/offline 模式。真实 LLM 建议在网页右上角 `LLM Config` 里填写 provider、base URL、model 和 API key。

不要把真实 API key 写进 `Dockerfile`、`docker-compose.yml` 或 git 仓库。

## 建议的访问保护

不要裸露到公网。至少使用其中一种：

- VPN
- 服务器防火墙只允许固定 IP
- Nginx/Caddy Basic Auth
- 内网访问

## 常用命令

查看日志：

```bash
docker compose logs -f
```

重启：

```bash
docker compose restart
```

停止：

```bash
docker compose down
```

删除 demo 数据 volume：

```bash
docker compose down -v
```

这会删除上传文件、runs、outputs 和审计记录。

## 当前边界

- 当前部署是单容器内部 demo。
- Python API、LangGraph、Rscript 在同一个容器内。
- Docker 隔离比本机直接运行更干净，但还不是生产级 R 沙盒。
- 后续正式化应拆出独立 R worker，并限制网络、CPU、内存和执行时间。
