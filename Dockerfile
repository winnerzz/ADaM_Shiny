FROM python:3.12-slim

ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    DEBIAN_FRONTEND=noninteractive \
    ADAM_AGENT_PRODUCT_STUDY_ROOT=/app/workspace/studies \
    ADAM_AGENT_DEMO_STUDY_ROOT=/app/workspace/demo_studies \
    ADAM_AGENT_RSCRIPT_PATH=/usr/bin/Rscript \
    ADAM_AGENT_GRAPH_CHECKPOINTER_BACKEND=sqlite \
    ADAM_AGENT_LIVE_LLM_TIMEOUT=300

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
      r-base \
      r-cran-haven \
      r-cran-jsonlite \
      r-cran-readr \
      r-cran-dplyr \
      r-cran-lubridate \
      r-cran-stringr \
      ca-certificates \
    && rm -rf /var/lib/apt/lists/*

COPY pyproject.toml README.md ./
COPY src ./src
COPY studies/_template ./studies/_template

RUN python -m pip install --upgrade pip \
    && python -m pip install .

RUN mkdir -p /app/workspace/studies /app/workspace/demo_studies

EXPOSE 8005

CMD ["python", "-m", "uvicorn", "adam_agent.api.app:app", "--host", "0.0.0.0", "--port", "8005"]
