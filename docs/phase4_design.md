# Phase 4 Design - Tool Layer MVP

Last updated: 2026-05-23

Phase status: implemented locally, awaiting user review and commit

## Purpose

Phase 4 creates stable tool interfaces between the LangGraph orchestration layer
and deterministic operations.

The graph should decide what happens next. Tools should do the concrete work.

This prevents graph nodes from directly handling:

- scattered file reads and writes
- artifact hashing and manifest entries
- provider-specific LLM calls
- R process execution
- study-folder scanning
- run configuration parsing

## Plain-Language Idea

Phase 3 proved that the graph skeleton can run.

Phase 4 gives the skeleton controlled "hands":

```text
Graph node
  -> asks ArtifactStore to register/read/write artifacts
  -> asks StudyInputScanner to find SDTM/spec/reference files
  -> asks LLMClient to generate text through a replaceable interface
  -> asks RRunner to execute or simulate R
  -> asks ConfigLoader for run policy
```

In Phase 4, these tools are mostly thin and deterministic. The goal is not to
build the final data engine yet.

## Non-Goals

Phase 4 should not implement:

- real ADaM derivation
- real LLM provider calls
- real R sandbox process execution
- full CSV/sas7bdat parsing
- full define.xml parsing
- production database persistence
- production security controls

Those belong to later phases.

## Proposed Modules

```text
src/adam_agent/tools/
  artifacts.py      # artifact references, hashing, manifest records
  study_inputs.py   # study folder scanning
  sdtm_reader.py    # lightweight SDTM metadata/profile stub
  r_runner.py       # R execution interface plus stub runner
  config.py         # run config loader and defaults

src/adam_agent/llm/
  clients.py        # LLM client protocol and mock client
  model_registry.py # provider/model registry

tests/
  test_tools_phase4.py
```

## Tool 1 - ArtifactStore

Purpose:

Centralize file references, hashes, and manifest records.

MVP behavior:

- compute sha256 for existing files
- create `ArtifactRef` objects using Phase 2 schema
- register artifact refs in memory
- write a manifest JSON file at the canonical path when asked:

```text
studies/{study_id}/runs/{run_id}/audit/manifest.json
```

- keep all run outputs under `studies/{study_id}/runs/{run_id}/`

Why it matters:

Graph nodes should not hand-build artifact paths and hashes.

Minimum tests:

- registering an existing file returns an `ArtifactRef`
- manifest JSON is written to `runs/{run_id}/audit/manifest.json`
- manifest JSON contains study id, run id, and artifact ids
- missing file raises a clear error

## Tool 2 - StudyInputScanner

Purpose:

Find canonical input folders and map files to artifact roles.

MVP behavior:

- scan this local shape:

```text
studies/{study_id}/
  input_sdtm/
  input_define/
  input_spec/
  reference_adam/
  legacy_code/
  runs/
```

- recognize `.csv` and `.sas7bdat` as data inputs
- recognize `.sas` as legacy code/text evidence, not data
- normalize domain/dataset names by filename stem, e.g. `dm.csv` -> `DM`
- return a structured input index, not a path list

The structured index should contain:

```text
input_sdtm: domain -> ArtifactRef
reference_adam: dataset -> ArtifactRef
input_spec: filename stem -> ArtifactRef
input_define: filename stem -> ArtifactRef
legacy_code: filename stem -> ArtifactRef
invalid_files: list of path/reason records
warnings: list of messages
```

Why it matters:

Phase 5 can ask for `DM` and `EX` without manually searching the folder.

Minimum tests:

- `dm.csv` is recognized as SDTM `DM`
- `ADSL.sas` is recognized as legacy code
- `.sas` under `input_sdtm` is rejected or flagged as invalid data
- scanner returns `ArtifactRef` objects, roles, formats, warnings, and invalid files

## Tool 3 - SDTMReader / Profiler

Purpose:

Provide a lightweight profile of source data.

MVP behavior:

- read CSV headers and a small number of rows using Python standard library
- return metadata such as column names, row count estimate, sample rows
- for `.sas7bdat`, return a clear `not_implemented_yet` result unless an
  optional reader is later added

Why it matters:

Phase 5 needs metadata for prompt construction and validation setup.

Minimum tests:

- CSV profile returns columns and sample rows
- sample row count respects config limits
- sas7bdat path returns `not_implemented_yet`, not a silent fake success

## Tool 4 - LLM Client Interface

Purpose:

Isolate graph logic from provider-specific APIs.

MVP behavior:

- define an `LLMClient` protocol with a `generate()` method
- implement `MockLLMClient`
- mock client requires no API key and returns deterministic text
- produce an `LLMCallRecord` for audit
- enforce exposure mode rules from `LLMExposureConfig`

`LLMClient.generate()` input contract:

```text
prompt
provider
model
LLMExposureConfig
datasets_included
variables_included
sample_row_counts
full_data_included
prompt_artifact_id
response_artifact_id
```

`LLMClient.generate()` output contract:

```text
response_text
LLMCallRecord
```

It must not return a bare string.

Mock behavior example:

```text
prompt -> MockLLMClient -> fixed deterministic response
```

Why no API key is needed:

Phase 4 tests the product architecture, not provider connectivity. Real
provider clients come later and should implement the same interface.

Minimum tests:

- mock call works without environment variables
- metadata-only call cannot include sample rows
- call record includes provider, model, exposure mode, prompt/response artifact ids
- mock call creates prompt and response hashes/artifact ids even for `metadata_only`

## Tool 5 - ModelRegistry

Purpose:

Keep model names and provider capabilities out of business logic.

MVP behavior:

- register `mock/mock-model`
- expose lookup by provider and model name
- return provider locality and whether API key is required
- explicitly reject `openai/*`, `anthropic/*`, and `openai-compatible/*` as
  `not_implemented_in_phase4`
- never fall back from a real provider name to mock

Why it matters:

Later we can add OpenAI-compatible, OpenAI, Anthropic, local models, and newer
model ids without rewriting graph nodes.

Minimum tests:

- mock model lookup succeeds
- unknown model raises a clear error
- real provider lookup raises `not implemented in Phase 4`, not mock fallback

## Tool 6 - RRunner Stub

Purpose:

Create an execution boundary for R code.

MVP behavior:

- define `RRunner` interface
- implement `StubRRunner`
- simulate success or failure
- return structured execution result with stdout, stderr, exit code, and artifact refs
- do not run a real subprocess yet

Why it matters:

Phase 5 can replace the stub with a real sandbox runner while graph nodes keep
the same call shape.

Minimum tests:

- stub success returns exit code 0
- stub failure returns nonzero exit code and error text
- result is structured, not plain prose

## Tool 7 - ConfigLoader

Purpose:

Load run configuration and default policies.

MVP behavior:

- return a default config if no file is provided
- support explicit demo config:

```text
data_classification = processed_demo
external_api_allowed = true
mode = demo_rich_context
```

- otherwise default to `metadata_only`

ConfigLoader may accept a user-facing alias such as `llm_exposure_mode`, but it
must normalize into the existing `LLMExposureConfig.mode` field. Do not create a
second exposure schema.

In Phase 4, `external_api_allowed=true` is only a policy rehearsal for future
external providers. A real external call still requires all of these:

```text
non-mock provider
implemented provider client
valid API key or credential
run config allowing external API
```

Why it matters:

LLM data exposure must be a run configuration decision, not a graph guess.

Minimum tests:

- default config uses `metadata_only`
- demo config creates valid `LLMExposureConfig`
- invalid full-data config without approver fails
- config returns existing schema objects such as `LLMExposureConfig`

## Proposed Phase 4 Tests

```text
tests/test_tools_phase4.py
```

Coverage:

- artifact registration and manifest writing
- study folder scanning
- CSV profiling
- sas7bdat not-implemented boundary
- mock LLM call without API key
- LLM exposure policy enforcement
- model registry lookup
- R runner success/failure stub
- config defaults and demo override
- boundary anti-regression: tools return existing schema objects such as
  `ArtifactRef`, `LLMCallRecord`, and `LLMExposureConfig`

## Key Design Claims To Review

| Claim | Reason | Failure condition |
|---|---|---|
| Graph nodes should call tools, not providers directly. | Keeps orchestration clean and swappable. | A graph node imports OpenAI/Anthropic/R subprocess directly. |
| Mock LLM is required in Phase 4. | Tests architecture without API keys or network. | Unit tests require a real API key. |
| Mock LLM must still be auditable. | Mock mode should test the same audit shape as real clients. | `generate()` returns only a string. |
| ModelRegistry must not fall back real providers to mock. | Prevents false confidence about OpenAI/Anthropic support. | `openai/gpt-*` silently uses mock. |
| R runner should be stubbed first. | Avoids mixing orchestration with sandbox complexity. | Phase 4 blocks on local R installation. |
| CSV can be profiled now, sas7bdat can be explicit not-implemented. | Keeps MVP honest while preserving file boundary. | sas7bdat is silently treated as parsed. |
| ArtifactStore owns paths/hashes/manifests. | Prevents scattered file I/O. | Tools and graph nodes each invent their own manifest format. |
| Config owns LLM exposure mode. | Data governance requires explicit run policy. | Graph auto-escalates data exposure. |

## Proposed Exit Criteria

Phase 4 is complete when:

- tool interfaces exist and are importable
- mock LLM runs without API keys
- artifact manifest can be written
- study inputs can be scanned
- CSV profile works for small fixture data
- R runner stub returns structured success/failure
- tests pass without network, API keys, or real R
- roadmap and review HTML explain what remains stubbed
