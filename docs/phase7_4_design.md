# Phase 7.4 Design - General Dependency Resolution and LLM-Driven Target Generation

Last updated: 2026-05-24

Phase status: dependency availability and decision gate implemented; LLM generation not started

## Purpose

Phase 7.4 connects the existing study-level dependency orchestration to the
first LLM-driven downstream ADaM generation path.

The goal is not to special-case `ADAE -> ADSL`. The goal is a general rule:

```text
If a requested target dataset depends on another ADaM dataset,
the system must check whether the required dependency dataset is available.

If the dependency is missing, the system must ask for a user decision.
It must not automatically generate unrequested dependency datasets unless the
user or run configuration explicitly approves that action.
```

ADAE can be the first test case, but the code design must work for any ADaM
dependency discovered by the dependency planner.

## Core Principle

Separate three ideas:

```text
dependency discovery
  The system may automatically discover that target ADTTE needs ADLB and ADSL.

dependency availability
  The system must check whether ADLB and ADSL already exist as usable artifacts.

dependency execution
  The system must not silently execute missing dependencies. The user decides
  whether to provide them, let the system generate them, or pause/skip.
```

This avoids turning dependency planning into hidden work execution.

## General Flow

Example:

```text
requested_datasets = ["ADTTE"]

DependencyPlanner:
  ADTTE depends on ADLB
  ADLB depends on ADSL

DependencyAvailabilityChecker:
  ADSL is available from reference_adam or prior run output
  ADLB is missing

DependencyDecisionGate:
  user_action_required for ADLB
  choices:
    - provide_existing_dataset
    - approve_system_generation
    - skip_target
    - pause_run
```

Only after dependency resolution can the target dataset enter real generation:

```text
target spec + source SDTM + resolved dependencies
  -> LLM context package
  -> LLM R code generation
  -> R sandbox
  -> output ADaM dataset
  -> validation
  -> audit
```

## Non-Special-Casing Rule

Do not create fields or functions such as:

```text
missing_adsl
run_adsl_for_adae
adae_requires_adsl_prompt
```

Use general names:

```text
required_dependency_datasets
dependency_resolution
missing_dependency_datasets
approved_dependency_generation
resolved_dependency_artifacts
```

ADSL is a common dependency, not a hard-coded product rule.

## Dependency Availability Sources

A dependency dataset is considered available if the system can point to a usable
artifact for it.

MVP sources:

```text
reference_adam/{dataset}.csv
reference_adam/{dataset}.sas7bdat
runs/{run_id}/outputs/{dataset}.csv
runs/{run_id}/outputs/{dataset}.sas7bdat
```

Phase 7.4 separates "found on disk" from "usable by this dependency gate." A
CSV dependency artifact must at least be readable enough to confirm a header.
A `.sas7bdat` dependency artifact may be found, but Python-side dependency
profiling is not available in this phase, so it is recorded as
`found_but_unusable` and still requires user action or a later reader path.

Later sources:

```text
prior run outputs selected by the user
uploaded dependency artifacts
enterprise data store references
```

Reference ADaM can satisfy an output-shape or dependency-availability need. It
still cannot decide derivation logic by itself.

## Dependency Decision State

Add a general dependency resolution record shape.

Suggested first shape:

```json
{
  "target_dataset": "ADTTE",
  "required_dataset": "ADLB",
  "available": false,
  "artifact_id": null,
  "resolution_status": "user_action_required",
  "allowed_actions": [
    "provide_existing_dataset",
    "approve_system_generation",
    "skip_target",
    "pause_run"
  ],
  "selected_action": null,
  "reason": "ADTTE depends on ADLB, but no usable ADLB artifact was found."
}
```

Possible `resolution_status` values:

```text
available
found_but_unusable
user_action_required
approved_for_system_generation
provided_by_user
skipped
blocked
```

## Execution Rule

The StudyGraph should only execute:

```text
requested target datasets
dependencies explicitly approved for system generation
```

It should not execute every dataset in `target_datasets` simply because the
planner discovered a dependency chain.

This means the current `auto_added_datasets` field should be treated carefully.
In Phase 7.4, UI/review language should prefer:

```text
required_dependency_datasets
```

over:

```text
auto_added_datasets
```

The old field may remain for compatibility, but the user-facing meaning should
be:

```text
required by dependency planning, not automatically approved for execution
```

Execution readiness must also check the dependency chain, not only the direct
target edge.

Example:

```text
requested_datasets = ["ADTTE"]
approved_dependency_datasets = ["ADLB"]

ADTTE depends on ADLB
ADLB depends on ADSL
ADSL is missing
```

In this case the graph must not run ADLB or ADTTE. It should record:

```text
ADLB blocked by ADSL
ADTTE blocked by ADLB
```

This keeps the rule general for any multi-step ADaM dependency chain.

## LLM-Driven Generation Boundary

Once dependencies are resolved, a target dataset can use the LLM generation path.

First target example:

```text
ADAE
  inputs: AE + resolved ADSL artifact + input_spec/adae_spec.*
  output: runs/{run_id}/outputs/adae.csv
```

But the code path should be general:

```text
target_dataset
  input_sdtm domains/profile
  input_spec for target_dataset
  resolved_dependency_artifacts
  output_path
```

## LLM Context Package

The LLM should not receive only a raw spec file. The system should build a
context package:

```json
{
  "target_dataset": "ADAE",
  "target_spec": "...",
  "source_dataset_profiles": {
    "AE": {
      "columns": ["USUBJID", "AETERM", "AESTDTC"],
      "row_count": 100
    }
  },
  "resolved_dependencies": {
    "ADSL": {
      "path": "runs/run_001/outputs/adsl.csv",
      "columns": ["USUBJID", "TRTSDT", "SAFFL"]
    }
  },
  "runtime_contract": {
    "language": "R",
    "output_path": "runs/run_001/outputs/adae.csv",
    "no_network": true,
    "write_only_to_run_dir": true
  }
}
```

For current demo work, richer context can be allowed when the run explicitly
uses `demo_rich_context`.

## LLM Output Contract

The first output contract should be strict enough to audit:

```json
{
  "dataset": "ADAE",
  "r_code": "...",
  "assumptions": [],
  "risk_points": [],
  "used_inputs": ["AE", "ADSL"],
  "expected_outputs": ["adae.csv"]
}
```

The system writes:

```text
runs/{run_id}/code/build_{dataset}.R
runs/{run_id}/llm/{dataset}_prompt.json
runs/{run_id}/llm/{dataset}_response.json
```

Then it executes the R code through the existing R runner boundary.

## Validation MVP

The first validation should be structural, not a CDISC compliance claim.

Minimum checks:

```text
output file exists
required key columns exist, especially USUBJID when applicable
row count is readable
no duplicate key when a unique key is declared in spec
all expected output files were written under the run directory
```

Do not call this regulatory-grade validation.

## Failure Handling

Phase 7.4 should record structured failures:

```text
missing_dependency
dependency_user_action_required
llm_generation_error
llm_output_parse_error
r_runtime_error
validation_error
spec_error
```

If a dependency is missing and no user approval exists, the route should be:

```text
human_review
```

not:

```text
auto_generate_dependency
```

## Proposed Subtasks

1. Add dependency availability checking.
2. Add generic dependency decision records.
3. Update StudyGraph execution so discovered dependencies are not silently run
   unless already requested or explicitly approved.
4. Add a real OpenAI-compatible LLM client behind the existing LLM interface.
5. Add LLM context builder for a target dataset.
6. Add LLM output parser and R code artifact writer.
7. Add generic downstream ADaM runner.
8. Use ADAE as the first test fixture, without hard-coding ADAE/ADSL logic.
9. Add tests for:
   - dependency available
   - dependency missing and user action required
   - dependency approved for system generation
   - LLM response parsed into R code
   - generated R script executed through R runner
   - audit files written

## Implementation Notes

Implemented so far:

- Added a generic dependency resolution layer.
- StudyGraph now records dependency resolution records for requested targets.
- A dependency can be resolved as:
  - available through an existing artifact
  - explicitly requested in the current run
  - explicitly approved for system generation
  - user action required
- Current MVP artifact availability checks:
  - `reference_adam/{dataset}.csv`
  - `reference_adam/{dataset}.sas7bdat`
  - `runs/{run_id}/outputs/{dataset}.csv`
  - `runs/{run_id}/outputs/{dataset}.sas7bdat`
- Missing dependencies block the target with:
  `dependency_user_action_required`.
- StudyGraph no longer silently executes discovered dependencies. It runs only:
  - requested datasets
  - dependency datasets explicitly approved for system generation
- StudyGraph checks that the dependency chain is runnable before execution. If a
  user approves a middle dependency but that dependency still needs a missing
  parent, both the middle dependency and final requested target stay blocked.
- Existing dependency artifacts satisfy dependency requirements without running
  that dependency in the current batch.
- Added an LLM context package builder that does not call a provider. It scans
  study inputs, loads the target spec artifact when present, profiles source
  SDTM files, profiles resolved dependency artifacts, and writes an auditable
  `runs/{run_id}/llm/{dataset}_context.json` package.
- The context builder follows the exposure policy:
  - `metadata_only` includes columns and row counts but no sample rows
  - `demo_rich_context` may include the configured sample rows
- Unreadable or missing dependency artifacts create context warnings instead of
  being silently trusted.
- Dependency availability now distinguishes usable artifacts from merely found
  files. `.sas7bdat` dependency artifacts are not marked `available` by the
  Python dependency gate in Phase 7.4; they become `found_but_unusable`.
- Added a strict LLM generated-code response parser for the first JSON contract:
  `dataset`, `r_code`, `assumptions`, `risk_points`, `used_inputs`, and
  `expected_outputs`.
- Added generated-code artifact writing:
  - raw LLM response under `runs/{run_id}/llm/{dataset}_response.json`
  - parsed response under `runs/{run_id}/llm/{dataset}_parsed_response.json`
  - generated R script under `runs/{run_id}/code/build_{dataset}.R`
- Added a small MVP guardrail that rejects obvious unsafe generated R calls such
  as `system()`, `shell()`, `unlink()`, `download.file()`, and
  `install.packages()` before R execution.
- Added a generic downstream runner that wires:
  `context package -> LLM client -> generated-code parser/writer -> R runner ->
  minimal output validation`.
- The downstream runner is generic by dataset name. It is not hard-coded to
  `ADAE`, although ADAE is used as the first test fixture.
- The runner can use a fixed mock LLM response and a stub R runner, so the
  chain can be tested without API keys or local R.
- The default stub runner reports `structural_stub_pass` and
  `not_real_derivation = true` rather than ordinary `pass`. This means the
  orchestration and artifact boundary worked; it does not mean a clinically
  meaningful downstream ADaM derivation was produced.
- Dataset summaries for stubbed downstream runs use `status = completed_stub`
  so callers that only inspect the summary status still see the prototype
  boundary.
- Wired the generic downstream runner into `DatasetGraph` behind the explicit
  mode `execution_mode = "llm_downstream_stubbed"`.
- StudyGraph now passes dependency-resolution records into each dataset task so
  downstream dataset graphs can build dependency-aware LLM context packages.
- The graph-level downstream mode writes context, response, generated R code,
  minimal output, and validation artifacts into the study run folder.
- Dependency-resolution records are filtered per target before context
  construction, so one downstream target cannot accidentally receive another
  target's resolved ADaM dependencies.
- Unusable dependency profiles now block downstream structural success instead
  of being hidden inside warnings.
- Full test suite after the target-filtering and boundary fixes:
  `Ran 86 tests ... OK`.

Not implemented yet:

- Graph-level use of a real LLM provider call.
- Real downstream R execution through local R for a non-ADSL target.
- Real downstream ADaM validation beyond structural checks.
- Stubbed downstream runs are smoke tests. A `structural_stub_pass` must not be
  described as real ADaM generation.

Latest LLM client boundary:

- Added an OpenAI-compatible chat-completions client behind the same
  `LLMClient.generate()` interface used by the mock client.
- The client accepts model IDs from configuration rather than from a hard-coded
  allowlist, so newer model names can be used without changing graph logic.
- External calls require both an API key and an `LLMExposureConfig` that allows
  external API use.
- Tests use a fake transport and do not call the network.
- StudyGraph/DatasetGraph still default to mock/stub mode; real provider use
  requires explicit client injection in a later wiring step.

Provider expansion boundary:

- Added a provider config/factory layer so run configuration can select
  `mock`, `openai`, `openai-compatible`, `deepseek`, `qwen`, `anthropic`, or
  `claude` without changing graph nodes.
- DeepSeek and Qwen use the OpenAI-compatible transport because their common
  API mode follows the OpenAI chat-completions shape.
- Claude/Anthropic uses a separate Anthropic Messages client because the
  request headers, request body, and response content blocks differ from
  OpenAI-compatible APIs.
- Audit records separate the provider identity from the transport protocol:
  `provider/provider_alias` records who the data was sent to, while `transport`
  records the protocol used.
- Custom `base_url` values are treated as relay risk. They require
  `allow_custom_base_url = true` and `custom_base_url_approved_by`, and the
  audit record marks `external_relay`.
- A generic `custom-http` client is intentionally not implemented yet. Most
  relay services should use OpenAI-compatible mode first; nonstandard relay
  formats can be added later with narrower tests and audit controls.

## Exit Criteria

Phase 7.4 is complete when:

- The graph distinguishes dependency discovery from dependency execution.
- Missing dependency datasets create a user decision record instead of silent
  automatic execution.
- Existing dependency artifacts can satisfy dependency requirements.
- At least one downstream ADaM dataset can be generated through:
  `input_spec + source profiles + resolved dependencies -> LLM R code -> R
  sandbox -> output file`.
- The implementation uses general dependency names and does not hard-code
  `ADAE -> ADSL` behavior.
- Prompt, response, generated code, R result, output, validation, and audit
  artifacts are recorded.

## Out of Scope

- Product UI.
- Full CDISC/P21 validation.
- Production-grade SAS parser.
- Production-grade define.xml parser.
- Generating all ADaM dataset types.
- Automatic multi-step repair loops.
- Automatically generating every missing dependency without user approval.
