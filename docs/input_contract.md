# Input Contract

This document defines what the MVP accepts as study input.

Phase:

`Phase 1 - Product and data contracts`

Last updated:

2026-05-21

## Study Folder Shape

Use this shape for local studies:

```text
studies/
  {study_id}/
    input_sdtm/
    input_define/
    input_spec/
    reference_adam/
    legacy_code/
    runs/
```

For test fixtures, use:

```text
studies/_template/
```

## First Supported Formats

Accepted in the first MVP:

- `.csv`
- `.sas7bdat`

Later:

- `.xpt`
- `.xlsx`
- direct define.xml extraction improvements

Reason:

The user agreed to support `csv` and `sas7bdat` first. This is enough for the
current demo and PSY201-style local data while keeping the first implementation
small.

## SAS File Boundary

There are two different SAS-related file types:

| File type | Meaning | MVP handling |
|---|---|---|
| `.sas7bdat` | SAS dataset/table | R sandbox should read it as data through a supported R package such as `haven` or `sas7bdat`. |
| `.sas` | SAS program/code | R sandbox should treat it as text evidence for lineage/spec drafting. It should not try to execute it as SAS code. |

The MVP does not require a SAS runtime.

If a future phase needs to execute `.sas` programs directly, that requires a
separate SAS runtime integration or a SAS-to-R translation workflow. Do not
assume R can run SAS programs natively.

## Minimum Required Inputs for ADSL MVP

For the first real ADSL loop:

```text
input_sdtm/
  dm.csv or dm.sas7bdat
  ex.csv or ex.sas7bdat
```

`DM` provides subject-level demographic and treatment assignment information.
`EX` provides exposure records needed for treatment start/end derivations.

## Optional Inputs

Optional but useful:

```text
input_spec/
  adsl_spec.csv
  adsl_spec.json

reference_adam/
  adsl.csv
  adsl.sas7bdat

input_define/
  define.xml

legacy_code/
  ADSL.sas
```

`legacy_code/` is optional but part of the canonical study shape because legacy
SAS AD programs can be strong evidence for derivation logic.

## Input Roles

| Input | Role |
|---|---|
| `input_sdtm/DM` | Main subject-level source |
| `input_sdtm/EX` | Exposure source for treatment dates and safety population logic |
| `input_spec/adsl_spec.*` | Preferred source for target variables and derivations if available |
| `reference_adam/ADSL` | Strong evidence for target variables, labels, types, and comparison |
| `input_define/define.xml` | Metadata and controlled terminology evidence |
| `legacy_code/ADSL.sas` | Text evidence for derivation logic and join order; not executed by the R sandbox in the MVP |

## Output Path Rule

Generated outputs and audit files belong under a specific run folder:

```text
studies/{study_id}/runs/{run_id}/
```

Do not write canonical generated outputs to top-level `outputs/` or `audit/`
folders in the MVP. If a future UI wants top-level shortcuts, they should be
copies or indexes that point back to the canonical run artifacts.

## Case and Naming Rules

The system should normalize file/domain names during input scan:

- `dm.csv`, `DM.csv`, and `dm.sas7bdat` all represent SDTM `DM`
- `ex.csv`, `EX.csv`, and `ex.sas7bdat` all represent SDTM `EX`
- `adsl.csv`, `ADSL.csv`, and `adsl.sas7bdat` all represent ADaM `ADSL`

Original paths and hashes must still be recorded in the artifact manifest.

## What Happens If Spec Is Missing

Spec is allowed to be missing.

When it is missing, the system must generate a draft spec from evidence. It must
not present this draft as a final approved spec.

The draft spec process is defined in:

```text
docs/spec_draft_contract.md
```
