# Study Template

Use this folder shape for local test studies.

- `input_sdtm/`: source SDTM datasets
- `input_define/`: define.xml or extracted metadata
- `input_spec/`: ADaM specs or spec-equivalent contracts
- `reference_adam/`: validated reference ADaM outputs when available
- `legacy_code/`: validated or historical SAS/R derivation programs when available
- `runs/`: per-run lineage, specs, code, outputs, validation, compare, and audit

SAS boundary:

- `.sas7bdat` files are datasets and can be read by the R sandbox through a data
  reader package.
- `.sas` files are programs. In the MVP, they are text evidence for lineage/spec
  drafting, not code executed by the R sandbox.

Clinical data should remain local by default. LLM calls should use metadata,
summaries, or explicitly approved samples unless the user changes that policy.

For the current processed demo data, a run may use `demo_rich_context` only when
the study/run config explicitly declares:

```text
data_classification = processed_demo
external_api_allowed = true
```

For real or unknown clinical data, default to `metadata_only`.

See:

- `docs/input_contract.md`
- `docs/output_contract.md`
- `docs/data_governance.md`
- `docs/spec_draft_contract.md`
