# Study Template

Use this folder shape for local test studies.

- `input_sdtm/`: source SDTM datasets
- `input_define/`: define.xml or extracted metadata
- `input_spec/`: ADaM specs or spec-equivalent contracts
- `reference_adam/`: validated reference ADaM outputs when available
- `outputs/`: generated ADaM outputs
- `audit/`: run manifests, reports, and decision logs

Clinical data should remain local by default. LLM calls should use metadata,
summaries, or explicitly approved samples unless the user changes that policy.
