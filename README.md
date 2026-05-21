# ADaM Agent Studio

This branch is the architecture starting point for a LangGraph-based ADaM
generation product.

The old Shiny prototype is not copied here. This branch starts from an empty
tree so the new architecture can be built without carrying prototype coupling
forward.

Primary documents:

- `CODEX.md`: project-level instructions for Codex and other AI coding tools
- `docs/langgraph_adam_architecture.html`: readable architecture explanation
- `docs/langgraph_execution_roadmap.md`: staged execution and handoff ledger

Initial direction:

- local-first product
- LangGraph orchestration
- per-dataset isolated state
- deterministic tools around LLM calls
- R sandbox execution
- audit-first clinical-data workflow
