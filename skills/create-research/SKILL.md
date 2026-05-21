---
name: create-research
description: Answer QRSPi v2 questions with current-state codebase research
---

# QRSPi v2: Create Research

## Recommended Model

Use the strongest available model with the highest reasoning setting for this stage when possible.

You are creating the **Research** artifact for the QRSPi v2 workflow.

Answer objective questions by documenting the codebase as it exists today. The result becomes compressed context for later design, structure, and plan sessions.

## Core Invariant

> Research is compressed current-state context for downstream sessions.

## Critical Rule: Document, Do Not Design

Your only job is to document and explain the current system.

- Do not suggest improvements unless explicitly asked.
- Do not perform root-cause analysis unless explicitly asked.
- Do not propose future enhancements.
- Do not critique the implementation.
- Do not recommend refactoring, optimization, or architecture changes.
- Only describe what exists, where it exists, how it works, and how components interact.

## Initial Response

If no questions artifact or task context is provided, infer the latest relevant artifact from `thoughts/shared/questions/`.

If inference is ambiguous, respond with:

```text
I need a questions artifact to research. Please provide a path under `thoughts/shared/questions/`, or clarify which ticket/slug to use.
```

## Artifact Inference

When no explicit path is provided, infer upstream artifacts by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible questions artifact in `thoughts/shared/questions/`.

If more than one plausible match exists, stop and ask the user.

## Process

### 1. Read upstream artifacts fully

Read fully, in the main context:

- The questions artifact.
- Any referenced ticket/task files.
- Any directly mentioned context files.

### 2. Build a research todo list

Turn the question groups into concrete research tasks. Preserve the question grouping so the final artifact can answer each group.

### 3. Spawn focused research agents where available

Use parallel subagents for breadth and context efficiency.

Recommended usage:

- `codebase-locator`: find where relevant files/components live.
- `codebase-analyzer`: explain how specific code works.
- `codebase-pattern-finder`: find similar existing patterns.
- `thoughts-locator`: find historical docs about the topic.
- `thoughts-analyzer`: extract insights from the most relevant docs.
- `web-search-researcher`: only if external/web research was explicitly requested.

Tell every subagent they are documenting what exists, not evaluating or improving it.

### 4. Wait for all research to complete

Do not synthesize until all spawned research tasks complete.

Cross-check findings and read key files yourself when necessary to verify important claims.

### 5. Write the research artifact

Write to:

```text
thoughts/shared/research/YYYY-MM-DD[-ENG-XXXX]-description.md
```

Use this structure:

```markdown
---
date: [ISO datetime]
source_questions: `thoughts/shared/questions/...`
ticket: "[ENG-XXXX or null]"
status: complete
tags: [research, codebase, qrspi-v2]
---

# Research: [Topic]

## Source Questions

- `thoughts/shared/questions/...`

## Summary

[Concise current-state answer]

## Detailed Findings

### [Area]

[What exists, where it exists, how it works, and how it connects]

## Answers by Question Group

### [Question Group]

- **Q:** [question]
  **A:** [evidence-based answer with file references]

## Code References

- `path/to/file.ext:123` - [what is there]

## Architecture Documentation

[Current patterns and boundaries found]

## Historical Context

[Relevant `thoughts/` docs, preserving exact paths except removing only `searchable/` when applicable]

## Related Artifacts

- Questions: `thoughts/shared/questions/...`

## Open Research Gaps

[Any areas not answerable from available code/docs; do not turn these into design recommendations]
```

### 6. Present the handoff

End with:

```text
Research artifact created: `thoughts/shared/research/...`

Next step: start a fresh session with `.agents/skills/create-design/SKILL.md`.
Pass the task/ticket and this research artifact explicitly, or allow it to infer the latest matching artifacts by ticket ID/slug.
```

## Path Handling

If using `thoughts/searchable/`, document canonical paths by removing only `searchable/` and preserving all other path segments.

Examples:

- `thoughts/searchable/allison/old_stuff/notes.md` -> `thoughts/allison/old_stuff/notes.md`
- `thoughts/searchable/shared/prs/123.md` -> `thoughts/shared/prs/123.md`

Never change ownership directories such as `allison/` to `shared/`.
