---
name: create-design
description: Create an interactive design artifact from QRSPi v2 research
---

# QRSPi v2: Create Design

## Recommended Model

Use the strongest available model with the highest reasoning setting for this stage when possible.

You are creating the **Design** artifact for the QRSPi v2 workflow.

Turn the task and research into an explicit design discussion, resolve human-judgment questions, and record the chosen approach.

## Core Invariant

> No unresolved product or technical judgment should flow into structure.

## Initial Response

If no task, ticket, research artifact, or questions artifact is provided, infer the latest compatible artifacts from:

- `thoughts/shared/research/`
- `thoughts/shared/questions/`

If inference is ambiguous, ask the user which ticket/slug/artifact to use.

## Artifact Inference

Prefer explicit paths. Otherwise infer by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible research artifact.
4. Matching questions artifact for the selected research artifact.

If there are multiple plausible matches, stop and ask.

## Process

### 1. Read upstream artifacts fully

Read fully, in main context:

- Task/ticket file or user-provided task text.
- Research artifact.
- Questions artifact if available.
- Any directly referenced files.

### 2. Restate current-state understanding

Before proposing designs, summarize:

- What the task is asking for.
- What the research established about current code.
- Relevant constraints and existing patterns.
- Any research gaps that affect design.

### 3. Generate design options

Present realistic design options with tradeoffs.

For each option include:

- Summary.
- How it fits current architecture.
- Benefits.
- Risks/complexity.
- Verification implications.
- Files/modules likely involved at a high level.

Do not write low-level implementation steps yet.

### 4. Ask focused human questions

Ask only questions that require human/product/design judgment and cannot be answered by code research.

Examples:

- Product behavior choices.
- UX copy/interaction choices.
- Policy or compatibility decisions.
- Risk tolerance for migrations or rollout.

Do not ask the human questions that code research can answer. If needed, do follow-up research instead.

### 5. Require proof when understanding is uncertain

If a key assumption can be verified with a small proof, learning test, fixture, storybook state, or executable check, call it out and ask for/define that proof before finalizing design.

The goal is to make the model prove important assumptions rather than merely assert them.

### 6. Resolve the design

Incorporate user feedback and choose/design the final approach.

Do not complete while product or technical judgment questions remain unresolved. If unresolved questions remain, stop and ask.

### 7. Write the design artifact

Write to:

```text
thoughts/shared/designs/YYYY-MM-DD[-ENG-XXXX]-description.md
```

Use this structure:

```markdown
---
date: [ISO datetime]
source_research: `thoughts/shared/research/...`
source_questions: `thoughts/shared/questions/...`
ticket: "[ENG-XXXX or null]"
status: complete
tags: [design, qrspi-v2]
---

# Design: [Topic]

## Source Artifacts

- Research: `thoughts/shared/research/...`
- Questions: `thoughts/shared/questions/...`
- Ticket/task: [path or summary]

## Current-State Summary

[What research established]

## Problem / Desired Outcome

[What needs to be true after implementation]

## Non-Goals

[Explicit exclusions]

## Design Options Considered

### Option A: [Name]

- Summary:
- Benefits:
- Tradeoffs:
- Verification implications:

### Option B: [Name]

...

## Decisions

[Final decisions and rationale]

## Resolved Questions

- **Question:** [question]
  **Decision:** [answer]
  **Rationale:** [why]

## Proofs / Learning Tests

[Any proofs requested or completed, and what they established]

## Risks and Constraints

[Known risks to carry into structure]

## Handoff to Structure

[Concise notes the structure stage should preserve]
```

### 8. Present the handoff

End with:

```text
Design artifact created: `thoughts/shared/designs/...`

Next step: start a fresh session with `.agents/skills/create-structure/SKILL.md`.
Pass this design artifact explicitly, or allow it to infer matching design/research/questions artifacts by ticket ID/slug.
```
