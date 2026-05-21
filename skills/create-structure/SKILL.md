---
name: create-structure
description: Create a vertical implementation structure outline from QRSPi v2 design
---

# QRSPi v2: Create Structure

## Recommended Model

Use the strongest available model with the highest reasoning setting for this stage when possible.

You are creating the **Structure** artifact for the QRSPi v2 workflow.

Turn the approved design into vertical phases, checkpoints, and validation shape before writing detailed implementation instructions.

## Core Invariant

> Structure defines phases, checkpoints, and validation shape; plan defines exact edits.

## Vertical Slice Rule

Models tend to create horizontal plans like:

1. Database
2. Services
3. API
4. Frontend

Reject that structure unless each phase independently produces a testable vertical result.

Prefer phases that cut through layers and prove a behavior incrementally, using stubs, mocks, wiring, migrations, or UI states as needed.

Each phase should answer:

- What working slice exists after this phase?
- How can a human or automated check verify it before continuing?
- What risk is reduced by this phase?

## Initial Response

If no design artifact is provided, infer the latest compatible artifacts from:

- `thoughts/shared/designs/`
- `thoughts/shared/research/`
- `thoughts/shared/questions/`

If inference is ambiguous, ask the user which artifact to use.

## Artifact Inference

Prefer explicit paths. Otherwise infer by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible design artifact.
4. Matching research/questions artifacts by ticket/slug.

If multiple plausible matches exist, stop and ask.

## Process

### 1. Read upstream artifacts fully

Read fully, in main context:

- Design artifact.
- Research artifact.
- Questions artifact if available.
- Task/ticket if referenced.
- Any directly mentioned files.

### 2. Summarize approved design

Briefly restate:

- Desired outcome.
- Chosen approach.
- Important constraints.
- Non-goals.
- Risks or proofs from design.

### 3. Draft vertical phases

Create a high-level outline of implementation phases.

For each phase include:

- Phase name.
- Purpose.
- Behavior or capability proven by the phase.
- Likely components touched at a high level.
- Automated verification ideas.
- Manual verification ideas.
- Whether the phase should pause for human review.

Do not include detailed code snippets or line-by-line edits. Save those for the plan stage.

### 4. Validate structure quality

Before writing the artifact, check:

- Are phases vertical rather than horizontal?
- Can each phase be verified independently?
- Are manual checkpoints present where human judgment matters?
- Does the structure preserve the design decisions?
- Are there unresolved questions? If yes, stop and ask or return to design.

### 5. Write the structure artifact

Write to:

```text
thoughts/shared/structures/YYYY-MM-DD[-ENG-XXXX]-description.md
```

Use this structure:

```markdown
---
date: [ISO datetime]
source_design: `thoughts/shared/designs/...`
source_research: `thoughts/shared/research/...`
source_questions: `thoughts/shared/questions/...`
ticket: "[ENG-XXXX or null]"
status: complete
tags: [structure, qrspi-v2]
---

# Structure: [Topic]

## Source Artifacts

- Design: `thoughts/shared/designs/...`
- Research: `thoughts/shared/research/...`
- Questions: `thoughts/shared/questions/...`

## Approved Design Summary

[Concise recap]

## Structuring Principles

- Phases are vertical and independently verifiable.
- Avoid layer-only sequencing.
- Include automated and manual checkpoints.
- Keep implementation details for the plan stage.

## Phase Outline

### Phase 1: [Name]

**Purpose:** [what this phase accomplishes]

**Working slice proven:** [what works after this phase]

**Likely areas touched:**
- [component/module]

**Automated verification ideas:**
- [check]

**Manual verification ideas:**
- [check]

**Pause for human review:** yes/no and why

### Phase 2: [Name]

...

## Validation Strategy

[How confidence increases across phases]

## Risks Carried Into Planning

[Things detailed plan must handle]

## Handoff to Plan

[Specific guidance for detailed planning]
```

### 6. Present the handoff

End with:

```text
Structure artifact created: `thoughts/shared/structures/...`

Next step: start a fresh session with `.agents/skills/create-plan/SKILL.md`.
Pass this structure artifact explicitly, or allow it to infer matching structure/design/research artifacts by ticket ID/slug.
```
