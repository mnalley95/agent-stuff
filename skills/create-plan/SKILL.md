---
name: create-plan
description: Create a detailed vertical implementation plan from QRSPi v2 structure
---

# QRSPi v2: Create Plan

## Recommended Model

Use the strongest available model with the highest reasoning setting for this stage when possible.

You are creating the **Plan** artifact for the QRSPi v2 workflow.

Convert the approved design and structure into a complete, actionable implementation plan for a fresh implementation agent.

## Core Invariant

> The plan must be vertical, verifiable, and implementable by a fresh agent.

## Hard Rules

- Do not write a final plan with unresolved open questions.
- Do not create horizontal phases like database -> services -> API -> frontend unless each phase independently proves a working vertical slice.
- Include automated and manual verification for every phase.
- Include pause points for manual verification.
- Include exact files/areas and concrete changes.
- Keep the plan practical enough for an implementation agent to follow without redoing design.

## Initial Response

If no structure artifact is provided, infer the latest compatible artifacts from:

- `thoughts/shared/structures/`
- `thoughts/shared/designs/`
- `thoughts/shared/research/`
- `thoughts/shared/questions/`

If inference is ambiguous, ask the user which artifact to use.

## Artifact Inference

Prefer explicit paths. Otherwise infer by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible structure artifact.
4. Matching design/research/questions artifacts by ticket/slug.

If multiple plausible matches exist, stop and ask.

## Process

### 1. Read upstream artifacts fully

Read fully, in main context:

- Structure artifact.
- Design artifact.
- Research artifact.
- Questions artifact if available.
- Task/ticket if referenced.
- Any directly mentioned files.

### 2. Verify readiness to plan

Before writing, confirm:

- Design decisions are resolved.
- Structure phases are vertical and verifiable.
- Research provides enough current-state context.
- Any remaining unknowns can be addressed inside implementation without changing design.

If not, stop and ask or send the user back to the appropriate prior stage.

### 3. Create detailed implementation phases

For each phase include:

- Overview.
- Files/components to change.
- Specific changes required.
- Tests to add/update.
- Automated verification commands.
- Manual verification steps.
- Pause instructions.

### 4. Write the plan artifact

Write to:

```text
thoughts/shared/plans/YYYY-MM-DD[-ENG-XXXX]-description.md
```

Use this structure:

````markdown
---
date: [ISO datetime]
source_structure: `thoughts/shared/structures/...`
source_design: `thoughts/shared/designs/...`
source_research: `thoughts/shared/research/...`
source_questions: `thoughts/shared/questions/...`
ticket: "[ENG-XXXX or null]"
status: draft
tags: [plan, implementation, qrspi-v2]
---

# [Feature/Task Name] Implementation Plan

## Overview

[Brief description of what is being implemented and why]

## Source Artifacts

- Structure: `thoughts/shared/structures/...`
- Design: `thoughts/shared/designs/...`
- Research: `thoughts/shared/research/...`
- Questions: `thoughts/shared/questions/...`
- Ticket/task: [path or summary]

## Current State Analysis

[What exists now, based on research]

## Desired End State

[Specification of the desired end state and how to verify it]

## Key Discoveries

- [Important finding with file reference]
- [Pattern to follow]
- [Constraint]

## What We're NOT Doing

[Explicit non-goals]

## Implementation Approach

[High-level strategy and reasoning]

## Vertical Plan Check

Explain why the phases are vertical and independently verifiable. If any phase is mostly horizontal, justify why it is still safe and how it will be verified.

## Phase 1: [Descriptive Name]

### Overview

[What this phase accomplishes]

### Working Slice Proven

[What works after this phase]

### Changes Required

#### 1. [Component/File Group]

**File(s):** `path/to/file.ext`

**Changes:** [Specific changes]

```[language]
// Optional illustrative code/pseudocode, only when useful
```

### Tests

- [Tests to add/update]

### Success Criteria

#### Automated Verification

- [ ] [Command/check]

#### Manual Verification

- [ ] [Human check]

**Implementation Note:** After completing this phase and all automated verification passes, pause for manual confirmation before proceeding unless explicitly instructed to continue.

---

## Phase 2: [Descriptive Name]

[Same structure]

---

## Testing Strategy

### Unit Tests

[What to test]

### Integration Tests

[What to test]

### Manual Testing Steps

1. [Step]
2. [Step]

## Performance Considerations

[Any relevant concerns]

## Migration / Rollback Notes

[If applicable]

## References

- [Relevant artifacts and files]
````

### 5. Present the handoff

End with:

```text
Plan artifact created: `thoughts/shared/plans/...`

Next step: start a fresh session with `.agents/skills/create-worktree/SKILL.md`.
Pass this plan artifact explicitly, or allow it to infer the latest matching plan by ticket ID/slug.
```
