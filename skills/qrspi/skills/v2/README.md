---
description: Overview of the QRSPi v2 decomposed workflow
---

# QRSPi v2 Workflow

QRSPi v2 decomposes the original RPI workflow into smaller, fresh-context stages.

The core idea from the source transcript is:

> none of the instructions have changed; they were broken down.

## Mapping from RPI to v2

```text
Research  -> Questions + Research
Plan      -> Design + Structure + Plan
Implement -> Worktree + Implement + PR
```

## Full v2 Sequence

```text
Questions -> Research -> Design -> Structure -> Plan -> Worktree -> Implement -> PR
```

## Skill Files

```text
qrspi/skills/v2/create_questions.md
qrspi/skills/v2/create_research.md
qrspi/skills/v2/create_design.md
qrspi/skills/v2/create_structure.md
qrspi/skills/v2/create_plan.md
qrspi/skills/v2/create_worktree.md
qrspi/skills/v2/implement_plan.md
qrspi/skills/v2/describe_pr.md
```

## Artifact Flow

```text
thoughts/shared/questions/...   -> created by create_questions.md
thoughts/shared/research/...    -> created by create_research.md
thoughts/shared/designs/...     -> created by create_design.md
thoughts/shared/structures/...  -> created by create_structure.md
thoughts/shared/plans/...       -> created by create_plan.md
worktree                        -> prepared by create_worktree.md
code changes                    -> made by implement_plan.md
thoughts/shared/prs/...         -> created by describe_pr.md
```

## Stage Responsibilities

### 1. Questions

Turns a task/ticket into objective research questions.

Questions are for the model to answer by inspecting the codebase, not for the human to answer during design.

### 2. Research

Answers the questions by documenting the current codebase.

Research describes what exists today. It does not recommend, critique, redesign, or implement.

### 3. Design

Converts task + research into design options and final decisions.

Design is the main human-in-the-loop stage. It should resolve product and technical judgment questions before structure.

### 4. Structure

Turns the approved design into vertical implementation phases and checkpoints.

Structure defines the shape of implementation, not detailed edits.

### 5. Plan

Writes the detailed implementation plan.

Plans must be vertical, verifiable, and implementable by a fresh agent. They must not contain unresolved open questions.

### 6. Worktree

Prepares an isolated implementation worktree and launch prompt.

No auto-advance orchestration is implemented here.

### 7. Implement

Implements the plan phase by phase, runs verification, updates checkboxes, and pauses for manual verification.

### 8. PR

Generates or updates the PR description from the diff, commits, template, and verification state.

## Manual Command Sequence

Use explicit artifact paths when possible:

```text
qrspi/skills/v2/create_questions.md [task/ticket]
qrspi/skills/v2/create_research.md thoughts/shared/questions/...
qrspi/skills/v2/create_design.md thoughts/shared/research/...
qrspi/skills/v2/create_structure.md thoughts/shared/designs/...
qrspi/skills/v2/create_plan.md thoughts/shared/structures/...
qrspi/skills/v2/create_worktree.md thoughts/shared/plans/...
qrspi/skills/v2/implement_plan.md thoughts/shared/plans/...
qrspi/skills/v2/describe_pr.md
```

Each skill also includes handoff instructions for the next skill.

## Artifact Inference

Each skill should prefer explicit paths. If paths are omitted, infer by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible artifact in the expected upstream directory.

If inference is ambiguous, stop and ask the user.

## No Auto-Advance Yet

This v2 pack does not implement auto-advance orchestration.

Each skill should complete its artifact and clearly tell the user which next skill to launch with which inferred or explicit artifact.

## Vertical Planning Principle

The structure and plan stages should avoid horizontal layer sequencing such as:

```text
database -> services -> API -> frontend
```

Prefer vertical phases where each phase proves a working behavior or reduces a concrete risk. A human should be able to validate progress before the implementation has accumulated a large, hard-to-debug surface area.
