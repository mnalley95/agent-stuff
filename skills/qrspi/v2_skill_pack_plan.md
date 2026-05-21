# QRSPi v2 Skill Pack Decomposition Plan

## Overview

Create a new standalone v2 skill pack under `qrspi/skills/v2/` that decomposes the existing RPI prompts into smaller context-engineering stages:

- Research -> Questions + Research
- Plan -> Design + Structure + Plan
- Implement -> Worktree + Implement + PR

This is grounded in the transcript's framing: the core RPI instructions do not change; they are broken into smaller prompts/stages with separate artifacts and fresh-context handoffs.

## Current State

Existing files remain untouched:

- `qrspi/skills/research_codebase.md`
- `qrspi/skills/create_plan.md`
- `qrspi/skills/implement_plan.md`
- `qrspi/skills/create_worktree.md`
- `qrspi/skills/describe_pr.md`

The new implementation will add files only under `qrspi/skills/v2/`.

## Desired End State

The repo contains a v2 skill pack with standalone prompts for each stage:

- `qrspi/skills/v2/create_questions.md`
- `qrspi/skills/v2/create_research.md`
- `qrspi/skills/v2/create_design.md`
- `qrspi/skills/v2/create_structure.md`
- `qrspi/skills/v2/create_plan.md`
- `qrspi/skills/v2/create_worktree.md`
- `qrspi/skills/v2/implement_plan.md`
- `qrspi/skills/v2/describe_pr.md`
- `qrspi/skills/v2/README.md`

Each skill is standalone enough to run in a fresh context window. Each skill writes or consumes a separate artifact type and ends with clear manual instructions for launching the next skill using inferred artifacts. No auto-advance orchestration is implemented.

## Artifact Locations

- Questions: `thoughts/shared/questions/YYYY-MM-DD[-ENG-XXXX]-description.md`
- Research: `thoughts/shared/research/YYYY-MM-DD[-ENG-XXXX]-description.md`
- Designs: `thoughts/shared/designs/YYYY-MM-DD[-ENG-XXXX]-description.md`
- Structures: `thoughts/shared/structures/YYYY-MM-DD[-ENG-XXXX]-description.md`
- Plans: `thoughts/shared/plans/YYYY-MM-DD[-ENG-XXXX]-description.md`
- PR descriptions: `thoughts/shared/prs/{number}_description.md`

## Inference Rules

Each skill should accept explicit artifact paths when provided. If no path is provided, it should infer the most relevant/latest artifact by:

1. Ticket ID if present, e.g. `ENG-XXXX`.
2. Shared slug/description in the filename.
3. Most recent compatible artifact in the expected upstream directory.
4. If inference is ambiguous, stop and ask the user which artifact to use.

## Phase 1: Questions + Research

### Files

- `qrspi/skills/v2/create_questions.md`
- `qrspi/skills/v2/create_research.md`

### `create_questions.md`

Extract the question-generation responsibility from `research_codebase.md` and the transcript.

Requirements:

- Read all directly mentioned files fully before doing anything else.
- Convert the task/ticket into objective research questions.
- Questions are for the model to answer, not for the human.
- Do not include implementation proposals, desired designs, or solution hypotheses.
- Group questions by area/component/system concern.
- Include suggested research lanes/agents for each question group.
- Write a questions artifact to `thoughts/shared/questions/...`.
- End with instructions to run `create_research.md`, using the questions artifact.

Core invariant:

> Questions build objective understanding of the codebase as-is.

### `create_research.md`

Extract the documentation/current-state responsibility from `research_codebase.md`.

Requirements:

- Read the questions artifact fully.
- Read any directly mentioned ticket/task/context files fully.
- Use codebase locator/analyzer/pattern-finder agents where available.
- Use thoughts locator/analyzer agents for historical context where relevant.
- Document what exists today: where it lives, how it works, and how components interact.
- Do not critique, recommend, refactor, or design future changes unless explicitly requested.
- Answer each research question with concrete evidence and file references.
- Write a research artifact to `thoughts/shared/research/...`.
- End with instructions to run `create_design.md`, using inferred ticket + research artifacts.

Core invariant:

> Research is compressed current-state context for downstream sessions.

## Phase 2: Design + Structure + Plan

### Files

- `qrspi/skills/v2/create_design.md`
- `qrspi/skills/v2/create_structure.md`
- `qrspi/skills/v2/create_plan.md`

### `create_design.md`

Extract the interactive design-discussion responsibilities from `create_plan.md`.

Requirements:

- Infer/read ticket, questions, and research artifacts.
- Verify upstream artifacts before designing.
- Present current-state understanding before proposing options.
- Generate design options with tradeoffs.
- Ask only questions that require human judgment and cannot be answered by research.
- If the model is uncertain about behavior, request or design a proof/learning-test step before continuing.
- Incorporate user answers into the design.
- Do not complete with unresolved design/product judgment questions.
- Write a design artifact to `thoughts/shared/designs/...`.
- End with instructions to run `create_structure.md`, using inferred design + research artifacts.

Core invariant:

> No unresolved product or technical judgment should flow into structure.

### `create_structure.md`

Create the structure/outline stage described in the transcript.

Requirements:

- Infer/read ticket, research, and design artifacts.
- Summarize the approved design.
- Produce a high-level implementation outline.
- Define phase boundaries and what each phase proves.
- Emphasize vertical, verifiable slices.
- Reject horizontal layer-by-layer structures such as database -> services -> API -> frontend unless each phase independently produces a testable vertical result.
- Include validation strategy and manual checkpoints.
- Avoid detailed line-by-line implementation instructions.
- Write a structure artifact to `thoughts/shared/structures/...`.
- End with instructions to run `create_plan.md`, using inferred structure + design + research artifacts.

Core invariant:

> Structure defines phases, checkpoints, and validation shape; plan defines exact edits.

### `create_plan.md`

Narrow the existing planning prompt to final plan writing only.

Requirements:

- Infer/read ticket, research, design, and structure artifacts.
- Produce a complete, actionable implementation plan.
- Include exact files, intended changes, testing strategy, and success criteria.
- Preserve the old plan template's useful sections: overview, current state, desired end state, non-goals, implementation approach, phases, automated/manual verification, testing strategy, migration notes, references.
- Make vertical phases a hard rule.
- Include phase pause points for manual verification.
- Do not write a final plan with unresolved open questions.
- Write the plan artifact to `thoughts/shared/plans/...`.
- End with instructions to run `create_worktree.md`, using the plan artifact.

Core invariant:

> The plan must be vertical, verifiable, and implementable by a fresh agent.

## Phase 3: Worktree + Implement + PR

### Files

- `qrspi/skills/v2/create_worktree.md`
- `qrspi/skills/v2/implement_plan.md`
- `qrspi/skills/v2/describe_pr.md`

### `create_worktree.md`

Adapt existing `create_worktree.md` for v2 artifact flow.

Requirements:

- Infer/read the plan artifact.
- Infer ticket ID and branch name when possible.
- Confirm worktree path, branch name, plan path, and launch prompt with the user.
- Use relative plan paths starting with `thoughts/shared/plans/...`.
- Provide the command to launch implementation in the worktree.
- Do not implement auto-advance orchestration.
- End with instructions to run `implement_plan.md` in the worktree.

### `implement_plan.md`

Adapt existing `implement_plan.md` with minimal semantic changes.

Requirements:

- Read the plan completely.
- Read all referenced files fully.
- Implement phase by phase.
- Run automated verification after each phase.
- Update plan checkboxes for completed implementation/automated checks.
- Do not check off manual testing until the user confirms it.
- Stop and ask when the plan does not match codebase reality.
- End with instructions to commit and run `describe_pr.md` after implementation is complete.

### `describe_pr.md`

Adapt existing `describe_pr.md` for v2.

Requirements:

- Read the PR template.
- Identify the current PR or ask the user to choose.
- Inspect diff, commits, metadata, and referenced files as needed.
- Run verification commands where possible.
- Write `thoughts/shared/prs/{number}_description.md`.
- Update the GitHub PR body.
- Clearly report manual verification still required.

## Phase 4: v2 README

### File

- `qrspi/skills/v2/README.md`

Requirements:

- Explain the v2 workflow and RPI decomposition.
- Document artifact flow.
- Document manual command sequence.
- Document inference rules and ambiguity behavior.
- Explain that no auto-advance orchestration is implemented yet.
- Include transcript grounding: the instructions are preserved but broken into smaller context windows/stages.

## Verification

Automated checks:

- `find qrspi/skills/v2 -maxdepth 1 -type f | sort` shows all expected files.
- Each markdown file has frontmatter with a `description`.
- Existing files outside `qrspi/skills/v2/` are unchanged.
- Markdown content consistently references the correct artifact directories.
- No v2 prompt claims auto-advance is implemented.

Manual checks:

- Read each v2 skill as if starting in a fresh context; it should be self-contained.
- Confirm handoff instructions are clear at the end of each skill.
- Confirm research/design/structure/plan boundaries are crisp.
- Confirm vertical planning language is strong enough.

## Non-goals

- Do not modify existing RPI skill files.
- Do not implement real auto-advance orchestration.
- Do not add executable scripts unless later requested.
- Do not change agent definitions.
- Do not migrate existing artifacts.
