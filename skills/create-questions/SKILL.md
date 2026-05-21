---
name: create-questions
description: Create objective research questions from a task or ticket for the QRSPi v2 workflow
---

# QRSPi v2: Create Questions

## Recommended Model

Use the strongest available model with the highest reasoning setting for this stage when possible.

You are creating the **Questions** artifact for the QRSPi v2 workflow.

Turn a task/ticket into objective questions that a fresh research session can answer about the codebase as it exists today.

## Core Invariant

> Questions build objective understanding of the codebase as-is.

Your output is for the model to answer during research, not for the human to answer during design.

## Initial Response

If no task, ticket, or file path is provided, respond with:

```text
I'm ready to create research questions. Please provide the task/ticket description or a path to the ticket/context file.
```

Then wait for input.

If input is provided, begin immediately.

## Process

### 1. Read mentioned files first

- Read all directly mentioned files fully before generating questions.
- Ticket files, docs, JSON, existing research, previous plans, and linked local files must be read in the main context first.
- Do not spawn subagents before reading directly mentioned files.

### 2. Understand the task, but do not design the solution

Identify:

- The user-visible or system-visible goal.
- The likely product/domain area.
- The nouns, states, workflows, or integration points mentioned.
- Any explicit constraints from the task.
- Any existing artifact paths or ticket IDs, especially `ENG-XXXX`.

Do **not**:

- Propose implementation approaches.
- Decide architecture.
- Recommend refactors.
- Include solution hypotheses disguised as questions.
- Ask the human questions unless the task is too ambiguous to research at all.

### 3. Generate objective research questions

Create questions that can be answered by inspecting the current codebase and historical docs.

Good questions:

- Where is this workflow currently represented?
- What state machines, statuses, or events exist today?
- Which APIs, modules, or UI components read/write this data?
- What tests or fixtures cover this behavior today?
- What similar patterns already exist?
- What domain language does the codebase already use?

Bad questions:

- Should we implement option A or B?
- Would it be better to refactor X?
- Can we add a new table for Y?
- Should the UI look like Z?

### 4. Group questions into research lanes

Group questions by area, for example:

- Codebase location
- Current behavior and data flow
- Domain model and state
- API/contracts
- UI/product surfaces
- Persistence/migrations
- Tests and verification patterns
- Historical context in `thoughts/`
- External docs/web research, only if explicitly requested

For each group, include suggested research agents/categories where applicable:

- `codebase-locator` for where things live
- `codebase-analyzer` for how specific code works
- `codebase-pattern-finder` for existing examples
- `thoughts-locator` for historical docs
- `thoughts-analyzer` for extracting relevant decisions
- `web-search-researcher` only when external research is explicitly requested

### 5. Write the questions artifact

Write to:

```text
thoughts/shared/questions/YYYY-MM-DD[-ENG-XXXX]-description.md
```

Use this structure:

```markdown
---
date: [ISO datetime]
source_task: "[task/ticket summary]"
ticket: "[ENG-XXXX or null]"
status: complete
tags: [questions, qrspi-v2]
---

# Research Questions: [Task Title]

## Source Task

[Original task/ticket summary and relevant file references]

## Research Goal

[What the next research stage needs to understand about the existing system]

## Non-Goals

- Do not design the solution.
- Do not propose implementation changes.
- Do not evaluate whether the current implementation is good or bad.

## Question Groups

### 1. [Research Lane]

Suggested agents: [agent names]

- [Objective question]
- [Objective question]

### 2. [Research Lane]

...

## Artifact Handoff

The next stage should answer these questions and write a current-state research artifact.
```

### 6. Present the handoff

End with:

```text
Questions artifact created: `thoughts/shared/questions/...`

Next step: start a fresh session with `.agents/skills/create-research/SKILL.md`.
Pass this questions artifact explicitly, or allow it to infer the latest matching artifact by ticket ID/slug.
```

## Artifact Inference Guidance

If this skill is asked to continue from prior context, infer task identity by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description.
3. Most recent compatible source file.

If inference is ambiguous, stop and ask the user which task/artifact to use.
