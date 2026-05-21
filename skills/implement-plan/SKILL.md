---
name: implement-plan
description: Implement a QRSPi v2 plan phase by phase with verification
---

# QRSPi v2: Implement Plan

You are performing the **Implement** stage for the QRSPi v2 workflow.

You implement an approved plan from `thoughts/shared/plans/` with verification and human checkpoints.

## Initial Response

If no plan path is provided, ask:

```text
Which plan should I implement? Please provide a relative path under `thoughts/shared/plans/`.
```

If a plan path is provided, begin immediately.

This skill is expected to run from the isolated worktree created by `create-worktree`, not from the source checkout that prepared the worktree. At startup, confirm the provided path is relative and begins with `thoughts/shared/plans/`. If the current checkout does not contain that plan path, stop and ask the user to reopen the implementation session in the worktree printed by `create-worktree`. Alternatively, if they are ok not using a worktree, allow the session to proceed without one.

## Getting Started

1. Read the plan completely.
2. Check for existing checkmarks (`- [x]`) and resume from the first incomplete item.
3. Read the original ticket and all source artifacts referenced by the plan when needed.
4. Read all files mentioned in the current phase fully before editing.
5. Think through how the pieces fit together.
6. Create a todo list to track implementation progress.

## Implementation Philosophy

Plans are carefully designed, but reality can be messy.

Your job is to:

- Follow the plan's intent while adapting to codebase reality.
- Implement each phase fully before moving to the next.
- Keep phases vertical and verifiable.
- Verify your work in the broader codebase context.
- Update checkboxes in the plan as implementation and automated checks complete.

## Mismatch Protocol

If the plan does not match the codebase:

1. Stop.
2. Think carefully about why the plan cannot be followed.
3. Present the mismatch clearly:

```text
Issue in Phase [N]:
Expected: [what the plan says]
Found: [actual situation]
Why this matters: [explanation]

How should I proceed?
```

Do not silently redesign the feature.

## Phase Workflow

For each phase:

1. Read phase instructions.
2. Read referenced files fully.
3. Make the smallest coherent vertical change.
4. Add/update tests specified by the plan.
5. Run automated verification for the phase.
6. Fix failures before proceeding.
7. Update completed implementation and automated verification checkboxes in the plan.
8. Pause for manual verification unless explicitly instructed to continue.

## Verification Approach

After implementing a phase, run the success criteria checks from the plan.

If verification passes, report:

```text
Phase [N] Complete - Ready for Manual Verification

Automated verification passed:
- [List automated checks]

Please perform the manual verification steps listed in the plan:
- [List manual verification items]

Let me know when manual testing is complete so I can proceed to Phase [N+1].
```

Do not check off manual testing steps until the user confirms them.

If instructed to execute multiple phases consecutively, skip pauses until the requested stopping point, but still report manual verification items clearly.

## If You Get Stuck

- Re-read the relevant plan section and source artifacts.
- Re-read the current code.
- Use subagents sparingly for targeted debugging or unfamiliar areas.
- Ask for guidance if proceeding would require changing design decisions.

## Completion Handoff

When all phases are implemented and automated verification passes, end with:

```text
Implementation complete for `thoughts/shared/plans/...`.

Next steps:
1. Commit the changes using the repository's commit instructions.
2. Open or update a pull request.
3. Start a fresh session with `.agents/skills/describe-pr/SKILL.md` to generate/update the PR description.
```
