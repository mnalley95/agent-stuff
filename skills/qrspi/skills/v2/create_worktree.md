---
description: Create a worktree and launch prompt for implementing a QRSPi v2 plan
---

# QRSPi v2: Create Worktree

You are preparing the **Worktree** stage for the QRSPi v2 workflow.

This stage decomposes old RPI **Implement** into environment setup before implementation. It creates or describes an isolated worktree and provides the launch command for the implementation session.

## Initial Response

If no plan path is provided, infer the latest compatible plan from `thoughts/shared/plans/`.

If inference is ambiguous, ask:

```text
Which plan should I create a worktree for? Please provide a path under `thoughts/shared/plans/` or a ticket/slug to infer from.
```

## Artifact Inference

Prefer explicit paths. Otherwise infer by:

1. Ticket ID, e.g. `ENG-XXXX`.
2. Shared slug/description in filenames.
3. Most recent compatible plan artifact in `thoughts/shared/plans/`.

If multiple plausible matches exist, stop and ask.

## Process

### 1. Read the plan fully

Read the selected plan artifact completely.

Extract:

- Ticket ID, if any.
- Feature/slug.
- Branch naming hints.
- Any implementation/session instructions.

### 2. Determine worktree details

Determine:

- Branch name.
- Worktree path.
- Relative plan path.
- Launch prompt.
- Command to run.

Use only relative plan paths beginning with:

```text
thoughts/shared/plans/...
```

Do not use absolute plan paths in the launch prompt.

### 3. Use repository worktree helper if available

If the repository has a worktree helper, read it before use.

For HumanLayer-style repos this may be:

```text
hack/create_worktree.sh
```

Example pattern:

```bash
./hack/create_worktree.sh ENG-XXXX branch-name
```

If no helper exists, use the repository's established worktree convention or ask the user.

### 4. Confirm with the user

Before creating or launching anything, present:

```text
Based on the plan, I intend to create a worktree with:

worktree path: [path]
branch name: [branch]
plan path: [relative path]
launch prompt:

    /qrspi/skills/v2/implement_plan.md at [relative plan path]

command to run:

    [full command]

Please confirm or adjust these details.
```

### 5. Create the worktree after confirmation

After user confirmation, create the worktree using the chosen command/helper.

### 6. Present the implementation handoff

End with:

```text
Worktree ready: `[path]`

Next step: start a fresh implementation session in that worktree with `qrspi/skills/v2/implement_plan.md` and the relative plan path:

`thoughts/shared/plans/...`
```

## Notes

- Do not implement auto-advance orchestration.
- Do not start implementation unless the user explicitly asks you to.
- The purpose of this stage is to isolate implementation context and provide a clean launch handoff.
