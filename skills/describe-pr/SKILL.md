---
name: describe-pr
description: Generate or update a PR description for the QRSPi v2 workflow
---

# QRSPi v2: Describe PR

You are performing the **PR** stage for the QRSPi v2 workflow.

This stage is final pull-request description and verification handoff after implementation.

## Initial Response

Begin by identifying the PR for the current branch.

If no PR exists or the repository is on `main`/`master`, list open PRs and ask the user which PR to describe.

## Process

### 1. Read the PR template

Check for:

```text
thoughts/shared/pr_description.md
```

If it does not exist, tell the user their thoughts setup is incomplete and ask them to create the template.

Read the template fully.

### 2. Identify the PR

Use GitHub CLI where available:

```bash
gh pr view --json url,number,title,state 2>/dev/null
```

If no PR exists for the current branch or current branch is main/master, list PRs:

```bash
gh pr list --limit 10 --json number,title,headRefName,author
```

Ask the user which PR to describe if ambiguous.

### 3. Check existing description artifact

Check whether this exists:

```text
thoughts/shared/prs/{number}_description.md
```

If it exists, read it and update it rather than ignoring it.

### 4. Gather PR information

Gather:

- Full PR diff: `gh pr diff {number}`
- Commit history: `gh pr view {number} --json commits`
- Base branch: `gh pr view {number} --json baseRefName`
- PR metadata: `gh pr view {number} --json url,title,number,state`

Read referenced files as needed to understand context not visible in the diff.

### 5. Analyze changes

Understand:

- What changed.
- Why it changed.
- User-facing impact.
- Internal implementation impact.
- Tests and verification.
- Migration or rollout notes.
- Risks or manual checks remaining.

### 6. Run verification where possible

For checklist items in the PR template:

- Run commands you can run safely.
- Mark passing command checks as complete.
- Leave manual checks unchecked and explain what the user must do.
- If a command fails, leave it unchecked and include the failure summary.

### 7. Write the PR description artifact

Write to:

```text
thoughts/shared/prs/{number}_description.md
```

Fill out every relevant template section. Be thorough but scannable.

### 8. Update the GitHub PR

Update the PR body:

```bash
gh pr edit {number} --body-file thoughts/shared/prs/{number}_description.md
```

If this fails because no default repository is configured, instruct the user to run:

```bash
gh repo set-default
```

### 9. Present final status

End with:

```text
PR description updated: `thoughts/shared/prs/{number}_description.md`
PR: [URL]

Verification completed:
- [checks]

Manual verification still needed:
- [items]
```
