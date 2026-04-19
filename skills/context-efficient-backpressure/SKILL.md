---
name: context-efficient-backpressure
description: Use this skill when a repo has noisy test, build, lint, or verification commands and you want deterministic low-noise output for coding agents. It provides a bundled `scripts/ctxrun` wrapper that collapses successful runs to one line and dumps captured output only on failure, with parser support for Vitest, Playwright, Pytest, and Jest.
---

# Context-Efficient Backpressure

Use the bundled `scripts/ctxrun` helper from this skill for commands that tend to flood agent context with success noise.

## Workflow

1. Resolve `scripts/ctxrun` relative to this skill directory and invoke that path directly.
2. Wrap the noisy command with `<skill-dir>/scripts/ctxrun --desc "..." --framework ... -- ...`.
3. Prefer direct runner commands over opaque package scripts when that makes fail-fast flags easy to add.
4. If the repo uses package scripts like `pnpm test`, keep `--framework` explicit so `ctxrun` can still summarize the run.
5. Do not assume `ctxrun` is on `PATH`.
6. Do not wrap interactive commands, watch mode, or anything that depends on a TTY stream.

## Good Defaults

- Prefer fail-fast options so the agent sees one actionable failure at a time.
- Keep the repo's native failure output intact; `ctxrun` already hides success spam.
- Re-run with `--verbose` only when you need live streaming output.

## Examples

Vitest:

```bash
<skill-dir>/scripts/ctxrun --desc "unit tests" --framework vitest -- pnpm vitest run --bail=1
```

Playwright:

```bash
<skill-dir>/scripts/ctxrun --desc "e2e tests" --framework playwright -- pnpm playwright test --max-failures=1
```

Pytest:

```bash
<skill-dir>/scripts/ctxrun --desc "backend tests" --framework pytest -- pytest -x
```

Jest:

```bash
<skill-dir>/scripts/ctxrun --desc "unit tests" --framework jest -- pnpm jest --bail
```

Lint or build:

```bash
<skill-dir>/scripts/ctxrun --desc "eslint" -- pnpm eslint .
<skill-dir>/scripts/ctxrun --desc "typecheck" -- pnpm tsc --noEmit
<skill-dir>/scripts/ctxrun --desc "production build" -- pnpm build
```

## Notes

- Testing Library is not a runner. Wrap the Vitest or Jest command that executes those tests.
- Recent Vitest versions already have an AI-agent-oriented reporter. Still use `ctxrun` when you want the same success/failure shape across Vitest, Playwright, lint, and build commands.
