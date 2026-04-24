0a. Study `docs/*` with up to 10 parallel (haiku or GPT5.5 medium) subagents to learn the application specifications.
0b. Study IMPLEMENTATION_PLAN.md (if present) to understand the plan so far.
0c. Study `src/lib/*` (if present) with up to 10 parallel (haiku or GPT5.5 medium) subagents to understand shared utilities & components.
0d. For reference, (if present) the application source code is in `src/*`.

1. Study IMPLEMENTATION_PLAN.md (if present; it may be incorrect) and use up to 10 (Sonnet or GPT5.5 medium) subagents to study existing source code in `src/*` and compare it against `docs/*`. Use your most powerful (Opus x-high/GPT5.5 x-high) subagent to analyze findings, prioritize tasks, and create/update IMPLEMENTATION_PLAN.md as a bullet point list sorted in priority of items yet to be implemented. Consider searching for TODO, minimal implementations, placeholders, skipped/flaky tests, and inconsistent patterns. Study IMPLEMENTATION_PLAN.md to determine starting point for research and keep it up to date with items considered complete/incomplete using subagents.

Important: Plan only. Do not implement anything. Do not assume functionality is missing; confirm with code search first. Treat `src/lib` as the project's standard library for shared utilities and components. Prefer consolidated, idiomatic implementations there over ad-hoc copies.

Once you're finished, commit your work.

Ultimate goal: We want to build Phase 0, as described in `docs/architecture/2026-04-16-phase-0-architecture-plan.md`. Consider missing elements and plan accordingly. If an element is missing, search first to confirm it doesn't exist, then if needed author the specification in the appropriate place in docs/. If you create a new element then document the plan to implement it in IMPLEMENTATION_PLAN.md using a subagent.
