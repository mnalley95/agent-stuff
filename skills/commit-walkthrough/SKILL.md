---
name: commit-walkthrough
description: Use this skill when the user asks to walk through a specific git commit, trace the logic that was implemented, explain function-call flow, explain business logic or reconciliation semantics, or generate ASCII diagrams for a code change. Anchor the explanation to the commit itself using git history rather than only the current working tree.
---

# Commit Walkthrough

Explain a commit as implemented behavior, not as diff narration.

## Workflow

1. Inspect the commit metadata and changed paths first.
2. Read the relevant files from the commit snapshot with `git show <sha>:<path>` so the explanation stays anchored to the commit itself.
3. Read the relevant tests from the same commit when available; use them to infer intended semantics.
4. If the current tree has drifted since the commit, say so explicitly and distinguish "in the commit" from "in the current tree."
5. Trace execution from the public entrypoint through helpers, repositories, contract constructors, hashing/serialization helpers, and persistence boundaries in call order.
6. Pick one representative business scenario from the tests or the domain and single-step it through the implementation with concrete values.
7. Explain reconciliation semantics carefully: intersect, union, precedence, narrowing, widening, omission vs explicit empty values, min/max, OR flags, fallbacks, and any other merge rules that matter.
8. Call out Phase 0 assumptions, stubbed adapters, missing precedence rules, or stored-but-not-enforced fields.

## Response Order

Always order the answer like this unless the user asks for a narrower slice:

1. High-level summary
2. ASCII diagrams
3. Execution trace
4. Business scenario walkthrough
5. Nuance and edge cases
6. Caveats, drift, or limitations

Put the ASCII diagrams second, immediately after the high-level summary.

## ASCII Diagrams

Include two small diagrams when they add clarity:

- A business-flow diagram showing the inputs, reconciliation step, and resulting behavior
- A sequence/function-call diagram using the real function names from the code

Keep diagrams compact and readable in plain text.

## Output Rules

- Prefer explaining behavior and intent over paraphrasing the diff.
- Cite the most important files and line numbers.
- If tests do not cover a behavior you are describing, say so.
- If the commit introduces business rules, translate them into operational meaning for a real scenario.
- When the commit mixes architectural work and business logic, explain both, but keep the execution path concrete.
