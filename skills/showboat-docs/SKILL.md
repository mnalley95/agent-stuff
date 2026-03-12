---
name: showboat-docs
description: Use when you need to prove and document a feature with Showboat, especially by running `uvx showboat --help`, creating or updating `docs/showboat/<feature-name>.md`, capturing executable command output, and verifying that the document reproduces the feature behavior.
---

# Showboat Docs

Use this skill when the task is to document a feature with an executable Showboat demo under `docs/showboat/`.

## Workflow

1. Start from the repo root.
2. Inspect the Showboat CLI before writing anything:
   ```bash
   uvx showboat --help
   ```
3. Read one or two nearby examples in `docs/showboat/` and mirror their level of proof, not just their headings.
4. Choose a kebab-case feature slug and target `docs/showboat/<feature-name>.md`.
5. Build the document with Showboat commands instead of hand-editing Markdown whenever practical.
6. End by verifying the document:
   ```bash
   uvx showboat verify docs/showboat/<feature-name>.md
   ```

## Required Outcome

The resulting document should:

- explain the feature in plain language
- show how the feature is discovered in CLI help when relevant
- run real commands that prove the behavior
- capture output blocks that can be re-verified later
- be safe to re-run on another machine with repo context

## Preferred Command Pattern

Create the document with `init`, then add commentary and executable proof incrementally:

```bash
uvx showboat init docs/showboat/<feature-name>.md "Feature Title"
uvx showboat note docs/showboat/<feature-name>.md "What this feature does and what the document will prove."
uvx showboat exec docs/showboat/<feature-name>.md bash "uv run readwise --help"
uvx showboat exec docs/showboat/<feature-name>.md bash "<command that exercises the feature>"
uvx showboat verify docs/showboat/<feature-name>.md
```

Use `uvx showboat note` for setup explanations, expectations, and interpretation of results.

Use `uvx showboat exec` for all commands whose output matters to the proof.

If a command was wrong or produced noise that should not remain in the artifact, remove it with:

```bash
uvx showboat pop docs/showboat/<feature-name>.md
```

## Repo-Specific Guidance

- Keep Showboat docs in `docs/showboat/`.
- Use the existing documents in `docs/showboat/` as style references before drafting a new one.
- Prefer temporary files or temporary SQLite databases under `/tmp` when the feature needs seeded data.
- Keep the document focused on proof. Avoid long design discussion or implementation history.
- If the feature is exposed through this repo's CLI, capture the relevant `uv run readwise ... --help` output before the behavior demo.
- If the feature needs Python setup, prefer `uv run ...` commands that already match the repo's toolchain.

## Authoring Heuristics

- Seed deterministic test data before running the user-facing command.
- Include at least one successful path and any important edge case that the feature promises to handle.
- When a command intentionally fails to prove validation behavior, capture it explicitly and keep the failure understandable.
- Keep commentary short; let the executable transcript do most of the work.
- If verification fails after edits, update the document by rerunning the broken commands or regenerate into a temporary file with `showboat verify --output`.

## Minimal Example

For a feature named `query-recent-documents`:

```bash
uvx showboat init docs/showboat/query-recent-documents.md "Query Recent Documents (--query-days)"
uvx showboat note docs/showboat/query-recent-documents.md "This document demonstrates the new query flag against a seeded local database."
uvx showboat exec docs/showboat/query-recent-documents.md bash "uv run readwise query --help"
uvx showboat exec docs/showboat/query-recent-documents.md bash "<seed demo data>"
uvx showboat exec docs/showboat/query-recent-documents.md bash "uv run readwise query --days 7 --db-path /tmp/demo.sqlite"
uvx showboat verify docs/showboat/query-recent-documents.md
```
