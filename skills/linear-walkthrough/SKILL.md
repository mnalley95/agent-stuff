---
name: linear-walkthrough
description: Create a detailed, linear code walkthrough in `walkthrough.md` for the current repository. Use this skill when asked to read the source, explain how the code works in execution order, or build a walkthrough with `showboat`, especially when the user wants commentary plus captured code snippets.
---

# Linear Walkthrough

Use this skill to produce a repo-local `walkthrough.md` that explains the codebase in the order a reader should follow it.

## Goals

- Read the source before writing.
- Explain the system as a linear story, not a bag of unrelated modules.
- Use `showboat` to make the walkthrough reproducible.
- Mix commentary with small, relevant code excerpts and command output.

## Workflow

### 1. Explore first

Before creating the walkthrough:

- Identify the entrypoints, major modules, and execution flow.
- Read enough source to understand the real control path.
- Build a short plan for the walkthrough order before writing into `walkthrough.md`.

The walkthrough should usually move through:

1. repo purpose and top-level shape
2. entrypoint or starting command
3. primary control flow
4. important data structures and state transitions
5. secondary subsystems that support the main path
6. tests, edge cases, or operational details that clarify behavior

### 2. Learn `showboat` first

Run:

```bash
uvx showboat --help
```

Use a normal double-dash `--help` even if the user typed a typographic dash.

Treat that help text as the contract. The commands you will normally need are:

- `showboat init`
- `showboat note`
- `showboat exec`
- `showboat pop`
- `showboat verify`

### 3. Create `walkthrough.md`

If `walkthrough.md` does not exist, initialize it:

```bash
uvx showboat init walkthrough.md "Code Walkthrough"
```

If it already exists, read it and extend or replace it deliberately rather than blindly appending duplicate material.

### 4. Build the walkthrough with notes plus evidence

Use `showboat note` for commentary that explains:

- what section the reader is in
- why the next file or snippet matters
- how control moves from one component to the next
- what invariants, branches, or side effects matter

Use `showboat exec` to capture the exact snippets you are discussing. Prefer shell commands such as:

- `sed -n 'start,endp' <file>`
- `rg -n '<pattern>' <path>`
- `cat <small-file>`
- `ls`, `find`, or similar orientation commands when they support the story

Keep excerpts tight. Do not dump entire large files when a 10 to 60 line slice will do.

Example pattern:

```bash
uvx showboat note walkthrough.md "The walkthrough starts at the CLI entrypoint, which parses arguments and hands off to the main application flow."
uvx showboat exec walkthrough.md bash "sed -n '1,120p' src/cli.ts"
```

### 5. Keep the narrative linear

Do not organize the document as a directory listing. Each section should answer:

- where execution is now
- what this code receives
- what it decides or transforms
- where execution goes next

When a module exists mainly to support another step, introduce it at the point where the main flow first depends on it.

### 6. Use `showboat` correctly

- Prefer many small `note` and `exec` entries over a few huge ones.
- If an `exec` step is wrong or noisy, remove it with `uvx showboat pop walkthrough.md` and redo it.
- Keep commands reproducible from the repo root. Use `--workdir` if needed.
- Use shell snippets that are safe to re-run and stable enough for `showboat verify`.

### 7. Finish cleanly

Before stopping:

- Read `walkthrough.md` end to end.
- Make sure the story is detailed and internally consistent.
- Verify the document if practical:

```bash
uvx showboat verify walkthrough.md
```

## Quality bar

The finished walkthrough should:

- help a new contributor understand the code in the right order
- include enough detail to explain real behavior, not just labels
- quote only the code needed to support the explanation
- stay grounded in source excerpts captured with `showboat exec`
