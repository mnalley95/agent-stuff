---
name: linear-walkthrough
description: Create a detailed, linear code walkthrough in `walkthrough.md` for the current repository. Use this skill when asked to read the source, explain how the code works in execution order, or build a walkthrough with `showboat`, especially when the user wants commentary plus captured code snippets.
---

# Linear Walkthrough

Use this skill to produce a repo-local `walkthrough.md` that explains the codebase in the order a reader should follow it.

## When to use

This technique is most valuable when you need to understand code you didn't write or don't remember writing:

- **Vibe-coded projects** — you prompted an agent to build something and it works, but you don't understand the implementation.
- **Inherited codebases** — you're picking up a project from someone else.
- **Forgotten personal projects** — you wrote it months ago and lost the mental model.

The core insight: by forcing all code snippets to be captured via shell commands (`sed`, `grep`, `cat`) through `showboat exec`, the walkthrough contains **real code from the repo**, not hallucinated approximations. This is what makes the technique trustworthy.

## Goals

- Read the source before writing.
- Explain the system as a linear story, not a bag of unrelated modules.
- Use `showboat` to make the walkthrough reproducible and grounded in real code.
- Mix commentary with small, relevant code excerpts and command output.
- Tailor depth to the reader — a senior engineer familiar with the stack needs less language-level explanation than someone new to it.

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

Not every repo fits this pattern cleanly. Libraries and frameworks may not have a single entrypoint or linear control flow — adapt the ordering to whatever sequence will build understanding most naturally (e.g., public API surface first, then internals that implement it).

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

If `walkthrough.md` does not exist, initialize it with a title that names the project:

```bash
uvx showboat init walkthrough.md "MyProject Walkthrough"
```

If it already exists, read it and extend or replace it deliberately rather than blindly appending duplicate material.

### 4. Build the walkthrough with notes plus evidence

Use `showboat note` for commentary that explains:

- what section the reader is in
- why the next file or snippet matters
- how control moves from one component to the next
- what invariants, branches, or side effects matter

Use `showboat exec` to capture the exact snippets you are discussing. This is critical — **never type out code manually in notes**. Always use a shell command to extract it so the walkthrough contains verified, real code. Prefer commands such as:

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
- Verify the document by running:

```bash
uvx showboat verify walkthrough.md
```

This re-runs every `exec` command and confirms the output still matches. Fix any mismatches before considering the walkthrough done.

## Quality bar

The finished walkthrough should:

- help a new contributor understand the code in the right order
- include enough detail to explain real behavior, not just labels
- quote only the code needed to support the explanation
- stay grounded in source excerpts captured with `showboat exec`
- be tailored to the reader's level of familiarity with the language and stack
