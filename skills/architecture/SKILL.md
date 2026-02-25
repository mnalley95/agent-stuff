---
name: architecture
description: This skill should be used when creating or updating an ARCHITECTURE.md file for a codebase. It applies the matklad approach to architecture documentation—producing a concise, stable, high-level guide that captures the mental map of a codebase for new contributors. Use when asked to "write an ARCHITECTURE.md", "document the architecture", "create architecture docs", or "update ARCHITECTURE.md".
---

# Architecture

## Overview

To write an ARCHITECTURE.md that gives new contributors a mental map of a codebase—not a
specification, API reference, or tutorial, but a high-level guide to the code's structure,
responsibilities, and invariants.

Read `references/guidance.md` for the full principles, structure guidelines, and examples before
proceeding.

## Workflow

### Step 1: Explore the codebase

Before writing, orient yourself in the codebase:

- Identify the top-level directory structure: major directories, packages, crates, or modules
- Find entry points: main files, CLI handlers, HTTP routers, index files
- Understand what the project does at a high level (README, existing docs, package manifests)
- Identify key types, interfaces, or abstractions that span the codebase
- Note any existing ARCHITECTURE.md or similar docs to update rather than replace

Use `find`, `ls`, and file reads as needed. The goal is enough orientation to write accurately,
not exhaustive coverage.

### Step 2: Check for an existing ARCHITECTURE.md

If `ARCHITECTURE.md` already exists:
- Read it in full
- Identify which sections are stale, missing, or inaccurate
- Update rather than rewrite where possible—preserve accurate content

### Step 3: Write the document

Structure the document in three sections:

**Bird's Eye View**
One to three paragraphs covering:
- What the project does and what problem it solves
- The high-level input/output contract or data flow
- Any non-obvious architectural properties (incremental computation, event-driven, layered, etc.)

**Code Map**
A coarse-grained tour of major components. For each entry (module, crate, package, directory):
- 1–3 sentences on what it does and why it exists
- Any critical invariants or constraints (especially things it does *not* do)
- Relationships to other components if non-obvious

Skip self-explanatory components. Prioritize entries that would confuse a new contributor
without guidance.

**Cross-Cutting Concerns**
Short paragraphs on patterns that span multiple components:
- Error handling strategy
- Testing approach and conventions
- Concurrency or async model
- Code generation or build automation
- Observability (logging, tracing, metrics)
- Any other patterns a contributor must understand before touching the codebase

### Step 4: Apply quality checks

Before finalizing, verify:

- [ ] No hyperlinks to specific files or line numbers (use names only—readers can grep)
- [ ] No implementation details better suited to inline comments
- [ ] Architectural invariants are explicit, including stated absences ("X does not depend on Y")
- [ ] Each code map entry is 1–3 sentences; nothing longer belongs here
- [ ] The document is concise enough that every contributor will read it in full
- [ ] Content reflects stable structure, not current sprint state

## Output

Write the document to `ARCHITECTURE.md` in the project root (or the path indicated by the user).
If updating an existing file, make targeted edits rather than a full rewrite unless the existing
content is substantially wrong.
