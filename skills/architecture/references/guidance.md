# ARCHITECTURE.md Guidance

Source: https://matklad.github.io/2021/02/06/ARCHITECTURE.md.html
Example: https://github.com/rust-analyzer/rust-analyzer/blob/d7c99931d05e3723d878bea5dc26766791fa4e69/docs/dev/architecture.md

## Purpose

An ARCHITECTURE.md is a high-level guide for new contributors to a codebase. It captures the
mental map that maintainers carry in their heads—the kind of knowledge you'd share over a
30-minute call with a new team member. It is not a specification, not API documentation, and not
a tutorial. It is a map.

Target audience: occasional contributors (not core maintainers, not total newcomers). Sized for
projects with roughly 10k–200k lines of code, but the principles scale.

## Core Principles

**Brevity over completeness.** Every recurring contributor will read this document. Keep it short
enough that it stays accurate and maintainable. Aim to review it a few times per year, not
continuously. If something changes frequently, don't document it here.

**Stability over recency.** Only specify things that are unlikely to change often. Architectural
invariants, layer boundaries, and major module responsibilities are good candidates. Implementation
details, function signatures, and current behavior are not.

**Absence is information.** Explicitly state architectural invariants, especially those expressed
as things that are *not* present or *not* allowed. "This component does not depend on X" is as
valuable as "this component does Y."

**No hyperlinks.** Links to specific files, line numbers, or even functions go stale quickly.
Name important files and types directly in prose—readers can use symbol search to find them.

## Document Structure

### 1. Bird's Eye View

One to three paragraphs. Answer: what does this project do, at the highest level? Describe the
input/output contract of the system, the main problem it solves, and its key non-obvious properties
(e.g., incremental computation, event-driven, purely functional core).

### 2. Code Map

A coarse-grained tour of the important directories, crates, packages, or modules. For each entry:

- One or two sentences on what it does and why it exists
- Note any critical design decisions or invariants
- Mention relationships to other entries if non-obvious

Think of this as a country-level map, not a street-level atlas. The reader should be able to
orient themselves in the codebase after reading it, not understand every street.

Good candidates to include:
- Entry points (main, CLI handlers, HTTP routers)
- Core domain logic
- Persistence/storage layer
- External integrations
- Build or tooling infrastructure that contributors will touch
- Any component with a non-obvious or surprising design

Skip components that are self-explanatory from their name and location.

### 3. Cross-Cutting Concerns

Architectural patterns and decisions that span multiple modules. These are the things a contributor
needs to know before touching any part of the codebase. Common topics:

- Error handling strategy
- Testing approach and conventions
- Code generation or build-time automation
- Cancellation / concurrency model
- Observability (logging, tracing, metrics)
- Data flow patterns
- Performance-critical paths or known constraints

Each concern gets a short paragraph—enough to know what the pattern is and where to look for examples.

## What to Include vs. Exclude

| Include | Exclude |
|---------|---------|
| Module/crate/package responsibilities | Per-function documentation |
| Architectural invariants | Implementation details |
| Layer boundaries and dependencies | Frequently-changing specs |
| Key types and their roles | Hyperlinks to specific files |
| Non-obvious design decisions | Tutorial walkthroughs |
| Cross-cutting concerns | Comprehensive changelogs |

## Example: rust-analyzer

The rust-analyzer ARCHITECTURE.md is a canonical example. Its structure:

1. **Bird's Eye View** — explains the input/output model (source → semantic model) and the
   incremental computation strategy (lazy, demand-driven via Salsa)

2. **Code Map** — ~18 entries covering each major crate. Each entry is 2–5 sentences. Invariants
   are stated explicitly (e.g., "the parser is independent from the tree structure," "syntax trees
   are value types, never modified in place").

3. **Cross-Cutting Concerns** — covers code generation, cancellation, testing, error handling,
   and observability. Each is a focused paragraph with enough detail to orient a new contributor.

Key observations from rust-analyzer:
- Module entries often lead with what the module is *not* or what constraints it operates under
- The document is ~600 lines but reads quickly; density comes from precision, not prose
- Testing strategy is explicitly documented (three tiers: LSP integration → IDE boundary → unit)
- The document names types and files by their exact names so contributors can grep for them
