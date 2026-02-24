---
name: aosa-writeup
description: Generates long-form architectural writeups in the style of "The Architecture of Open Source Applications" (AOSA) and the Latent Patterns architecture series. Use this skill when asked to write an architectural writeup, architecture chapter, AOSA-style article, or technical deep-dive about a codebase or GitHub repository. Triggers on requests like "write an architectural writeup of this repo", "create an AOSA-style chapter about X", "analyze this codebase and write it up", or "write a technical article about how X is architected".
allowed-tools: Glob, Grep, Read, Bash, Task
---

# AOSA Writeup

## Overview

This skill produces architectural writeups modeled on *The Architecture of Open Source Applications* book series and the Latent Patterns architecture article series. The goal is not to document every function or command — it is to show the reader *why* the software is shaped the way it is: the key design decisions, the trade-offs, and the lessons those decisions teach.

## Workflow

Follow these two phases in order.

### Phase 1: Explore the Codebase

Before writing a single word, thoroughly explore the codebase. Load `references/codebase-exploration.md` and follow the 7-phase exploration process:

1. **Orient** — understand the shape and size of the codebase
2. **Find the guiding metaphor** — the one idea that explains all the design choices
3. **Map the architecture** — layers, core abstractions, storage, boundaries
4. **Identify interesting design decisions** — the 3–5 non-obvious choices that make this codebase distinctive
5. **Read key code** — go deep on the relevant code for each decision
6. **Find the diagrams** — gather material for component, sequence, and flow diagrams
7. **Find the lessons** — generalizable insights a reader could apply elsewhere

**Parallelizing phase 5 with subagents**: Once phases 1–4 are complete and the 3–5 design decisions are identified, dispatch one `general-purpose` subagent per decision using the Task tool. Each subagent receives the decision name, the relevant file paths or module areas already identified, and instructions to: read the code deeply, extract a simplified illustrative snippet (2–25 lines with `// Simplified:` comment), identify the trade-off, and return its findings as text. Collect all subagent results before proceeding to phases 6–7. Phases 1–4 and 6–7 are done by the orchestrating agent directly.

Do not start writing until the pre-writing checklist in `references/codebase-exploration.md` is complete.

### Phase 2: Write the Article

Load `references/style-guide.md` and follow the structure and voice conventions exactly.

The article structure is:

1. **Opening scenario** (~150 words) — second-person scenario that makes the reader feel the problem
2. **Introduction** (~150 words) — software name, language, size, guiding metaphor, roadmap sentence
3. **The big picture** (~300 words) — ASCII component diagram + Component/Job table + transition sentence
4. **Core sections × 3–5** (~600–1200 words each) — one per interesting design decision, each with:
   - "The problem" subsection (naive approach and why it fails)
   - "The solution: [name]" subsection (prose → code snippet → diagram → analogy → trade-off)
5. **Supporting components** (~300–600 words total) — 2–5 additional components, briefly
6. **Lessons learned** (~400–600 words) — 3–6 bold-lead lessons that generalize beyond this codebase
7. **Conclusion** (~100 words) — restate metaphor, name architectural pillars, link to repo

## Key Conventions

- **Voice:** Present tense, direct, declarative. No hedging, no filler phrases.
- **Analogies:** Every solution needs at least one. "Think of it like..."
- **Code snippets:** Always simplified. Include `// Simplified:` comment. 2–25 lines.
- **Diagrams:** ASCII box-and-arrow for components, sequence format for flows.
- **Trade-offs:** Every core section must end with an honest acknowledgment of the downside.
- **Lessons:** Bold lead sentence, imperative or declarative, starts with the key noun.

## Resources

- `references/style-guide.md` — complete style, structure, voice, diagram, and code conventions
- `references/codebase-exploration.md` — 7-phase exploration guide with bash commands and a pre-writing checklist
