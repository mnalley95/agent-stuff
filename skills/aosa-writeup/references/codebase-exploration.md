# Codebase Exploration Guide for AOSA Writeups

Before writing, thoroughly explore the codebase to answer specific questions. This guide tells you what to look for and how to find it.

---

## Phase 1: Orient (10–15 minutes)

**Goal:** Understand the shape of the codebase before reading any code.

### For a local codebase
```bash
# Directory structure
find . -maxdepth 3 -type d | sort

# File count and language breakdown
find . -name "*.go" -o -name "*.ts" -o -name "*.py" -o -name "*.rb" | wc -l

# Entry points
ls -la cmd/ bin/ main.* index.* app.*

# Package/module files
cat go.mod package.json Cargo.toml pyproject.toml Gemfile

# README
cat README.md
```

### For a GitHub repo
```bash
# Overview (stars, description, language, topics)
gh repo view owner/repo

# Full file tree
gh api repos/owner/repo/git/trees/HEAD?recursive=1 | jq '.tree[].path' | head -150

# README
gh api repos/owner/repo/readme | jq -r '.content' | base64 -d

# Dependency/module files
gh api repos/owner/repo/contents/go.mod | jq -r '.content' | base64 -d
gh api repos/owner/repo/contents/package.json | jq -r '.content' | base64 -d | jq '{dependencies,devDependencies}'
gh api repos/owner/repo/contents/pyproject.toml | jq -r '.content' | base64 -d
```

### What to record
- Language(s) and approximate line count
- Monorepo or single package?
- Top-level directory names and what they contain
- Key dependencies (these often reveal architectural choices)
- The README's own description of the software

---

## Phase 2: Find the Guiding Metaphor

**Goal:** Identify the one idea that explains *why* the software is structured the way it is.

This rarely appears as a comment in the code. Look for it in:
- README or docs
- Top-level module/package names (e.g., "beads", "molecules", "formulas" → the necklace metaphor)
- The names of core data types (e.g., `Issue`, `Dependency`, `Storage`)
- Commit messages explaining major architectural decisions
- Issue/PR discussions if accessible

```bash
# For a GitHub repo: scan commits and issues for architectural intent
gh api repos/owner/repo/commits?per_page=30 | jq '.[].commit.message'
gh api "repos/owner/repo/issues?state=all&per_page=20&labels=architecture,design" | jq '.[].title'
```

Ask yourself: "What is the simplest analogy that makes all the design choices feel inevitable?"

---

## Phase 3: Map the Architecture

**Goal:** Understand the layers and how they connect.

### Find the core abstractions

For a local codebase:
```bash
# Interface definitions (Go)
grep -r "type.*interface" --include="*.go" -l

# Core types/structs
grep -r "type.*struct" --include="*.go" | grep -v "_test" | head -40

# Key classes (TypeScript/Python)
grep -r "^class\|^export class\|^export interface" --include="*.ts" -l
```

For a GitHub repo (use GitHub code search API):
```bash
# Find interfaces/abstractions by pattern
gh api "search/code?q=type+interface+repo:owner/repo+language:go" | jq '[.items[] | .path]'
gh api "search/code?q=export+interface+repo:owner/repo+language:typescript" | jq '[.items[] | .path]'

# Read a specific file
gh api repos/owner/repo/contents/path/to/file.go | jq -r '.content' | base64 -d

# List a directory
gh api repos/owner/repo/contents/internal | jq '.[].name'
```

### Find the entry point and trace the call chain
Start at `main()`, `cmd/`, or the primary CLI entry point. Follow the call chain 3–4 levels deep. This reveals the layers.

For a GitHub repo, find and read the entry point:
```bash
gh api repos/owner/repo/contents/cmd | jq '.[].name'
gh api repos/owner/repo/contents/main.go | jq -r '.content' | base64 -d
```

### Find the storage layer
How is data persisted? File, SQLite, Postgres, custom format? The storage choice is often the most architecturally significant decision.

### Find the abstraction boundaries
Look for interfaces, protocols, or abstract base classes. These are the seams — they tell you where the designer drew the lines.

---

## Phase 4: Identify the Interesting Design Decisions

**Goal:** Find the 3–5 decisions that are distinctive, non-obvious, and illustrative.

Ask for each major component: "What would a naive implementation do here, and why didn't they do that?"

### Signals that a decision is interesting:
- A custom data structure when a standard one would seem to work
- An unusual dependency or library choice
- A protocol or file format designed from scratch
- A trade-off explicitly called out in comments or docs
- Something that surprised you when you first saw it
- A solution to a distributed systems problem (coordination, identity, conflict resolution)
- A place where performance, correctness, and simplicity were in tension

### Questions to answer for each decision:
1. What is the naive/simpler alternative?
2. Why does the naive alternative fail in this context?
3. What does the actual solution do instead?
4. What are the trade-offs of the chosen approach?
5. What broader principle does this illustrate?

---

## Phase 5: Read Key Code

For each interesting design decision, read the relevant code in depth.

### Useful patterns to look for

For a local codebase:

**Registry pattern** (extensibility):
```bash
grep -r "register\|Register" --include="*.go" -l
```

**State machine** (complex flows):
```bash
grep -r "state\|State\|enum.*state" -l
```

**Interface/abstraction** (layer boundaries):
```bash
grep -r "interface\|Interface\|Protocol\|Abstract" -l
```

**Pipeline/chain** (data transformation):
```bash
grep -r "pipeline\|Pipeline\|middleware\|Middleware" -l
```

**Event system** (decoupling):
```bash
grep -r "emit\|subscribe\|Event\|Handler" -l
```

For a GitHub repo (GitHub code search, then fetch the file):
```bash
# Search for a pattern across the repo
gh api "search/code?q=Register+repo:owner/repo" | jq '[.items[] | .path]'
gh api "search/code?q=middleware+repo:owner/repo" | jq '[.items[] | .path]'

# Read a file once you've identified it
gh api repos/owner/repo/contents/path/to/file.go | jq -r '.content' | base64 -d
```

Note: GitHub code search has rate limits (~10 req/min unauthenticated). Batch searches — identify all files of interest first, then fetch them. Cloning is faster if you need to search many patterns:
```bash
gh repo clone owner/repo /tmp/owner-repo
```

### When reading code for the writeup:
- Look for the **simplest representative snippet** (20 lines or less) that illustrates the concept
- Note any `// Simplified` opportunities — cases where the real code has more methods/fields than you need to show
- Look for data structure definitions (they reveal the designer's mental model)
- Look for the "happy path" flow, not edge cases

---

## Phase 6: Find the Diagrams in the Code

AOSA chapters always include diagrams. Find the material for them by:

**For component diagrams:** Map `import`/`require`/`use` statements between packages to understand dependency direction.

**For sequence diagrams:** Trace a key operation (e.g., "what happens when a user runs the main command") through the call stack. The sequence diagram shows the call chain between components.

**For state machines:** Find the states and transitions. Often in an enum + switch statement.

**For data flow:** Find where data enters the system, how it transforms, and where it exits.

---

## Phase 7: Find the Lessons

**Goal:** Identify the generalizable insights.

For each interesting design decision, ask: "What would someone building a different system learn from this?"

Good lessons:
- "X beats Y" (comparative: "Hash IDs with math beat auto-increment")
- "X is a feature, not a bug" (reframing)
- "[Property] that don't leak" (structural)
- "X-first beats Y-first" (priority/philosophy)

---

## Checklist Before Writing

- [ ] Know the approximate line count and languages
- [ ] Know the top-level structure (monorepo? single package?)
- [ ] Have a one-sentence guiding metaphor
- [ ] Have a component table (all major components, one-sentence description each)
- [ ] Have 3–5 interesting design decisions, each with: problem / solution / code snippet / diagram material / trade-off
- [ ] Have 2–5 supporting components to mention briefly
- [ ] Have 3–6 lessons
- [ ] Have the repo URL for the conclusion

---

## Common Pitfalls

**Documenting features instead of decisions.** "The CLI has 80 commands" is a feature. "The CLI uses Cobra, which lets each command declare its own flags without touching the main binary" is a decision.

**Describing what without why.** Every section should answer: "Why is it shaped this way?"

**Picking too many interesting decisions.** 5 decisions covered deeply is better than 10 covered shallowly. Be selective.

**Making up trade-offs.** Only state trade-offs you can ground in the code. "Dolt is a large dependency — it pulls in hundreds of packages including a full MySQL server implementation" is specific. "This approach has some downsides" is not.

**Using passive voice for the interesting parts.** "Data is stored in Dolt" is weaker than "Beads stores everything in Dolt." Active voice, named subject.
