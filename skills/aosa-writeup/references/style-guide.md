# AOSA-Style Architectural Writeup: Style Guide

This guide documents the precise style, structure, and voice used in *The Architecture of Open Source Applications* (AOSA) and the Latent Patterns "architecture chapter" series. Study these patterns before writing.

## The Goal

The goal of every writeup is captured in one sentence, which should appear near the top of the article:

> "The goal is not to document every [command/function/class] — it is to show you *why* the pieces are shaped the way they are."

This is the north star. Every section must answer "why is it shaped this way?" not "what does it do?"

---

## Overall Structure

```
1. Opening scenario        (~150 words)
2. Introduction            (~150 words)
3. The big picture         (~300 words, with diagram + table)
4. Core sections × 3–5    (~600–1200 words each)
5. Supporting components   (~300–600 words total)
6. Lessons learned         (~400–600 words)
7. Conclusion              (~100 words)
```

Total length: 4,000–12,000 words. Aim for the architecture to determine the length, not a target word count.

---

## Section 1: Opening Scenario

Open with a concrete, second-person scenario that puts the reader *inside* the experience of the problem the software solves. Not what the software is — what it feels like to need it.

**Pattern:**
```
Imagine you [specific action]. [What happens]. [What the reader observes].
[One day later / next step]. [The problem surfaces]. [Why this matters].
This is the [name of the problem].
```

**Examples from the wild:**
- "Imagine you ask an AI coding agent to build a feature. It creates three files, writes tests, and reports success. The next day you start a new session and ask it to fix a bug..."
- "Imagine you open a terminal and type: `pi "there is a bug in the login page — fix it"`. A few seconds later, text starts streaming onto your screen..."

The scenario should make the reader feel the pain *before* introducing the solution.

---

## Section 2: Introduction

Brief paragraph introducing the software by name. Cover:
- What it is (one sentence)
- Who built it / what it does
- Implementation language and rough size ("roughly 105,000 lines of production code")
- How to install/use it (one-liner command if applicable)
- The name's meaning if it has one ("The name is literal: issues chained together like beads on a string")

Then: **the guiding metaphor** — one sentence that encapsulates the entire design philosophy. This metaphor will recur throughout the article.

Examples:
- "Pi follows one guiding idea: **adapt the tool to your workflow, not the other way around.**"
- "Beads follows one guiding metaphor: **issues chained together like beads on a string.**"

Then: the roadmap sentence. "This article walks through the architecture of [X]. We will start with the big picture, then [zoom/dive] into [N] [layers/design decisions], and close with the lessons the design teaches us."

---

## Section 3: The Big Picture

Subsection heading: **"The big picture"**

1. Restate the guiding metaphor/principle in one sentence.
2. Describe the high-level structure in one sentence.
3. ASCII box-and-arrow diagram showing all major components and their relationships.
4. A two-column table: **Component | Job** — one sentence per component.
5. Transition sentence: "The rest of this article goes through [the N core layers / the four core design decisions]: [list them by name]."

**Diagram style:** Use plain ASCII box diagrams. Each box contains the component name and a parenthetical descriptor. Arrows show direction of dependency/flow.

```
ComponentA              ComponentB
(descriptor)            (descriptor)
     |                       |
     v                       v
     ComponentC (descriptor)
```

---

## Core Sections (3–5 sections)

These are the heart of the article. Choose 3–5 design decisions that are:
- **Distinctive** — something that makes this codebase different from a naive implementation
- **Non-obvious** — the *why* is not immediately apparent from reading the code
- **Illustrative** — they teach a broader lesson

Each core section has this structure:

### Section heading
Use a noun phrase that names the component or concept (not a gerund). Examples:
- "The version-controlled database"
- "The LLM layer: pi-ai"
- "Collision-free IDs"
- "The dependency graph"

### The problem
Subsection heading: **"The problem"**

Explain what a naive or simpler approach would be, why it fails in this context, and why the failure mode matters. Be specific. Name the alternatives and their failure modes.

Pattern: "Most [X] use [naive approach]. That works fine when [simple case]. But [this software's context] creates a different situation. [Why naive approach fails]. [What you need instead]."

### The solution: [name]
Subsection heading: **"The solution: [short name]"**

Explain the approach in prose first. Then show code. Then explain the code. Then show a diagram if there's a flow to illustrate.

**Code snippets:** Always simplified. Include a comment: `// Simplified: the real [function/interface] has [more X]`. Show just enough to make the concept concrete. 2–20 lines.

**Diagrams:** Use ASCII box-and-arrow for component diagrams. Use sequence diagrams for flows (Participant → action → Participant format).

**Analogies:** Every solution should have at least one analogy. The analogy makes it stick:
- "Think of Dolt as **git for your SQL tables**."
- "Think of it like a **universal power adapter**."
- "Think of it like **React, but for your terminal**."
- "Think of it like **license plates**. A small town can use three-letter plates. A growing city needs longer ones."

**Trade-offs:** Every solution section must end with an honest acknowledgment of trade-offs. Pattern: "The trade-off [of / is] [honest downside]. [Why the upside is worth it]."

### Subsections within core sections
Use tertiary headings for sub-topics within a core section. Examples:
- "Cross-provider handoffs"
- "The edge cases nobody warns you about"
- "Cycle detection"
- "The *bd ready* algorithm"

These deepen the treatment without adding a full problem/solution structure.

---

## Supporting Components Section

Heading: **"Supporting components"**

Cover 2–5 additional components that matter but don't each warrant a full problem/solution treatment. Use tertiary headings for each. 1–4 paragraphs per component. Focus on the one or two interesting design choices, not a complete description.

---

## Lessons Learned Section

Heading: **"Lessons learned"**

3–6 lessons. Each lesson:
1. **Bold lead sentence** — the lesson in one sentence, imperative or declarative. Start with the key noun, not "I" or "We". Examples:
   - "**Version-controlled data beats version-controlled files.**"
   - "**Layer boundaries that do not leak.**"
   - "**Hash IDs with math beat auto-increment.**"
   - "**Forgetting is a feature, not a bug.**"
2. 2–4 paragraph sentences explaining *why* this lesson is true, illustrated from the codebase.

Lessons should generalize beyond this specific software. A reader should be able to apply them to their own projects.

---

## Conclusion

2–3 short paragraphs. Structure:
1. Restate the guiding metaphor and what the software demonstrates at a high level.
2. Name the 2–4 architectural pillars and what failure mode each one addresses.
3. "If you want to explore the code, the repository is at [repo]. [Community location if any]."

---

## Voice and Tone

**Present tense throughout.** "The storage layer *does* X", not "the storage layer *did* X" or "the storage layer *will* X".

**Direct, declarative sentences.** No hedging. "This is fast enough." Not "This may be fast enough in most cases."

**Imperative/instructional language in explanations.** "Think of it like...", "Here is what happens when...", "Here is how the types break down:"

**Second person for scenarios and analogies only.** Use "you" when walking through the user's experience or explaining an analogy. Use third person for the software: "Beads solves this by...", "Pi-ai handles this with..."

**No filler phrases.** Never: "It's worth noting that", "Interestingly,", "As we can see,", "Let's look at". Just say the thing.

**Short paragraphs.** 2–5 sentences. Dense technical content should be broken up with code blocks or diagrams between paragraphs.

**Bold key terms** when first introduced in a section. Italic for inline code identifiers, method names, and technical terms used as terms.

**Honest about limits.** State trade-offs plainly: "The trade-off is real." "This is permanent data loss by design."

---

## Diagrams

### Component/Architecture diagrams
Use ASCII box-and-arrow. Layout: top-down or left-right depending on the data flow.

```
ComponentA
(descriptor)
     |
     v
ComponentB          ComponentC
(descriptor)        (descriptor)
     |                   |
     +--------+----------+
              |
              v
         ComponentD
         (descriptor)
```

### Sequence diagrams
Show labeled participants across the top, with messages as horizontal arrows between them. Vertical is time.

```
Client      Server      Database
  |            |            |
  |--request-->|            |
  |            |--query---->|
  |            |<--result---|
  |<--response-|            |
```

### Flow/Decision diagrams
Show decision points as diamond-ish nodes, processes as boxes, connected by labeled arrows.

```
[Input]
   |
   v
[Process A]---condition--->[ Process B ]
   |                             |
   v                             v
[Output X]               [Output Y]
```

---

## Tables

Two-column tables: **Component | Job** or **Attribute | Value**. Each Job is exactly one sentence. Use for:
- Listing all components at a glance (in "The big picture")
- Showing scaling/comparison data (e.g., ID length scaling table)

---

## Code Snippets

- Always include the language (for syntax highlighting where rendered)
- Add `// Simplified:` comment when abbreviated
- Show enough to make the concept concrete, not the full implementation
- Use meaningful variable names, not `x`, `y`, `foo`
- Comments in code should explain *why*, not *what* (the prose explains what)
- 2–25 lines per snippet; break longer ones into multiple snippets with prose between

---

## What to Avoid

- **Do not document every feature.** Pick the most architecturally interesting pieces.
- **Do not use "I" or "we" for the author's opinions** — state lessons as facts.
- **Do not use jargon without explanation.** Every technical term should be explained on first use or have an analogy.
- **Do not be exhaustive in the big picture table.** Only list the major components; detail comes in the core sections.
- **Do not end sections with a summary.** Each section should end on the most interesting insight or the trade-off — not a recap.
- **Do not inflate.** A 4,000-word article for a small codebase is fine. Don't pad to match AOSA's 12,000-word chapters if the subject doesn't warrant it.
