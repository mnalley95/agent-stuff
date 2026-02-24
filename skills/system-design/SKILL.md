---
name: system-design
description: "System design best practices and architectural patterns. Use when answering system design questions, designing or reviewing distributed systems, explaining architectural trade-offs, or discussing concurrency models including the actor model, OTP supervision trees, and fault-tolerant design. Covers networking fundamentals, databases, caching, scalability, microservices, messaging, API design, observability, and security patterns."
---

# System Design Best Practices

To answer system design questions, look up the relevant topic in `references/resources.md`
and fetch the corresponding URL. Present design principles and trade-offs; do not get
sidetracked by language-specific syntax (especially Erlang/Scala/Java when using LYSE or
Akka docs—focus on the underlying design concepts).

## Sources

Four authoritative sources cover different areas:

| Source | Best for |
|--------|----------|
| **karanpratapsingh/system-design** | Breadth: networking, databases, scalability, architecture patterns, real-world case studies |
| **Learn You Some Erlang (LYSE)** | Concurrency concepts, actor model internals, process isolation, message passing, fault tolerance design |
| **Erlang OTP Design Principles** | Supervision trees, behaviours (gen_server / gen_statem / gen_event), application structure |
| **Akka Typed docs** | Actor model applied to JVM systems, interaction patterns, cluster, persistence/event sourcing |

## How to Look Up a Topic

1. Check `references/resources.md` for the topic and its URL.
2. Fetch the URL with a focused prompt, e.g. "Explain the design principles and trade-offs for load balancing."
3. Synthesize the fetched content into a clear, source-language-agnostic answer.
4. When a question spans multiple topics (e.g. "design a rate-limited API gateway"), fetch
   each relevant section and integrate the answers.

## Design Principle Guidelines

- Always explain **trade-offs**, not just definitions.
- Prefer **concrete examples** (e.g. "a Redis sorted set for rate limiting") over abstract descriptions.
- When discussing actors/OTP: focus on **isolation**, **message passing**, **supervision strategies**,
  and **let it crash** philosophy—not Erlang/Scala syntax.
- For distributed systems topics: always address **consistency vs availability** (CAP/PACELC)
  and **failure modes** explicitly.
