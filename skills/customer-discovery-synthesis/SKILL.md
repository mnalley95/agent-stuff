---
name: customer-discovery-synthesis
description: Synthesize customer discovery session notes against the discovery playbook, hypothesis/question table, transcript evidence, and broader product strategy docs. Use when Codex needs to update a customer discovery session file with hypothesis answers/status, off-table questions, latent/burning need options, or "previously dismissed" product ideas revived by an interview.
---

# Customer Discovery Synthesis

## Overview

Use this skill to turn a raw customer discovery transcript into structured product learning. Preserve the transcript, add synthesis above it, and ground conclusions in the canonical discovery playbook plus broader product docs.

Default to skepticism. The goal is not to validate the product thesis; it is to separate directly evidenced facts, plausible implications, founder inferences, unsupported willingness-to-pay claims, and contradictions against current docs.

## Source Order

Read only what is needed, but prefer this order:

1. Target session file, usually `docs/product/customer_discovery/sessions/*.md`.
2. Discovery playbook: `docs/product/customer_discovery/customer_discovery_development_playbook.md`.
3. Canonical product docs when judging latent needs or prior dismissals:
   - `docs/product_prd.md`
   - `docs/product_vision.md`
   - `docs/product/programs/commercial_model_and_packaging.md`
   - `docs/product/programs/pricing_discussion_notes.md`
   - `docs/product/programs/competitive_intelligence.md`
   - relevant scenario or capability docs, especially `docs/product/scenarios/shortage_signal_to_receipt.md`

If a referenced path has moved, locate it with `rg --files`.

## Workflow

1. Locate the transcript and preserve it unchanged.
2. Add a `### Summary` section at the top of the output file, above the hypothesis table and above `## Transcript`.
3. Copy the `Working Hypotheses And Interview Questions` table from the playbook into the session file above `## Transcript` if it is not already present.
4. Add a respondent-specific answer column named after the participant, for example `Ryan answer`.
5. Add `Hypothesis status` as a separate column. Do not put status words in the answer column.
6. Fill the answer column with concise evidence from the meeting. Use `Did not Address` when the meeting did not address the row.
7. Fill `Hypothesis status` with one of:
   - `Confirmed`
   - `Partially confirmed`
   - `Contradicted`
   - `Partially contradicted`
   - `Not addressed`
8. Add `## Other Questions` below the hypothesis table when the interview included questions that do not neatly fit the table. Use a two-column table: `Question` and `<Participant> answer`.
9. Add `## Critical Evaluation`. Include a table that challenges the synthesis, current wedge, evidence strength, buyer/economic assumptions, implementation burden, and contradictions against broader docs.
10. Add `## Hypotheses To Test Next`. Turn the session into an iterative discovery backlog: what changed, why, what evidence is needed next, and who/artifacts should be targeted.
11. Add `## Latent Burning Need Options`. Include several options and explicitly compare them to broader product docs.
12. When the call may change wedge, ICP, pricing, buyer, roadmap, or positioning, use independent subagent critique passes before finalizing:
    - Evidence quality: overclaims, weak statuses, unsupported pay claims.
    - Product-doc consistency: contradictions with PRD, vision, commercial model, scenario docs.
    - GTM/economics: buyer, budget, ROI, pricing, willingness to pay.
13. Verify Markdown table shape before finalizing.

## Summary Format

At the top of the output file, create meeting notes under a `### Summary` heading. If the file does not already have a top-level title, title it `# Meeting Notes` or `# Meeting Notes - <date>` when the date is known.

Format the summary as follows:

- Organize content into logical `##` sections based on topics discussed, such as `## Customer Discovery`, `## Technical Decisions`, or `## Pricing Strategy`.
- Use `###` subsections when a topic has distinct sub-parts.
- Write in second person, using `you`, when referring to the primary user or recorder.
- Use bullet points for all content. Do not write paragraphs.
- Bold key terms, names, numbers, and decisions.
- Include specific numbers, dollar amounts, dates, and percentages when mentioned.
- End with an `## Action Items` section, split by person when multiple participants exist.
- Include a `## Next Steps` section when there are clear next steps or a follow-up meeting.
- Include a `Key quotes:` line at the end when there are notable verbatim quotes worth preserving.
- Be comprehensive. Capture all substantive topics discussed, not just highlights.
- Preserve the natural flow and order of the conversation.
- Use em dashes for parenthetical context.
- Bold names and entities on first reference.
- When the speaker references specific tools, companies, or frameworks, name them explicitly.

## Evidence Rules

- Treat the answer column as interview evidence, not a verdict. Put the verdict only in `Hypothesis status`.
- Prefer concrete facts: volumes, systems, teams, existing workarounds, pricing anchors, events that got management attention, and budget/headcount comments.
- Mark partial coverage honestly. Do not stretch an answer to cover an entire hypothesis.
- For `Did not Address`, use that exact casing in respondent-answer cells. For status cells, use `Not addressed`.
- Preserve ambiguity where the meeting gives mixed evidence.
- Treat enthusiasm, future opinions, "that would be valuable," generic AI interest, and broad pain as weak evidence. They are not willingness-to-pay evidence unless tied to current spend, budget owner, approval path, price reaction, or prior purchase behavior.

## Hypothesis Status Rubric

- Use `Confirmed` only when the participant directly addressed the main claim and most critical subclaims.
- Use `Partially confirmed` when the participant validates one part of the hypothesis but leaves important parts untested.
- Use `Contradicted` or `Partially contradicted` when the participant's evidence points against the claim, even if they are generally positive about the problem area.
- Use `Not addressed` when the row depends on budget, pricing, approval path, workflow ownership, systems, volume, metrics, current spend, or implementation path that were not directly discussed.
- Do not confirm willingness-to-pay, buyer, pricing, or pilot hypotheses from enthusiasm, future opinion, or generic statements that something sounds useful.

## Critical Evaluation

Add a critical evaluation table before hypotheses to test next:

```markdown
| Challenge | Why it matters | Product implication |
|---|---|---|
```

Include the strongest challenges from the meeting, especially:

- evidence against the current wedge or first workflow
- where participant language differs from docs' language
- where the synthesis may be overfitting one anecdote
- where enthusiasm is not willingness to pay
- where the buyer, user, budget owner, and economic owner may differ
- where implementation burden, data quality, trust, or security may erase value
- where current roadmap assumptions are being defended because they are already documented

## Hypotheses To Test Next

Add a forward-looking table after critical evaluation:

```markdown
| Updated hypothesis or change to test | Why this changed | Evidence needed next | Best next respondent / artifact |
|---|---|---|---|
```

Use this section to make customer discovery iterative over time. Include:

- hypotheses to revise, narrow, split, or retire
- wedge, ICP, buyer, pricing, adoption, trust, implementation, or message changes to test
- concrete evidence that would confirm or falsify the change
- next personas, companies, artifacts, reports, dashboards, trackers, budgets, or approval paths to inspect

Avoid vague next steps like "talk to more customers." Each row should name the next evidence that would change the team's mind.

## Latent Need Synthesis

Read the transcript to determine whether the interview revealed some implicit or latent problem / burning need that customers would pay for. Give several options.

In addition, include anything that may have been previously dismissed. Judge the dismissal by the broader docs in the repo, paying attention to something already deprioritized, framed as later, or treated skeptically. Use subagents to explore the repo.

Use a table like:

```markdown
| Option | Latent problem / burning need | Interview evidence | Product-doc implication | Problem intensity | Pay evidence |
|---|---|---|---|---|---|
```

Rank options by strength from the meeting, not by current roadmap fit. Include:

- the strongest current-wedge interpretation
- adjacent wedges revived by the participant
- ideas the docs currently push later or caution against
- ideas weakened by the meeting

For `Pay evidence`, distinguish:

- `Direct`: price reaction, budget owner, current spend, approval path, paid pilot threshold, or prior purchase for the same problem.
- `Indirect`: prior tool, consulting, headcount, expedite, premium freight, shortage, or process-improvement spend; executive metric ownership.
- `Missing`: no current buyer, budget, approval, price, spend, or purchase evidence.

Do not recommend broad horizontal repositioning just because the primitive generalizes. State when evidence validates reusable primitives but does not justify changing near-term positioning.

## Red Flag Checks

Before finalizing, scan for these failure modes and adjust the document when present:

- `Confirmed` status based on adjacent or partial evidence.
- Pilot, pricing, or willingness-to-pay claims without budget or approval-path evidence.
- Generic AI interest treated as demand for this workflow.
- Labor savings treated as realized ROI without buyer confirmation.
- PO count, task count, or activity volume treated as business value.
- Participant language pointing to a different wedge than the docs.
- Current roadmap defended because it is already documented.
- Broad platform implications drawn from one workflow anecdote.
- Problem intensity mislabeled as pay likelihood.

## Useful Checks

Run these after editing tables:

```bash
awk '/^\| H/{n=gsub(/\|/,"&"); if(n!=8){print NR, n, $0}}' <session-file>
```

For `Other Questions`, expect three pipe characters per row:

```bash
awk '/^\| [^|-]/{if ($0 !~ /^\| H/ && $0 !~ /^\| ID/ && $0 !~ /^\| Question/){n=gsub(/\|/,"&"); if(n!=3){print NR, n, $0}}}' <session-file>
```

For `Critical Evaluation`, expect four pipe characters per row:

```bash
awk 'BEGIN{s=0} /^## Critical Evaluation/{s=1; next} /^## Hypotheses To Test Next/{s=0} s && /^\|/{n=gsub(/\|/,"&"); if(n!=4){print NR, n, $0}}' <session-file>
```

For `Hypotheses To Test Next`, expect five pipe characters per row:

```bash
awk 'BEGIN{s=0} /^## Hypotheses To Test Next/{s=1; next} /^## Latent Burning Need Options/{s=0} s && /^\|/{n=gsub(/\|/,"&"); if(n!=5){print NR, n, $0}}' <session-file>
```

For `Latent Burning Need Options`, expect seven pipe characters per row:

```bash
awk 'BEGIN{s=0} /^## Latent Burning Need Options/{s=1; next} /^## Transcript/{s=0} s && /^\|/{n=gsub(/\|/,"&"); if(n!=7){print NR, n, $0}}' <session-file>
```

Adjust the expected pipe count if the table schema changes.
