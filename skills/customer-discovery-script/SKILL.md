---
name: customer-discovery-script
description: Create tailored customer discovery prep scripts and interview guides for specific people, companies, and roles. Use when asked to create or revise customer discovery prep docs, interview scripts, design-partner discovery guides, role-specific discovery questions, or call prep for executives, operators, advisors, consultants, investors, systems/IT stakeholders, procurement, supply chain, operations, AP, manufacturing, logistics, or related personas.
---

# Customer Discovery Script

## Core Workflow

Create a Markdown prep file that matches the style of existing docs in `docs/product/customer_discovery/prep/` and uses `docs/product/customer_discovery/customer_discovery_development_playbook.md` as the discovery system when available.

Do not copy a generic script. Tailor the guide to the person's role, seniority, company context, and relationship to the founder.

1. Read the playbook and 1-2 nearby prep examples.
2. Read any named source docs or session notes.
3. Search local docs for the person, company, and related prior calls.
4. Use public web research only when local context is insufficient or the user asks for it.
5. Classify the primary persona and any secondary lens:
   - Executive / decision-maker
   - Operator / workflow owner
   - Advisor / consultant / investor / pattern observer
   - Systems / IT / implementation stakeholder
6. Create one practical 30-minute script using the canonical call spine below, then tailor emphasis, probes, and close to the persona.

## Discovery Rules

Follow Jason Cohen / Mom Test style discovery:

- Start broad enough for the person to name the pain, then double-click into a specific recent example.
- Extract workflow, actors, systems, current workaround, cost, ownership, prior attempts, and buying logic.
- After the example, step back to test whether it is a recurring pattern or one-off.
- Ask about actual past behavior, current spend, prior attempts, approval path, artifacts, and proof of value.
- Do not pitch too early.
- Do not ask "Would you use this?" or "Would you pay for this?" in the abstract.
- Keep the 30-minute path usable; put optional depth in time-permitting sections.

Use light Pyramid Principle:

- The opening should state the main learning objective plainly.
- Each major section should have one governing question.
- Priority questions should support that question.
- Follow-ups should be optional drills.
- Do not force every section into three MECE bullets if it makes the call unnatural.

## Standard Output Shape

Use this shape unless the surrounding prep docs clearly use a different convention:

```markdown
___
Title: Customer Discovery Interview
Date: M/D/YY
Person: [Name]
Goal: [Short goal line]
___

Public research notes

- ...

Script

## Priority -- 30-Minute Call

## 0. Intro -- 60 seconds max
...

## 1. Their World -- minutes 1-8
...

## 2. Last Real Example -- minutes 8-16
...

## 3. Prior Attempts -- minutes 16-20
...

## 4. Buying Conditions -- minutes 20-23
...

## 5. Optional Solution Share -- minutes 23-26
...

## 6. Close -- minutes 26-30
...

## Time Permitting
...

## If Time Is Running Short
...

## Post-Call Notes To Capture
...

Transcripts

Sources
...
```

Keep the Goal line short. Put nuance into research notes and section goals.

## Canonical Call Spine

The generated script should default to this flow. Do not create four different full scripts for four personas; create one call path and tune the wording, probes, and close.

### Goal

The goal should be concrete and behavior-focused:

- Learn their version of the problem, not whether they like the product hypothesis.
- Get one specific story, one dollar/time cost, one failed prior attempt, one referral or next step, and one buying condition grounded in past behavior.

### 0. Intro -- 60 Seconds Max

The intro should be short enough to leave the call for discovery:

- Left Apple, building in supply chain, doing research.
- Want to learn from their experience.
- Not pitching; genuinely trying to understand what breaks.
- Offer to share what you are working on near the end if they are curious.

### 1. Their World -- Minutes 1-8

Governing question: what recurring problem do they name before being anchored by the product hypothesis?

Start broad, then stay quiet:

- "What's the most frustrating recurring problem in your supply chain / procurement / operations world right now?"
- For a shorter recency frame: "What's the most frustrating thing that happened this week / month in your supply chain / procurement / operations world?"
- "Tell me more about that."
- "How often does that happen?"
- "Who deals with that when it breaks?"

Do not rush to categorize the pain. Let them talk uninterrupted for 2-3 minutes when they start naming pain. Count to 5 after silence. Do not react with the product thesis; use "tell me more about that" before introducing any interpretation.

### 2. Last Real Example -- Minutes 8-16

Governing question: what actually happened the last time this problem showed up?

Use this as the main depth section:

- "Walk me through the last time that happened. Start from when you first noticed."
- If they named a broad problem: "Walk me through the last time that actually cost money. What happened, who got involved, and how long did it take to fix?"
- Probe for who got pulled in, how long it took, what systems were involved, where work happened, what workaround was used, and what outcome resulted.
- Ask what it cost in time, money, missed commitment, relationship damage, escalation, premium freight, quality cost, working capital, or avoided / wasted labor.
- Step back: "Is that a one-off, or does some version of that happen every week / month / quarter?"
- If they cannot quantify: "If you added up all the times that happened last quarter, what would the number look like: labor hours, premium freight, penalties, late orders, missed commitments, or anything else?"

Stay in the example longer than feels natural. Ask 2-3 follow-ups before moving on, especially:

- "Why was that hard?"
- "What did you do next?"
- "What decision did this slow down or change?"
- "What would you have done differently if you had known the truth earlier?"

When they name a vague desired state such as "one version of truth," force it into action while still in the example:

- "If you had that tomorrow, what would you actually do differently?"
- "What decision would you make faster?"
- "What action would happen that does not happen today?"
- "Who would stop doing manual work?"

### 3. Prior Attempts -- Minutes 16-20

Governing question: what have they already done, bought, built, or worked around?

Ask about past behavior:

- "What have you already tried to fix this?"
- "Why did that not work?"
- "Did you evaluate any tools, vendors, internal projects, reports, dashboards, process changes, or headcount? What happened?"
- "Who internally cared enough to try fixing it?"
- "Is it actively owned, or is it accepted as the way work gets done?"

### 4. Buying Conditions -- Minutes 20-23

Governing question: when this class of problem gets funded, what has to be true?

Only ask fundability questions after vague pain has been converted into a concrete decision, action, cost, owner, or consequence.

Avoid abstract "would you buy" questions. Anchor on real approval behavior:

- "When problems like this actually get funded, what usually has to be true?"
- "What was the last similar tool, process improvement, headcount ask, or automation that got approved?"
- "Who had to believe it was worth doing?"
- "What proof, artifact, metric, or risk moved it from complaint to funded project?"
- "Is there already a budget category for this, or would someone need to create a new line item?"
- After a specific painful example is clear: "If something solved [their specific pain], what would need to be true for your company to actually approve buying it?"

Use 5-whys-style drills when the answer is vague or too agreeable:

- "Why is that hard?"
- "Why has nobody fixed it?"
- "Why did the last attempt fail?"
- "Why would or would not your boss fund fixing it?"
- "What would have to be true for them to fund it?"

### 5. Optional Solution Share -- Minutes 23-26

Only share a solution if they ask or the call has already produced a real example with at least one specific dollar amount, time cost, workload cost, risk, or consequence. If they have not quantified pain, keep asking about the example; the pitch has not been earned.

Keep the share to 60 seconds max. Use their language from earlier in the call, then test fit without leading:

- "Does that map to what you described, or am I off?"
- If they validate: "What's the closest thing to this you have seen? Why did it fail or fail to spread?"

If the call is still shallow, skip solution sharing and keep asking for concrete examples.

### 6. Close -- Minutes 26-30

Governing question: what concrete next step would improve learning?

The close should match the persona:

- At minute 20 of a 30-minute call, start preparing the next step even if some sections remain uncovered.
- Ask for a specific operator, buyer, planner, expediter, systems owner, or executive sponsor: "Who on your team lives this problem every single day? Can you introduce me to them this week, even for a 15-minute call?"
- Ask for a follow-up workflow observation, artifact walkthrough, redacted report, queue, inbox, tracker, ERP view, SOP, or screen share when appropriate.
- Pin a timeline: "Could we set that up before [specific date]?"
- If they offer to stay connected or be a design partner, make it concrete: "What does that look like concretely? Would you be open to a 2-hour working session where I observe the team's workflow? What works in the next two weeks?"
- If signal is strong, ask whether there is a legitimate narrow path to test with real workflow evidence and a clear proof measure.
- If signal is weak, ask who is closer to the pain and what hypothesis should be sharpened before returning.

## Persona Tailoring Layer

Apply this layer after the canonical call spine. Personas should change emphasis, vocabulary, probes, and close; they should not replace the spine.

### Executive / Decision-Maker

Use when the person is a COO, CFO, CEO, VP/SVP, GM, procurement/supply-chain executive, PE operating leader, or budget owner.

Opening logic:

> The main thing I'm trying to understand is what makes a supply chain or operations problem commercially worth solving.

Use three themes:

- **Pain:** Which recurring execution problems create real operating risk, not just frustration.
- **Economics:** Where cost or value shows up in real terms: P&L, working capital, service levels, avoided headcount, labor productivity, premium freight, quality cost, customer commitments, EBITDA, or growth without adding people.
- **Proof:** How leaders would measure and believe the value created by a solution, and what result would make the impact clear enough to justify spend.

Tune the script this way:

- Ask for the last real example that reached leadership attention.
- Probe visibility and operating truth: what leaders could not see early enough, what facts were stale, disputed, unreliable, or manually reconstructed.
- Tie cost to P&L, working capital, service levels, headcount, EBITDA, customer commitments, or value creation.
- Ask how similar problems get prioritized, sponsored, and funded.
- Ask what proof would make leaders believe a solution created enough value.
- Close for the right operator / workflow owner first; artifact access can happen with that person.

Do not make artifact access the primary ask for executives. Ask for the right operator or workflow owner first; artifact or workflow walkthrough can happen with that person.

If they could be a paid design partner and signal is strong, adapt this conditional close:

```markdown
If the signal is strong:

I'll be direct because we know each other: if this maps to a real [company] pain point, what would help me most is finding a narrow, legitimate way to work with real operators and real workflow evidence.

The useful version would need three things:

- **Access:** a real workflow, real operators, and enough system / report visibility to learn safely.
- **Proof:** a narrow scope with a clear before / after value measure.
- **Commercial commitment:** a named owner and a real paid-pilot or budget path if the value is credible.

Would it be reasonable to explore whether [company] could be that kind of design partner, or should I think about this as a referral path to someone else?
```

If signal is weak or unclear, keep the door open:

```markdown
If the signal is weak or unclear:

This has been helpful. It sounds like I may still need to sharpen the workflow or find the part of the organization where the pain is more acute.

Would it be reasonable to come back to you once I have a narrower hypothesis or prototype, especially if it maps more clearly to something [company] cares about?

In the meantime, is there someone at [company] or in your network who would be closer to the day-to-day execution pain and could help me keep learning?
```

### Operator / Workflow Owner

Use when the person owns or performs day-to-day work: buyer-planner, expeditor, materials planner, AP analyst, receiving/warehouse lead, production planner, customer supply-chain manager, procurement ops manager.

Opening logic:

> The main thing I'm trying to understand is how this work actually happens today, where it breaks, and what would make it meaningfully easier or better.

Organize around:

- **Workflow:** What happens step by step.
- **Friction:** Where work breaks, slows down, gets reworked, or depends on manual judgment.
- **Evidence:** What artifacts, queues, inboxes, trackers, reports, or system views show the real workflow.

Tune the script this way:

- Ask about day-to-day scope, work volume, systems, channels, peaks, and coverage gaps.
- Make the last real example concrete: trigger, people, systems, workaround, timeline, outcome.
- Walk through official process vs actual process, system of record vs where work actually happens, and what gets copied, reconciled, chased, or manually decided.
- Ask what happens when work is late, wrong, or missed; who complains; what metric or escalation shows impact.
- Ask for artifacts: redacted report, queue, inbox, tracker, ERP view, SOP, or screen share.
- Ask who else touches the workflow and who owns the metric or budget if the pain is real.

Do not over-index on P&L or pricing with operators. Capture concrete workflow evidence and consequences. Ask budget questions as "who would care enough to fix this?" or "who gets pulled in when this breaks?"

Close by asking for a redacted report, queue, inbox, tracker, ERP view, SOP, or screen share; ask who else touches the workflow; ask who owns the metric or budget if the pain is real.

### Advisor / Consultant / Investor / Pattern Observer

Use when the person sees many companies but may not own one workflow directly.

Opening logic:

> The main thing I'm trying to understand is which patterns repeat across companies, and which ones are actually budget-worthy rather than just operationally annoying.

Organize around:

- **Pattern:** What repeats across companies.
- **Segment:** Where pain is strongest and weakest.
- **Commerciality:** Which problems get budget, ownership, and implementation priority.

Tune the script this way:

- Ask what types of companies they see and where their pattern recognition is strongest.
- Ask for recent examples separated by company or context; do not let examples collapse into generic claims.
- Probe segment differences: industry, size, maturity, ERP/process maturity, PE-backed vs public vs founder-led.
- Compare wedges: PO exceptions, materials planning, logistics, AP, sourcing, quality, customer delivery.
- Ask what gets funded, what stays unfunded, where the CFO/COO/buyer accepts the case, and what implementation friction blocks adoption.
- Close for referrals to direct operators, buyers, or systems stakeholders who can validate details.

Do not let advisor calls become generic market pontification. Keep asking for recent examples, named patterns, and differences by context.

### Systems / IT / Implementation Stakeholder

Use when the person owns systems, integrations, security, data access, ERP, automation, IT, or implementation feasibility.

Opening logic:

> The main thing I'm trying to understand is what would make this feasible, safe, and governable in real systems.

Organize around:

- **Feasibility:** What access, data, and integration are possible.
- **Governance:** What controls, approvals, audit trails, and security requirements matter.
- **Implementation path:** What low-risk first scope could work.

Tune the script this way:

- Ask about ERP, MRP, AP, WMS, TMS, supplier portals, EDI, inboxes, identity, data warehouse, and reporting layers.
- Map where data is read, where decisions are recorded, and where writebacks happen.
- Probe access paths: read-only exports, APIs, mailbox, reports, RPA, database views, manual upload.
- Ask about data quality: missing fields, stale records, duplicates, ownership, master data.
- Ask about vendor review, AI policy, supplier/customer data rules, controls, approvals, and audit trails.
- Test the automation trust ladder: summarize, draft, recommend, approval-gated execute, policy execute, ERP writeback.
- Close by asking what low-risk technical path would make a pilot possible and who else must be involved.

Do not ask systems stakeholders to validate market pain as the primary topic. Use them to test feasibility, risk, and implementation path.

## Prompt Bank Appendix

Use these where relevant:

- "Tell me about the last time this happened."
- "What happened first?"
- "Who noticed?"
- "Where did the real work happen?"
- "What system was supposed to contain the truth?"
- "Where was the truth actually reconstructed?"
- "What did it cost, even if there was no clean line item?"
- "What has already been tried?"
- "Who owned the metric?"
- "Who controlled budget or approval?"
- "What proof would make leaders believe a solution created enough value?"
- "Is this a recurring class of problems or a one-off?"
- "What was the last similar tool, process improvement, headcount ask, or automation that got approved?"
- "What proof, artifact, metric, or risk moved it from complaint to funded project?"

Use Ryan Williams-style value probes when the call concerns willingness to pay:

- Generic follow-up may be hard to fund when cost is diffuse.
- Value gets stronger when tied to avoided headcount, growth without adding people, better planning data quality, or protection of high-risk commitments.
- Test whether the value case is hard dollars, avoided hire, growth leverage, service/risk reduction, or a more accepted operational cost bucket.
- Test whether transaction, exception, site, workflow, or outcome-based packaging would be easier to evaluate than a fixed software subscription.

## Post-Call Capture

Every generated prep guide should include fields for:

- Their #1 stated problem.
- Their #1 revealed problem from concrete stories.
- Gap between stated and revealed problem.
- Specific story captured.
- Dollar, time, risk, or workload cost surfaced.
- Prior attempts and why they failed.
- Buying condition and decision path.
- Referral or next step secured, with name and timeline.
- What changed about the product hypothesis.

## Self-Discipline Reminders

Include a short reminder block when useful, especially for founder-led calls:

- Put a physical timer or sticky note in view: "DON'T PITCH YET."
- Do not pitch before minute 20 unless they explicitly ask and a real example has already surfaced.
- Do not use "AI execution layer" or architecture language until the respondent has given at least one specific dollar amount, time cost, workload cost, risk, or consequence.
- Target 20-30% talk time.
- When they validate, flip it: "What has failed before?"
- When the urge to explain the product shows up, ask instead: "What would it be worth if that problem went away?" or "If you could snap your fingers, what would be different?"
- Silence is useful; count to 5.
- If the discussion turns into architecture explanation, stop and return to the customer's workflow, cost, prior attempts, or buying path.

## Quality Checklist

Before finishing:

- The guide has a short Goal line.
- The opening matches the persona's level.
- The first questions do not over-anchor on the product hypothesis.
- A specific recent example appears before solution sharing.
- The guide includes a step-back pattern check after the example.
- Prior attempts are captured before buying conditions.
- Buying questions are grounded in past approvals, current spend, approval paths, artifacts, and proof of value rather than abstract willingness to pay.
- The guide asks for one specific story, one dollar/time/risk cost, one failed prior attempt, one buying condition, and one referral or next step.
- Economics and proof are included for decision-makers.
- Artifacts/workflow walkthroughs are included for operators.
- Technical feasibility and governance are included for systems stakeholders.
- The close matches the person's role: paid design-partner path, operator referral, artifact walkthrough, or feasibility next step.
- Post-call capture fields include stated vs revealed problem and what changed about the hypothesis.
- Sources are listed when public research was used.
