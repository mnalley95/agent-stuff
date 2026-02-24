---
name: compare-companies
description: Compare two companies side-by-side for competitive positioning analysis. Use when user asks to compare competitors, analyze differences between companies, create a competitive comparison, or generate a battlecard. Triggers on requests like "compare [company A] vs [company B]", "how does [A] differ from [B]", "[A] versus [B]", "competitive comparison", or "battlecard for [company]". Works with existing profile files or generates fresh profiles as needed.
---


# Compare Companies


Generate a side-by-side competitive comparison of two companies.


## Input Options


1. **Two existing profile files**: User provides paths to previously generated evaluate-company profiles
2. **Two company names**: Generate fresh profiles using evaluate-company process, then compare
3. **One profile + one company name**: Mix of existing and fresh data


## Process


### 1. Load or Generate Profiles


If profile files provided, read them. If company names provided, run the evaluate-company process for each.


### 2. Dimension-by-Dimension Comparison


Compare across all major dimensions:


| Dimension | Comparison Focus |
|-----------|------------------|
| Positioning | How do their value props differ? Who are they really for? |
| Target Audience | Overlapping vs. distinct segments? |
| Pricing | Price points, model differences, value perception |
| Product | Feature gaps, unique capabilities, platform differences |
| Content | Topic overlap, publishing volume, format differences |
| Growth Stage | Team size, hiring patterns, funding stage |
| Strategic Direction | Where is each headed? Collision course or diverging? |


### 3. Synthesize Analysis


Generate:
- **Overlap Analysis**: Where do they compete directly?
- **Differentiation Map**: What's unique to each?
- **Strengths/Weaknesses**: Relative to each other
- **Competitive Dynamics**: Who's winning where? Why?
- **Positioning Recommendations**: How to position against each


### 4. Output


Save as: `[company-a]-vs-[company-b]-[YYYY-MM-DD].md`


Example: `acme-vs-globex-2025-01-15.md`


## Output Template


```markdown
# [Company A] vs [Company B]


**Generated**: [YYYY-MM-DD]


---


## Executive Summary


[3-4 sentences: Who are these companies, how do they compare, what's the key takeaway]


---


## Quick Comparison


| Dimension | [Company A] | [Company B] |
|-----------|-------------|-------------|
| Tagline | [Tagline] | [Tagline] |
| Primary Audience | [Audience] | [Audience] |
| Pricing Model | [Model] | [Model] |
| Starting Price | [Price] | [Price] |
| Core Strength | [Strength] | [Strength] |
| Primary Weakness | [Weakness] | [Weakness] |


---


## Positioning Comparison


**[Company A] Positioning**:
[How they position themselves]


**[Company B] Positioning**:
[How they position themselves]


**Overlap**: [Where positioning overlaps]


**Divergence**: [Where they're clearly different]


---


## Target Audience Comparison


**[Company A] Targets**:
- Segment: [Segment]
- Company Size: [Size]
- Buyer: [Persona]


**[Company B] Targets**:
- Segment: [Segment]
- Company Size: [Size]
- Buyer: [Persona]


**Head-to-Head Competition**: [Where they compete for same buyers]


**Distinct Territories**: [Where each owns unique space]


---


## Product Comparison


| Capability | [Company A] | [Company B] |
|------------|-------------|-------------|
| [Capability 1] | ✓ / ✗ / Partial | ✓ / ✗ / Partial |
| [Capability 2] | ✓ / ✗ / Partial | ✓ / ✗ / Partial |
| [Capability 3] | ✓ / ✗ / Partial | ✓ / ✗ / Partial |


**[Company A] Unique Features**: [What only A has]


**[Company B] Unique Features**: [What only B has]


---


## Pricing Comparison


| Tier | [Company A] | [Company B] |
|------|-------------|-------------|
| Entry | [Price] | [Price] |
| Mid | [Price] | [Price] |
| Enterprise | [Price] | [Price] |


**Value Analysis**: [Who offers better value at what tier?]


**Pricing Strategy Differences**: [What pricing signals about strategy]


---


## Content & Marketing Comparison


| Aspect | [Company A] | [Company B] |
|--------|-------------|-------------|
| Publishing Frequency | [Frequency] | [Frequency] |
| Primary Topics | [Topics] | [Topics] |
| Content Formats | [Formats] | [Formats] |


**Content Overlap**: [Shared topics/themes]


**Content Gaps**: [What one covers that the other doesn't]


---


## Growth & Trajectory


| Signal | [Company A] | [Company B] |
|--------|-------------|-------------|
| Team Size | [Size] | [Size] |
| Hiring Focus | [Areas] | [Areas] |
| Recent Funding | [Status] | [Status] |
| Expansion Signals | [Signals] | [Signals] |


**Growth Assessment**: [Who's growing faster? In what direction?]


---


## SWOT: [Company A] Relative to [Company B]


**Strengths**: [What A does better than B]


**Weaknesses**: [Where A falls short vs B]


**Opportunities**: [Gaps A could exploit]


**Threats**: [Where B threatens A]


---


## SWOT: [Company B] Relative to [Company A]


**Strengths**: [What B does better than A]


**Weaknesses**: [Where B falls short vs A]


**Opportunities**: [Gaps B could exploit]


**Threats**: [Where A threatens B]


---


## Competitive Dynamics


**Current State**: [Who's winning? In what segments?]


**Trajectory**: [Are they converging or diverging?]


**Key Battlegrounds**: [Where will competition intensify?]


---


## Positioning Recommendations


**If competing against [Company A]**:
- Emphasize: [What to highlight]
- Avoid: [What not to compete on]
- Key message: [Suggested positioning]


**If competing against [Company B]**:
- Emphasize: [What to highlight]
- Avoid: [What not to compete on]
- Key message: [Suggested positioning]


---


## Source Profiles


- [Company A] Profile: [filename or "generated fresh"]
- [Company B] Profile: [filename or "generated fresh"]
```


## Usage Notes


- When both companies need fresh profiles, gather data for both before synthesizing comparison
- Use ✓ / ✗ / Partial for feature comparisons rather than verbose descriptions
- Be objective—note strengths and weaknesses for both sides
- Save output to `/mnt/user-data/outputs/` and present to user
- If user provides their own company as one side, tailor recommendations accordingly

