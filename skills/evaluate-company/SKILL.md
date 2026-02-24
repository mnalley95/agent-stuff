---
name: evaluate-company
description: Generate a comprehensive competitive intelligence profile for any company. Use when user asks to research a competitor, evaluate a company, create a competitor profile, or gather intelligence on a business. Triggers on requests like "profile [company]", "evaluate [company]", "research [company] as a competitor", "competitive analysis of [company]", or "what is [company] doing".
allowed-tools: WebSearch, WebFetch, Task
---


# Evaluate Company


Generate a point-in-time competitive intelligence snapshot for a single company.


## Process


### 1. Gather Data


Use web_search and web_fetch to collect information across these dimensions:


| Dimension | Search Query Pattern | What to Extract |
|-----------|---------------------|-----------------|
| Homepage | `[company]` → fetch main site | Tagline, hero messaging, primary value prop |
| Sitemap | Fetch `https://[company-domain]/sitemap.xml` | Full site structure, page categories, hidden sections |
| LinkedIn Posts | Fetch `https://www.linkedin.com/company/[company-slug]/posts/` | Recent announcements, tone, priorities, campaign themes |
| Pricing | `[company] pricing` | Tiers, price points, packaging model, free tier |
| Product/Features | `[company] features` or product page | Core capabilities, recent additions, integrations |
| Blog/Content | `[company] blog` | Recent topics, publishing frequency, content themes |
| Careers | `[company] careers jobs` | Open roles, departments hiring, growth signals |
| News | `[company] news 2025` | Funding, partnerships, launches, executive changes |
| About/Team | About page | Founding story, team size, leadership, mission |
| Customers | Case studies or customers page | Target segments, logo customers, use cases |


For each search, fetch the most relevant page to extract details. Skip dimensions that return no useful results.

**Sitemap traversal**: After fetching `/sitemap.xml`, parse all `<loc>` URLs and `<sitemap>` child references. Follow nested sitemaps (e.g. `/sitemap-products.xml`, `/sitemap-blog.xml`) one level deep. Use the full URL tree to identify product areas, content hubs, and site sections that may not appear in top-level navigation—these often reveal strategic priorities the company doesn't advertise prominently.

**LinkedIn**: Use the URL pattern `https://www.linkedin.com/company/[company-slug]/posts/` where the slug is typically the lowercased, hyphenated company name (e.g. `acme-corp`). Extract post topics, frequency, tone, and any product or hiring announcements from recent posts. If web_fetch is blocked by a login wall, note "LinkedIn blocked" and fall back to `[company] site:linkedin.com/company` search.

**Parallel data gathering with subagents**: All dimensions in the table above are independent—use the Task tool to dispatch them concurrently rather than sequentially. Spawn one `general-purpose` subagent per logical group, give each its assigned dimensions and the company name, and collect results before synthesizing. Suggested grouping:

| Subagent | Dimensions |
|----------|-----------|
| A | Homepage, Sitemap |
| B | LinkedIn Posts |
| C | Pricing, Product/Features |
| D | Blog/Content, Customers |
| E | Careers, News, About/Team |

Each subagent should return its raw findings as text. Aggregate all results, then proceed to the Analyze step.


### 2. Analyze


Synthesize findings into these categories:


**Positioning**: What problem do they claim to solve? For whom? What's their unique angle?


**Target Audience**: Who are they selling to? (Company size, industry, role, use case)


**Business Model**: How do they make money? Pricing structure? Sales motion (self-serve vs. sales-led)?


**Product Focus**: Core product capabilities. What do they emphasize? What's missing?


**Content Strategy**: What topics do they publish about? How often? What formats?


**Growth Signals**: Hiring velocity, departments expanding, funding, new markets


**Strategic Direction**: Based on all signals, where are they headed?


### 3. Output


Save as markdown file: `[company-slug]-[YYYY-MM-DD].md`


Example: `acme-corp-2025-01-15.md`


## Output Template


```markdown
# [Company Name] - Competitive Profile


**Generated**: [YYYY-MM-DD]
**Website**: [URL]


---


## Summary


[2-3 sentence overview of who they are and what they do]


---


## Positioning


**Tagline**: [Their primary tagline/headline]


**Value Proposition**: [What they claim to deliver]


**Differentiation**: [How they position against alternatives]


---


## Target Audience


**Primary Segment**: [Who they're selling to]


**Company Size**: [SMB / Mid-market / Enterprise]


**Buyer Persona**: [Role/title they target]


**Use Cases**: [Primary use cases they highlight]


---


## Product


**Core Capabilities**:
- [Capability 1]
- [Capability 2]
- [Capability 3]


**Notable Features**: [Anything distinctive]


**Integrations**: [Key integrations mentioned]


**Platform**: [Web / Mobile / Desktop / API]


---


## Pricing


**Model**: [Freemium / Free trial / Sales-led / etc.]


**Tiers**:
| Tier | Price | Key Inclusions |
|------|-------|----------------|
| [Name] | [Price] | [What's included] |


**Pricing Signals**: [Cheap/expensive for category, recent changes, etc.]


---


## Content & Marketing


**Blog Frequency**: [Posts per week/month]


**Content Themes**:
- [Theme 1]
- [Theme 2]


**Content Formats**: [Blog / Podcast / Video / Webinars / etc.]


**SEO Focus**: [Keywords they appear to target]


**LinkedIn Activity**: [Post frequency, recent topics, tone, notable announcements]


---


## Hiring & Growth


**Open Roles**: [Number and types]


**Departments Hiring**: [Engineering / Sales / Marketing / etc.]


**Growth Signals**: [What hiring suggests about strategy]


---


## Recent News


- [Date]: [News item 1]
- [Date]: [News item 2]


---


## Strategic Assessment


**Current Focus**: [What they're prioritizing now]


**Likely Direction**: [Where they seem headed]


**Strengths**: [What they do well]


**Weaknesses/Gaps**: [What's missing or weak]


---


## Raw Data Sources


- [URL 1]: [What was extracted]
- [URL 2]: [What was extracted]
```


## Usage Notes


- If Apify is available, use for LinkedIn company posts or deeper social data when web_fetch is insufficient
- Always include the date in filename—profiles are point-in-time snapshots
- Save output to `/mnt/user-data/outputs/` and present to user
- If a dimension has no data, note "Not found" rather than omitting the section
- Prioritize accuracy over completeness—don't invent details

