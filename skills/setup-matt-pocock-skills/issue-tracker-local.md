# Issue tracker: Local Markdown

Issues and PRDs for this repo live as markdown files in `.scratch/`.

## Conventions

- One feature per directory: `.scratch/<feature-slug>/`
- The PRD is `.scratch/<feature-slug>/PRD.md`
- Implementation issues are `.scratch/<feature-slug>/issues/<NN>-<slug>.md`, numbered from `01`
- PRD review state is recorded as a `PRD Status:` line near the top of each PRD file. Use `draft`, `reviewed`, `accepted`, or `superseded`.
- Issue triage state is recorded as a `Status:` line near the top of each issue file (see `triage-labels.md` for the role strings). Reserve `Status:` for triage-able implementation issues.
- Comments and conversation history append to the bottom of the file under a `## Comments` heading

## When a skill says "publish to the issue tracker"

Create a new file under `.scratch/<feature-slug>/` (creating the directory if needed).

## When a skill says "fetch the relevant ticket"

Read the file at the referenced path. The user will normally pass the path or the issue number directly.
