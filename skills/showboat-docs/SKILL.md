---
name: showboat-docs
description: Use after building or changing code to manually test it works, capturing the proof with Showboat. Applies whenever you've written code that should be exercised — run it, verify the output, and document the result as a reproducible artifact.
---

# Agentic Manual Testing with Showboat

Never assume code you generate works until you've executed it. Automated tests are valuable but insufficient — they can pass while the code crashes, misses UI elements, or fails on untested edge cases. **You should witness the feature working before calling it done.** 

The above is important - this is about proving that a feature works. It is not sufficient to run the test suite. Document, explore, and think through edge cases that may not be captured in the tests. Document their work and show proof that things work as intended.

Use Showboat to capture your manual testing as a reproducible, verifiable document.

## When to Use This

- After implementing or changing a feature — prove it works
- After fixing a bug — prove the fix holds
- When exploring an API or CLI — document what you discover
- When a user asks you to demonstrate behavior — show, don't tell

## How to Test by Context

**Python libraries and scripts:** Use `python -c "..."` or write a demo script to `/tmp` and run it. Import the module, call the function, print results.

**CLIs:** Run the command with `--help` first, then exercise the actual behavior with real arguments.

**Web APIs:** Use `curl` to explore endpoints against a running dev server. Don't just hit one — explore multiple aspects of the API.

**Web UIs:** Use browser automation (e.g. Playwright via a CLI wrapper) to interact with the page, take screenshots, and verify visual state.

**Compiled languages:** Write a small demo file in `/tmp`, compile and run it — keep throwaway test code out of the repo.

## Fix-and-Document

When manual testing reveals a problem, fix it. Then consider writing a proper automated test for the issue (red/green TDD). The Showboat document captures the full arc: discovery, fix, and proof.

## Showboat Commands

Inspect the CLI before writing anything:

```bash
uvx showboat --help
```

The three core commands:

- **`exec`** — Run a command and capture its output. This is the heart of manual testing. The output is real, not fabricated.
- **`note`** — Add Markdown commentary: context, expectations, interpretation of results.
- **`image`** — Embed images like browser screenshots.

Supporting commands:

- **`init`** — Create a new document.
- **`pop`** — Remove the last block if a command was wrong or produced noise.
- **`verify`** — Re-run all `exec` blocks and confirm outputs still match.

## Workflow

1. Check `uvx showboat --help` for current options.
2. If existing Showboat docs exist in the repo, read one to match the style.
3. Init the document, then build it incrementally with `note` and `exec`.
4. Let the executable transcript do most of the work — keep commentary short.
5. Verify at the end:
   ```bash
   uvx showboat verify <path-to-doc>.md
   ```

## Typical Pattern

```bash
uvx showboat init docs/showboat/feature-name.md "Feature Title"
uvx showboat note docs/showboat/feature-name.md "What we're testing and why."
uvx showboat exec docs/showboat/feature-name.md bash "command --help"
uvx showboat exec docs/showboat/feature-name.md bash "<command that exercises the feature>"
uvx showboat exec docs/showboat/feature-name.md bash "<edge case or error path>"
uvx showboat verify docs/showboat/feature-name.md
```

## Heuristics

- Seed deterministic test data before exercising the feature — use `/tmp` for throwaway files and databases.
- Include at least one success path and one interesting edge case.
- When a command intentionally fails (to prove validation works), capture it explicitly.
- If `verify` fails after edits, re-run the broken `exec` blocks rather than hand-editing output.
- Keep documents focused on proof. Design discussion belongs elsewhere.

## Repo-specific guidance
- Check to see if there are other showboat docs in './docs/showboat/' if so, save your new document there. If not, create that directory.

## Resources
- https://simonwillison.net/guides/agentic-engineering-patterns/agentic-manual-testing/