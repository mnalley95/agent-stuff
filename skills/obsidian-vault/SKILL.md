---
name: obsidian-vault
description: "Add, update, organize, or search markdown notes in Obsidian. Use when the user asks to capture notes, save research, add meeting notes, create daily notes, append to an existing Obsidian note, maintain markdown/frontmatter/tags/backlinks, or otherwise put information into Obsidian."
---

# Obsidian Vault

## Overview

Use this skill to write markdown into Obsidian:

`/Users/mark/Library/Mobile Documents/iCloud~md~obsidian/Documents/vault/`

The vault path contains a space, so always quote it in shell commands. The vault is outside most repo sandboxes, so request filesystem approval when a task needs to create or edit vault files.

## Workflow

1. Clarify the target note only when the prompt does not imply a reasonable location or filename.
2. Inspect existing notes before appending or editing. Use `rg`/`find` against the quoted vault path.
3. Prefer markdown that works well in Obsidian: `#` headings, `[[wikilinks]]`, `#tags`, and YAML frontmatter when useful.
4. Preserve existing frontmatter and user formatting when editing an existing note.
5. Avoid overwriting notes unless the user explicitly asks. Append or create a dated sibling note when unsure.
6. After writing, report the absolute path of the changed note.

## Note Placement

- Use an existing folder when the user's request names or implies one.
- Use `Inbox/` for quick capture when no better folder is clear.
- Use `Daily/YYYY-MM-DD.md` for daily logs, journal entries, or date-specific notes unless the vault already uses a different daily-note convention.
- Use readable filenames with spaces or hyphens; keep `.md` extension.

## Editing Existing Notes

- Read the target note before editing.
- Keep YAML frontmatter at the top.
- For append operations, add a heading or timestamp when it improves scanability.
- Do not reorganize or rename existing notes unless requested.
- Match the existing tone or format of the note unless otherwise specified.
