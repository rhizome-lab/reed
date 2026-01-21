# CLAUDE.md

Behavioral rules for Claude Code in this repository.

## Overview

Reed is a language translation layer. It converts source code between languages via a restricted intermediate representation (IR).

### Architecture

```
Source (TypeScript, Lua, etc.)
    ↓ Reader
Restricted IR (expressions, statements, calls)
    ↓ Writer
Target (Lua, TypeScript, etc.)
```

### Key Concepts

**Opcode-agnostic IR:** The IR doesn't know about domains (games, files, etc.). Domain operations like `lotus.spawn_entity(x)` are just function calls. The runtime (spore) provides implementations.

**Readers & Writers:** Each language has a reader (parse → IR) and writer (IR → emit). Adding a language means implementing one or both.

## Core Rule

**Note things down immediately:**
- Bugs/issues → fix or add to TODO.md
- Design decisions → docs/ or code comments
- Future work → TODO.md
- Key insights → this file

**Triggers:** User corrects you, 2+ failed attempts, "aha" moment, framework quirk discovered → document before proceeding.

**Don't say these (edit first):** "Fair point", "Should have", "That should go in X" → edit the file BEFORE responding.

**Do the work properly.** When asked to analyze X, actually read X - don't synthesize from conversation.

**If citing CLAUDE.md after failing:** The file failed its purpose. Adjust it to actually prevent the failure.

## Behavioral Patterns

From ecosystem-wide session analysis:

- **Question scope early:** Before implementing, ask whether it belongs in this crate/module
- **Check consistency:** Look at how similar things are done elsewhere in the codebase
- **Implement fully:** No silent arbitrary caps, incomplete pagination, or unexposed trait methods
- **Name for purpose:** Avoid names that describe one consumer
- **Verify before stating:** Don't assert API behavior or codebase facts without checking

## Commit Convention

Use conventional commits: `type(scope): message`

Types:
- `feat` - New feature
- `fix` - Bug fix
- `refactor` - Code change that neither fixes a bug nor adds a feature
- `docs` - Documentation only
- `chore` - Maintenance (deps, CI, etc.)
- `test` - Adding or updating tests

Scope is optional but recommended for multi-crate repos.

## Negative Constraints

Do not:
- Announce actions ("I will now...") - just do them
- Leave work uncommitted
- Create special cases - design to avoid them
- Create legacy APIs - one API, update all callers
- Do half measures - migrate ALL callers when adding abstraction
- Mark as done prematurely - note what remains

## Design Principles

**IR stays minimal.** Only add to the IR what multiple languages need. Domain-specific constructs stay as function calls.

**Readers are permissive, writers are strict.** Readers should handle real-world code variations. Writers emit canonical, clean output.

**Unify, don't multiply.** One interface for multiple cases > separate interfaces. Plugin systems > hardcoded switches.

**Simplicity over cleverness.** Functions > traits until you need the trait.

## Crate Structure

All crates use the `rhizome-reed-` prefix:
- `rhizome-reed-ir` - Core IR types
- `rhizome-reed-read-ts` - TypeScript reader
- `rhizome-reed-read-lua` - Lua reader
- `rhizome-reed-write-ts` - TypeScript writer
- `rhizome-reed-write-lua` - Lua writer
