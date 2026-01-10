# Reed

Language-agnostic code translation via a restricted intermediate representation.

## Overview

Reed translates code between languages by:
1. **Reading** source syntax into a minimal IR
2. **Transforming** the IR (optional optimization passes)
3. **Writing** the IR to target syntax

The IR is deliberately simple - expressions, statements, function calls, bindings, control flow. Domain-specific semantics (like "spawn an entity") are just function calls that the runtime handles.

## Architecture

```
reed/
├── crates/
│   ├── reed-ir/              # Core IR types
│   ├── readers/
│   │   ├── reed-read-ts/     # TypeScript → IR
│   │   └── reed-read-lua/    # Lua → IR
│   └── writers/
│       ├── reed-write-ts/    # IR → TypeScript
│       └── reed-write-lua/   # IR → Lua
```

### IR Design

The IR is **opcode-agnostic**. It doesn't know about game entities, file systems, or any domain. It only knows:

- Literals (null, bool, number, string, array, object)
- Expressions (binary ops, unary ops, calls, member access)
- Statements (let, if, while, for, return, block)
- Functions (parameters, body, return)

Domain operations like `lotus.spawn_entity(x)` are just function calls in the IR. The runtime (spore) provides the actual implementations.

### Readers & Writers

Each language has a reader (parse → IR) and writer (IR → emit):

| Language | Reader | Writer |
|----------|--------|--------|
| TypeScript | `reed-ts` | `reed-ts` |
| Lua | `reed-lua` | `reed-lua` |

Adding a new language requires implementing reader and/or writer for that language's syntax, targeting the same IR.

## Usage

```rust
use rhizome_reed_read_ts::TsReader;
use rhizome_reed_write_lua::LuaWriter;

// Read TypeScript
let ir = TsReader::parse("const x = 1 + 2;")?;

// Write to Lua
let lua = LuaWriter::emit(&ir);
// => "local x = 1 + 2"
```

## Relationship to Spore

Reed is a **compiler** - it translates source code. It doesn't execute anything.

Spore is a **runtime** - it executes Lua with native bindings. It doesn't translate anything.

They compose: Reed produces Lua source, Spore runs it with integrations (moss, lotus, fs, etc.).
