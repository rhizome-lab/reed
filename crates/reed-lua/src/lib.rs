//! Lua reader/writer for reed IR.
//!
//! - `LuaWriter`: Emit IR as Lua source code
//! - `LuaReader`: Parse Lua source to IR (TODO)

mod writer;

pub use writer::LuaWriter;
