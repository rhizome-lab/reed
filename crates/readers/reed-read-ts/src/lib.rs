//! TypeScript reader for reed IR.
//!
//! Parses TypeScript source code into reed IR using tree-sitter.

mod reader;

pub use reader::{read, ReadError};
