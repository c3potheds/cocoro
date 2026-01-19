# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

The `cocoro` crate is an experimental Rust library implementing stackless coroutines with compile-time type safety. Unlike `std::ops::Coroutine`, this approach uses move semantics to enforce that coroutines cannot be resumed after returning, preventing common contract violations at the type system level.

## Core Architecture

### Central Traits
- `Coro<I, Y, R>`: Main coroutine trait with `resume(self, input: I) -> Self::Suspend`
- `Suspended<I, Y, R>`: Abstraction over suspended states (yield/return)
- `SuspendedVisitor<I, Y, R, N>`: Visitor pattern for handling suspended states

### Key Design Principles
- Coroutines consume `self` on resume, preventing use after return
- Statically typed state machines with optional runtime branching
- Functional combinators for composition (similar to `Iterator`)
- No `Pin` requirement - avoids self-referential types

### Module Structure
- `coro.rs`: Core trait definitions and combinators (including `process()`, `compose()`, `weave()`)
- `suspend.rs`: `Suspend` enum and related types (`Yield`, `Return`)
- `from_fn.rs`, `yield_with.rs`: Constructor functions for coroutines
- `just_yield.rs`, `just_return.rs`: Simple coroutine implementations
- `process.rs`: Implementation of the `process()` method for stream processing with workers
- `continue_while.rs`: Combinator for "take while predicate" patterns
- `until.rs`: Combinator for "find first matching" patterns
- `take.rs`: Take N elements from a coroutine
- `with_state.rs`: State wrapper for stateful coroutines
- `from_control_flow.rs`: Convert `ControlFlow` to coroutines
- `weave.rs`: Interleave two coroutines (used by `process()`)
- `metaprogramming.rs`: Type-level utilities for static guarantees

## Development Commands

### Build and Test
```bash
cargo build --verbose    # Build the project
cargo test --verbose     # Run all tests (unit + integration)
cargo test --doc         # Run documentation tests only
cargo check              # Type check without building
cargo +nightly fmt       # Format code (uses nightly-only features for docstrings)
```

### Testing Structure
- Unit tests: `src/test.rs` (requires `extern crate alloc`)
- Integration tests: `tests/lib.rs`
- Test helper methods: `.assert_yields()`, `.assert_returns()`

### No-std Compatibility
The crate is `#![no_std]` compatible. Tests that require `std` features (like `String`) should use `extern crate alloc` and be placed in integration tests when they need `std`.

## Working with the Codebase

### Type Annotations
Due to generic design over `I`, `Y`, `R` parameters, type annotations are often required:
- Use `.yields::<Type>()` and `.returns::<Type>()` for disambiguation
- `Void` type indicates "never yields" or "never returns"

### Common Patterns
- Implement `Coro` trait directly for custom state machines
- Use `from_fn()` with closures for simple coroutines
- Chain combinators: `.map_yield()`, `.map_return()`, `.flatten()`, `.compose()`
- Static guarantees via `Yielded<Y, N>` and `Returned<R>` structs
- Use `process()` for advanced stream processing with worker coroutines
- Use `continue_while()` for "take while" and `until()` for "find first" patterns

### Important API Notes
- `take()` is a **free function**, not a method: use `.compose(take(10))` not `.take(10)`
- Examples in documentation should use correct imports and current API
- When adding new combinators, provide at least 2 clear examples in docstrings

### Dependencies
- `either = "1.11"` - Only external dependency
- Uses `core::` instead of `std::` for no-std compatibility

## Conventions

- Generic parameters should be declared in topological order. If one parameter
  "depends on" another parameter, the dependent parameter should appear later in
  the list of parameters than the independent parameter.
- The parameter order for input, yield, and return types for `Coro` traits and
  implementations should be `I`, `Y`, `R`.
- Always run `cargo +nightly fmt` before committing.

## Documentation

### Documentation Sync
**IMPORTANT**: `README.md` and `src/lib.rs` contain nearly identical documentation and must be kept in sync. When updating one, update the other.

Differences allowed:
- Minor formatting/rendering adjustments for crates.io vs GitHub
- Both should be functionally equivalent

### Writing Documentation
- Use clear, concise language
- Provide at least 2 examples per combinator: one simple, one showing practical usage
- Test all docstring examples with `cargo test --doc`
- Avoid examples that show patterns that don't work well (e.g., don't use `continue_while()` for chunking that requires splitting inputs)

## Git Workflow

### Commit Guidelines
- **Each commit must pass tests independently** - verify with `cargo test` at each commit
- When doing interactive rebases, test each commit after rebasing
- Use clear, descriptive commit messages
- For breaking changes or new features, mention them in commit messages
- Co-author commits with: `Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>`

### Testing Commit History
```bash
# Test each commit in a range
git rebase -i <base-commit>
# Mark commits as 'edit', then for each:
cargo test
git rebase --continue
```
