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
- `coro.rs`: Core trait definitions and combinators
- `suspend.rs`: `Suspend` enum and related types (`Yield`, `Return`)
- `from_fn.rs`, `yield_with.rs`: Constructor functions for coroutines
- `just_yield.rs`, `just_return.rs`: Simple coroutine implementations
- `metaprogramming.rs`: Type-level utilities for static guarantees

## Development Commands

### Build and Test
```bash
cargo build --verbose    # Build the project
cargo test --verbose     # Run all tests (unit + integration)
cargo check             # Type check without building
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
- Chain combinators: `.map_yield()`, `.map_return()`, `.flatten()`
- Static guarantees via `Yielded<Y, N>` and `Returned<R>` structs

### Dependencies
- `either = "1.11"` - Only external dependency
- Uses `core::` instead of `std::` for no-std compatibility

## Conventions

- Generic parameters should be declared in topological order. If one parameter
  "depends on" another parameter, the dependent parameter should appear later in
  the list of parameters than the independent parameter.
- The parameter order for input, yield, and return types for `Coro` traits and
  implementations should be `I`, `Y`, `R`.
