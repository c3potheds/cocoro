//! The `cocoro` crate shows a different approach to coroutines in Rust from the
//! one in `std::ops` that's guarded by the `coroutine_trait` feature.
//!
//! A *coroutine* is a state machine that may consume "input" values one at a
//! time, and each time "yields" a value *or* "returns" with a final value. A
//! coroutine can yield arbitrarily many times but may only return once.
//!
//! In this crate, the core coroutine trait looks like:
//!
//! ```rust
//! pub trait SuspendedVisitor<I, Y, R, N>
//! where
//!     N: Coro<I, Y, R>,
//! {
//!     type Out;
//!     fn on_yield(self, y: Y, next: N) -> Self::Out;
//!     fn on_return(self, r: R) -> Self::Out;
//! }
//!
//! pub trait Suspended<I, Y, R> {
//!     type Next: Coro<I, Y, R>;
//!     fn visit<X>(
//!         self,
//!         visitor: impl SuspendedVisitor<I, Y, R, Self::Next, Out = X>,
//!     ) -> X;
//! }
//!
//! pub trait Coro<I, Y, R = ()>: Sized {
//!     type Next: Coro<I, Y, R>;
//!     type Suspend: Suspended<I, Y, R, Next = Self::Next>;
//!     fn resume(self, input: I) -> Self::Suspend;
//! }
//! ```
//!
//! Note the following differences from `std::ops::Coroutine`:
//!
//!   * The `resume` method takes `self` by value, not by a pinned exclusive
//!     reference.
//!   * The types for "yield" and "return" are generic parameters rather than
//!     associated types.
//!   * The `resume` method returns a `Suspend` type that wraps the state of
//!     the coroutine, which provides to a "visitor" a handle to a coroutine
//!     that can be resumed again.
//!
//! The `Suspended` trait can be thought of as an abstraction over an enum.
//! Indeed, many implementations of coroutines will use the following enum which
//! is provided by the crate:
//!
//! ```rust
//! pub enum Suspend<Y, R, N> {
//!     Yield(Y, N),
//!     Return(R),
//! }
//! ```
//!
//! The `Yield` and `Return` variants are imported into the crate's root
//! namespace, so they can be used withoutthe `Suspend::` prefix.
//!
//! In addition, the `Coro` trait provides a number of default combinators that
//! should feel familiar to anyone working with `Iterator`, for example:
//!
//!   * `map_yield` to transform the yielded values with an `FnMut`
//!   * `map_return` to transform the return value with an `FnOnce`
//!   * `flatten` to flatten a coroutine that returns another coroutine into a
//!     single coroutine
//!
//! This crate makes no attempt to use fancy macros or code transformations to
//! let you write coroutines as if they were procedural functions, as the
//! `coroutine_trait` feature does. Instead, it is designed for a functional
//! style where the elements yielded and returned are transformed with pipelines
//! of combinators, with an emphasis on type-safety.
//!
//! # Examples
//!
//! ## A basic counter
//!
//! Here's a coroutine that yields successive integers and never returns:
//!
//! ```rust
//! use cocoro::{Coro, Suspended, Yielded};
//!
//! struct Counter(i32);
//! impl Coro<(), i32, ()> for Counter {
//!     type Next = Self;
//!     type Suspend = Yielded<i32, Self>;
//!     fn resume(self, _: ()) -> Self::Suspend {
//!         Yielded(self.0, Counter(self.0 + 1))
//!     }
//! }
//!
//! let mut counter = Counter(0);
//! for _ in 0..10 {
//!     let Yielded(n, next) = counter.resume(());
//!     println!("{}", n);
//!     counter = next;
//! }
//! ```
//!
//! Notice how the `Counter` struct is immutable, and the next state is
//! returned from the `resume` method by constructing a new `Counter` instance.
//!
//! Because the `Next` associated type is `Self`, we are able to mutate the
//! variable `counter` in-place. However, in the next example, we'll see a
//! coroutine that yields with a state of a different type than itself.
//!
//! Furthermore, we can irrefutably match the `Yielded` struct because the
//! `Suspend` associated type of the coroutine is known at compile time and
//! transparent to the user. More generally, when the `Suspend` associated type
//! of a coroutine isn't known (e.g. because it's not constrained by bounds on
//! a function parameter or on an `impl Coro` return type from the function that
//! the coroutine came from), the `Suspended` trait provides a `visit` method
//! that can be converted to a `Suspend` enum to pattern-match against, or
//! visited directly with a `SuspendedVisitor`.
//!
//! It's more common to use helper functions and combinators to create
//! coroutines, rather than implement the `Coro` trait directly.
//!
//! Using the `yield_with()` function, we can do the same thing as above with a
//! closure:
//!
//! ```rust
//! use cocoro::{Coro, Void, Yield, yield_with};
//! let mut i = 0;
//! let _: Option<Void> = yield_with(|()| {
//!     i += 1;
//!     i
//! })
//! .take(10)
//! .for_each(|n| {
//!     println!("{}", n);
//! });
//! ```
//!
//! ## Static-sized countdown
//!
//! ```rust
//! use cocoro::{Coro, Returned, Suspend, Suspended, Void, Yielded};
//!
//! struct Three;
//! struct Two;
//! struct One;
//! #[derive(Debug, PartialEq, Eq)]
//! struct Blastoff;
//!
//! impl Coro<(), i32, Blastoff> for Three {
//!     type Next = Two;
//!     type Suspend = Yielded<i32, Self::Next>;
//!     fn resume(self, _: ()) -> Self::Suspend {
//!         Yielded(3, Two)
//!     }
//! }
//!
//! impl Coro<(), i32, Blastoff> for Two {
//!     type Next = One;
//!     type Suspend = Yielded<i32, Self::Next>;
//!     fn resume(self, _: ()) -> Self::Suspend {
//!         Yielded(2, One)
//!     }
//! }
//!
//! impl Coro<(), i32, Blastoff> for One {
//!     type Next = Blastoff;
//!     type Suspend = Yielded<i32, Self::Next>;
//!     fn resume(self, _: ()) -> Self::Suspend {
//!         Yielded(1, Blastoff)
//!     }
//! }
//!
//! impl Coro<(), i32, Blastoff> for Blastoff {
//!     type Next = Void;
//!     type Suspend = Returned<Blastoff>;
//!     fn resume(self, _: ()) -> Self::Suspend {
//!         Returned(Blastoff)
//!     }
//! }
//!
//! let countdown = Three;
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Returned(blastoff) = countdown.resume(());
//! println!("{:?}!", blastoff);
//! ```
//!
//! This shows how the `Next` associated type can be used to chain together
//! coroutines of different types, as long as they all have the same input and
//! output types (`Y`, `R`, and `I`).
//!
//! The `as_yield()` and `as_return()` helper methods on `Suspended` provide a
//! convenient way to get an `Option` of the yielded value or the return value,
//! respectively.
//!
//! Another thing to note: the `Void` coroutine can be used as the `Next` type
//! to statically indicate that the coroutine will not yield again. Because the
//! `Void` type is not instantiable, it is impossible to `Yield` from a
//! coroutine whose `Next` type is `Void`.
//!
//! One could have written the same example using closures:
//!
//! ```rust
//! use cocoro::{Coro, Returned, Suspended, Void, Yielded, from_fn};
//! #[derive(Debug, PartialEq, Eq)]
//! struct Blastoff;
//! #[rustfmt::skip]
//! let countdown = from_fn(|_| {
//!     Yielded(3, from_fn(|_| {
//!     Yielded(2, from_fn(|_| {
//!     Yielded(1, from_fn(|_| {
//!     Returned(Blastoff) })) })) }))
//! });
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Yielded(n, countdown) = countdown.resume(());
//! println!("{}", n);
//! let Returned(blastoff) = countdown.resume(());
//! println!("{:?}!", blastoff);
//! ```
//!
//! This is considerably more compact, but formatting it the way that rustfmt
//! wants can look very unapproachable. Nevertheless, this example shows that
//! you can define coroutines with a static, type-safe state machine from
//! ordinary closures and proves that this state information is preserved at
//! compile time with irrefutable pattern matching.
//!
//! More commonly, you will use the `Suspend` enum, rather than `Yielded` or
//! `Returned` structs, in `from_fn()` coroutines, especially if the coroutine
//! uses run-time information to determine whether to yield or return.
//!
//! # Theory
//!
//! The main motivation of this crate is to show how the Rust type system could
//! provide compile-time correct-by-construction guarantees that standard
//! library coroutines, or other state machines like `Iterator` and `Future`,
//! imply by contract but cannot enforce at compile time: they will never yield
//! again after returning. Most design choices follow from that, including,
//! ultimately, the emphasis on functional combinators, which are helpful to add
//! the missing expressivity that can't come from procedural `gen` blocks. `gen`
//! blocks are the reason for `Pin`, which would have, if used in `cocoro`
//! coroutines, forced a `resume()` method to mutably borrow a (pinned)
//! reference rather than accept `self` by value, which was the only way to
//! "maybe consume" the coroutine, passing it back to the caller on yield but
//! dropping it on return.
//!
//! Because `cocoro` coroutines are implemented with combinators and manual
//! impls instead of syntactic sugar like `gen` blocks or macros that transform
//! code, the trait was designed to be interoperable with other traits and use
//! functional programming patterns make them as expressive as their procedural
//! counterparts.
//!
//! A `cocoro` coroutine is a functor over both the yielded type and the return
//! type. The `map_yield` and `map_return` combinators correspond to the
//! theoretical `map` operation on these respective interpretations of the
//! coroutine as a functor.
//!
//! A coroutine is also a *contravariant functor* over the input type, so you
//! can `contramap_input()` with a function that takes a different input type
//! and returns the original input type, and get a new coroutine that uses the
//! `contramap()` function's input type as its input type.
//!
//! The `flatten()` combinator corresponds to the `join` operation on the monad
//! over the return type. With it and `map_return()`, the `flat_map()`
//! combinator can be implemented, corresponding to the `bind` operation on the
//! monad.
//!
//! In order to complete the monad axioms, the `return` operation is implemented
//! with the `JustReturn` wrapper struct, which is a coroutine that can take
//! anything as an input, never yields, and always returns with the value it was
//! constructed with. Together, the `JustReturn` struct and `flat_map()`
//! combinator abide by the monad laws for the functor over the return type.
//!
//! There is no monad implementation for the functor over the yielded type, but
//! the `just_yield` struct can be thought of as a `pure` operation for an
//! *applicative functor* over the yielded type. The `zip()` combinator
//! meanwhile is an operation on which the applicative `lifta2` function can be
//! derived.
//!
//! Formalizing operations on coroutines as well-established functional
//! programming concepts is a way to show that the `cocoro` coroutines are
//! theoretically sound and can be used in a variety of ways that are familiar
//! to functional programmers. Which includes Rust programmers who are familiar
//! with other types with similar traits, like `Iterator` and `Result`.
//!
//! # FAQ
//!
//! ## Why is it called `cocoro`?
//!
//! I was thinking more along the lines of a pun: "coro" as an abbreviation
//! for "coroutine", and "co" as a prefix meaning "together" or "with" to indicate
//! its complementarity and perhaps subordination to Rust standard library
//! coroutines. As another layer to the pun, "cocoro" sounds like "kokoro",
//! which is the Japanese word for "heart", with all the attendant connotations
//! of mind, spirit, and core-ness.
//!
//! I was also vaguely gesturing at the idea of co- as a prefix for mathematical
//! duals, especially in the context of category theory. Although a coroutine is
//! not the categorical dual of a routine in any strict sense, one can entertain
//! the concept of a "co-routine" as the dual of a "routine", and thus a
//! co-coroutine as something... routine.
//!
//! But also the name happened to be free on crates.io.
//!
//! ## Why is the `resume()` method consuming `self`?
//!
//! In the `std::ops::Coroutine` trait, the `resume` method takes
//! `Pin<&mut Self>` as the receiver type. This was done with the ability to
//! write [`gen` blocks](https://github.com/rust-lang/rust/issues/117078) in
//! mind, where the language would synthesize a state machine out of procedural
//! code. These state machines could include local variables that are references
//! to other local variables in the block, making the type is self-referential
//! immovable after the references may have been created. To enforce this,
//! generators require a `Pin<&mut Self>` to ensure that the generator is
//! "pinned" in memory and cannot be moved. The same goes for the more
//! well-known, and more stable, `Future` trait, for which self-referential
//! implementations can be constructed with `async` blocks.
//!
//! The `cocoro` crate does not need to support self-referential types, because
//! it does not try to describe the state machines of `gen` blocks. Instead,
//! coroutines are hand-written or composed from combinators and closures that
//! are not suspended.
//!
//! ## Ok, but why not use `&mut self` as the reciver for `resume()`?
//!
//! This is an experiment to try to express in the type system something that is
//! implicit in the contract of types like `Iterator`, `Future`, and, yes,
//! `std::ops::Coroutine`. The contract is that once you hit the "end" of the
//! iterator/future/coroutine (i.e. it returns `None`/`Ready`/`Complete`), you
//! can't call `next()`/`poll()`/`resume()` again. Some utility functions like
//! `Iterator::fuse()` attempt to add a runtime safety layer to more concretely
//! define what happens when the contract is violated.
//!
//! But `cocoro` chooses a different approach: enforce this contract at compile
//! time. The `resume()` method consumes `self` and returns a `Suspended` enum
//! that can only obtain the coroutine back if it `Yield`s. This makes it
//! impossible to call `resume()` again after a `Return`, because the `Return`
//! variant does not include a coroutine to resume, and calling `resume()`
//! had already consumed the coroutine.
//!
//! ## How's the performance compared to `std::ops::Coroutine`?
//!
//! The `cocoro` coroutines are designed to be as lightweight as possible. But
//! so are the `std::ops::Coroutine` coroutines, and by people who are much more
//! dedicated and experienced in writing high-performance Rust code.
//!
//! `cocoro` coroutines are "stackless" and avoid allocation, just like
//! `std::ops::Coroutine` coroutines. One potential benefit of `cocoro`
//! coroutines is that standard library coroutines are desugared into state
//! machines that may be represented as a Rust enum (see the [unstable book](
//! https://doc.rust-lang.org/beta/unstable-book/language-features/coroutines.html#coroutines-as-state-machines))
//! which means that every iteration of the state machine may contain a branch.
//! `cocoro` coroutines, on the other hand, support coroutines that are
//! statically known to proceed deterministically to a particular state, as
//! seen with the `just_yield()` and `just_return()` functions. Such coroutines
//! eliminate branches at these points, which may lead to faster code.
//!
//! However, `cocoro` coroutines move the data that represents their state
//! around when resumed and suspended, as opposed to standard library
//! coroutines, which must be pinned in memory. This means there may be copy
//! operations in the code generated by `cocoro` coroutines that don't appear
//! in the code generated by standard library coroutines. I generally expect the
//! compiler to optimize these copies away, but I haven't done any benchmarks to
//! confirm this.
//!
//! Many basic `cocoro` combinators are tail recursive and can be optimized by
//! the compiler into loops, but Rust does not guarantee tail call optimization
//! in general.
//!
//! ## Why return an associated type instead of a `Suspend` enum?
//!
//! This is the secret sauce that allows `cocoro` coroutines to opt into
//! statically deterministic state machines. The `Next` associated type of a
//! coroutine is the type of the next state of the coroutine, and if every
//! coroutine were to neecessarily return an enum, the compiler would have to
//! insert a check on that enum's tag when matching the result.
//!
//! In particular, this lets `just_yield()` coroutines return a `Yielded` struct
//! instead of a `Suspend` enum's `Yield` variant, and `just_return()`
//! coroutines return a `Returned` struct instead of a `Suspend` enum's
//! `Return` variant. All three of `Suspend`, `Yielded`, and `Return` implement
//! the `Suspended` trait, so they are all valid options to return from a
//! coroutine depending on whether it is known to branch at runtime.
//!
//! ## Why are the yield and return types generic type parameters?
//!
//! It's an established convention in Rust that "input" types for a trait are
//! generic types, and "output" types are associated types, as seen in the
//! `FnOnce` trait. It's important that traits like this can be generic over
//! input types, i.e. implement the trait for arbitrarily many possible
//! input types. This is what allows the `Fn` series of traits to work with
//! reference types, which are actually higher-ranked types that are generic
//! over an elided lifetime parameter.
//!
//! The `Coro` trait is generic over not just the input type, but the yield and
//! return types too. This is to allow a type that implements `Coro` to
//! implement the trait for arbitrarily many input, yield, and return types.
//! For example, a coroutine that ignores its input can be generic over all
//! `I` input types, a coroutine that never yields can be generic over all `Y`
//! yield types, and a coroutine that never returns can be generic over all `R`
//! return types.
//!
//! When you use `just_yield()` to make a coroutine, you can inject that
//! coroutine into any function that takes a coroutine with any return type,
//! or return it from a function that uses `impl Coro` return type syntax with
//! any return type.
//!
//! This does mean that type annotations need to be provided in some places that
//! otherwise wouldn't need them if there were `Yield` and `Return` associated
//! types. You'll see `.yields::<Void>()` and `.returns::<Void>()` a lot in
//! examples to provide required type annotations for code snippets contained
//! within a single function's body. But in real code, you will likely be
//! returning or consuming coroutines through functions that have `impl Coro`
//! bounds that specify the expected yield, return, and input types, which
//! guide the type checker to select one particular implementation of the `Coro`
//! trait out of infinitely many.

// Coroutines should not rely on any allocators or system calls.
//
// Tests for integrations with standard library APIs that use these features
// should be put in the integratation tests in the `tests/` folder.
#![no_std]

mod compose;
mod contramap_input;
mod coro;
mod either;
mod feed_with;
mod fixed_point;
mod flatten;
pub mod from_control_flow;
mod from_fn;
mod into_coro;
mod join;
mod just_return;
mod just_yield;
mod map_return;
mod map_yield;
mod metaprogramming;
mod recursive;
mod return_with;
mod suspend;
mod suspended;
mod void;
mod weave;
mod with_state;
mod yield_with;
mod zip;

pub use coro::Coro;
pub use fixed_point::FixedPointCoro;
pub use from_control_flow::from_control_flow;
pub use from_fn::from_fn;
pub use into_coro::IntoCoro;
pub use join::join;
pub use just_return::Returned;
pub use just_return::just_return;
pub use just_yield::Yielded;
pub use just_yield::just_yield;
pub use recursive::recursive;
pub use return_with::return_with;
pub use suspend::Suspend;
pub use suspended::Suspended;
pub use void::Void;
pub use weave::{WeaveConsumer, weave, weave_cps};
pub use with_state::with_state;
pub use yield_with::yield_with;

/// `Yield` and `Return` are imported into the crate root namespace because
/// they are used so often. Do not confuse these enum variants with the
/// `Yielded` and `Returned` structs.
pub use Suspend::{Return, Yield};

#[cfg(test)]
mod test;
