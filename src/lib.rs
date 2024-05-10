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
//! pub enum Suspended<Y, R, N> {
//!     Yield(Y, N),
//!     Return(R),
//! }
//!
//! pub trait Coro<Y, R, I = ()>: Sized {
//!     type Next: Coro<Y, R, I>;
//!     fn resume(self, input: I) -> Suspended<Y, R, Self::Next>;
//! }
//! ```
//!
//! Note the following differences from `std::ops::Coroutine`:
//!
//!   * The `resume` method takes `self` by value, not by a pinned exclusive
//!     reference.
//!   * The types for "yield" and "return" are generic parameters rather than
//!     associated types.
//!   * The `resume` method returns a `Suspended` enum that wraps the state of
//!     the coroutine, exposing it to be resumed again *only* if it `Yield`ed.
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
//! use cocoro::{Coro, Suspended, Yield};
//!
//! struct Counter(i32);
//! impl Coro<i32, (), ()> for Counter {
//!     type Next = Self;
//!     fn resume(self, _: ()) -> Suspended<i32, (), Self> {
//!         Yield(self.0, Counter(self.0 + 1))
//!     }
//! }
//!
//! let mut counter = Counter(0);
//! for _ in 0..10 {
//!     let (n, next) = match counter.resume(()) {
//!         Yield(n, next) => (n, next),
//!         _ => unreachable!(),
//!     };
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
//! Using the `yield_with()` function, we can do the same thing with a closure:
//!
//! ```rust
//! use cocoro::{Coro, Yield, yield_with};
//! let mut i = 0;
//! let n = yield_with(|()| {
//!     i += 1;
//!     i
//! })
//! .returns::<()>()
//! .take(10)
//! .for_each(|n| {
//!     println!("{}", n);
//! });
//! ```
//!
//! ## Static-sized countdown
//!
//! ```rust
//! use cocoro::{Coro, Suspended, Yield, Return, Void};
//!
//! struct Three;
//! struct Two;
//! struct One;
//! #[derive(Debug, PartialEq, Eq)]
//! struct Blastoff;
//!
//! impl Coro<i32, Blastoff, ()> for Three {
//!     type Next = Two;
//!     fn resume(self, _: ()) -> Suspended<i32, Blastoff, Self::Next> {
//!         Yield(3, Two)
//!     }
//! }
//!
//! impl Coro<i32, Blastoff, ()> for Two {
//!     type Next = One;
//!     fn resume(self, _: ()) -> Suspended<i32, Blastoff, Self::Next> {
//!         Yield(2, One)
//!     }
//! }
//!
//! impl Coro<i32, Blastoff, ()> for One {
//!     type Next = Blastoff;
//!     fn resume(self, _: ()) -> Suspended<i32, Blastoff, Self::Next> {
//!        Yield(1, Blastoff)
//!     }
//! }
//!
//! impl Coro<i32, Blastoff, ()> for Blastoff {
//!     type Next = Void;
//!     fn resume(self, _: ()) -> Suspended<i32, Blastoff, Void> {
//!         Return(Blastoff)
//!     }
//! }
//!
//! let countdown = Three;
//! let (n, countdown) = countdown.resume(()).as_yield().unwrap();
//! println!("{}", n);
//! let (n, countdown) = countdown.resume(()).as_yield().unwrap();
//! println!("{}", n);
//! let (n, countdown) = countdown.resume(()).as_yield().unwrap();
//! println!("{}", n);
//! let blastoff = countdown.resume(()).as_return().unwrap();
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
//! use cocoro::{Coro, Suspended, Yield, Return, Void};
//! #[derive(Debug, PartialEq, Eq)]
//! struct Blastoff;
//! let countdown = |_| {
//!     Yield::<_, Blastoff, _>(3, |_| {
//!         Yield::<_, Blastoff, _>(2, |_| {
//!             Yield::<_, Blastoff, _>(1, |_| {
//!                 Return::<Void, _, Void>(Blastoff)
//!             })
//!         })
//!     })
//! };
//! let (n, countdown) = countdown(()).as_yield().unwrap();
//! println!("{}", n);
//! let (n, countdown) = countdown(()).as_yield().unwrap();
//! println!("{}", n);
//! let (n, countdown) = countdown(()).as_yield().unwrap();
//! println!("{}", n);
//! let blastoff = countdown(()).as_return().unwrap();
//! println!("{:?}!", blastoff);
//! ```
//!
//! ... but notice that we have to explicitly specify the return type on the
//! yielding steps and the yield/next types on the returning step.
//!
//! # Theory
//!
//! The main motivation of this crate is to show how the Rust type system could
//! provide compile-time correct-by-construction guarantees that a coroutine,
//! or other state machines like `Iterator` and `Future`, can only be used in
//! accordance with the contract that they will never yield again after
//! returning. Most design choices follow from that, including, ultimately, the
//! emphasis on functional combinators, which are helpful to add the missing
//! expressivity that can't come from procedural `gen` blocks, which are the
//! reason for `Pin`, which would have forced a `resume()` method to mutably
//! borrow a (pinned) reference rather than accept `self` by value, which was
//! the only way to "maybe consume" the coroutine, passing it back to the caller
//! on yield but dropping it on return.
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
//! can `contramap()` the input type with a function that takes a different
//! input type and returns the original input type, and  get a new coroutine
//! that uses the `contramap()` function's input type as its input type.
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
//! derived
//!
//! # FAQ
//!
//! ## Why is it called `cocoro`?
//!
//! Github Copilot thinks it's a truncation of "combinator coroutine", which
//! honestly seems appropriate.
//!
//! But i was thinking more along the lines of a pun: "coro" as an abbreviation
//! for "coroutine", "co" as a prefix meaning "together" or "with", and "cocoro"
//! sounding like "kokoro" which is the Japanese word for "heart", with all the
//! attendant connotations of mind, spirit, and core-ness.
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
//! code. These state machines could include references to local variables in
//! the `gen` block, which means the type is self-referential and cannot be
//! moved in memory after the references may have been created. To solve this,
//! generators require a `Pin<&mut Self>` to ensure that the generator is
//! "pinned" in memory and cannot be moved.
//!
//! The `cocoro` crate does not need to support self-referential types, because
//! it does not try to describe the state machines of `gen` blocks. Instead,
//! coroutines are hand-written or composed from combinators.
//!
//! ## Ok, but why not use `&mut self`` as the reciver for `resume()`?
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

mod compose;
mod coro;
mod either;
mod flatten;
mod from_fn;
mod just_return;
mod just_yield;
mod map_return;
mod map_yield;
mod suspended;
mod try_map_yield;
mod void;
mod yield_with;

pub use coro::Coro;
pub use just_return::just_return;
pub use just_yield::just_yield;
pub use suspended::Suspended;
pub use void::Void;
pub use yield_with::yield_with;
pub use Suspended::{Return, Yield};
