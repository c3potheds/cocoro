use crate::flatten::FlattenImpl;
use crate::map_return::MapReturn;
use crate::map_yield::MapYield;
use crate::try_map_yield::TryMapYield;
use crate::Suspended;
use crate::Void;
use crate::compose::Compose;
use either::Either;
use std::ops::ControlFlow;
use Suspended::Return;
use Suspended::Yield;

/// A coroutine that can be resumed with an input value of type `I`, returning
/// a suspended state that either "yields" a value of type `Y` or "returns" a
/// a value of type `R`.
///
/// Unlike a `std::ops::Coroutine`, the Rust standard library's coroutine
/// trait, this trait's `resume()` method consumes the coroutine, and gives the
/// caller a new coroutine instance to continue `resume()`ing *only* if the
/// coroutine yielded a value. If the coroutine returned instead, no new
/// coroutine is provided back to the caller, so it is impossible to continue
/// the coroutine after it has returned.
///
/// Contrast to something like `Iterator`, which lets you call `next()` as many
/// times as you want, even after it has returned `None`. Implementors of
/// `Iterator`s are expected to return `None` every time after they have once
/// returned `None`, but there is no way to enforce this in the type system.
/// Meanwhile, consumers of `Iterator`s tend not to test for cases where the
/// iterators they are given violate this contract. This can lead to subtle bugs
/// that are difficult to track down.
///
/// The `std::iter` library provides a `fused()` combinator and a
/// `FusedIterator` trait to attempt to mitigate ethese risks, but there is no
/// substitute for type safety. The `Coro` trait is an experiment in using move
/// semantics to enforce the contracts implicit in the design of `Coroutine` and
/// similar traits like `Iterator` and `Future`.
///
/// `cocoro` coroutines tend to be constructed by either implementing `Coro` for
/// a type, or, more commonly, by using functions like `yield_with()` or
/// `just_yield()` and chaining combinators like `map_yield()` and
/// `map_return()`.
pub trait Coro<Y, R, I = ()>: Sized {
    /// The next state of the coroutine after a call to `resume()`. Often, this
    /// is `Self`, but it is possible for the next state in a `Coro` after
    /// yielding to be a different type altogether. The only constraint is that
    /// the next state must implement the `Coro` trait for the same input and
    /// output types.
    ///
    /// This allows you to create a sort of linked list of coroutine states,
    /// with the starting state at the head. The `resume()` method of each state
    /// advances the coroutine to the next state, until finally reaching a state
    /// that suspends with `Return` when `resume()`d.
    ///
    /// When `Next` is set to `Self`, the function that calls `resume()` can use
    /// a mutable variable to hold the coroutine state, setting it to the state
    /// yielded from the `Yield` variant. When `Next` is set to a different
    /// type, this is not possible, and the function must use a recursive
    /// function that is generic over the `Next` type to continue calling the
    /// coroutine.
    ///
    /// The `Void` type is a special case of `Next` that indicates that the
    /// coroutine will never yield again. A `Coro` whose `Next` is set to `Void`
    /// *must* return and *cannot* yield, simply because it's not possible to
    /// instantiate the `Void` type.
    ///
    /// Note that `Void` will likely be replaced by `!` when that type is
    /// stabilized in Rust.
    type Next: Coro<Y, R, I>;

    /// Advances the coroutine to the next state, returning a suspended state
    /// that either "yields" a value of type `Y` or "returns" a value of type
    /// `R`.
    fn resume(self, input: I) -> Suspended<Y, R, Self::Next>;

    fn yields<Y2>(self) -> impl Coro<Y2, R, I>
    where
        Y2: From<Y>,
    {
        self.map_yield(Y2::from)
    }

    /// Fixes the return type to a specific type.
    ///
    /// Many methods of instantiating coroutines are generic over the return
    /// type, for example `yield_with()` and `just_yield()`, which are
    /// compatible with any return type because they never return. However, when
    /// using other combinators on coroutines that are generic over the return
    /// type, the compiler often isn't able to infer the return type and asks
    /// you to make it explicit.
    ///
    /// The primary function of this method is an ergonomic way to do just that.
    /// The most common use is to call it on a coroutine returned by a function
    /// like `yield_with()` with a type parameter specified by `::<()>`, which
    /// tells the compiler to pretend it returns with `()` as a value.
    ///
    /// This will be less necessary when the
    /// [`!`](https://doc.rust-lang.org/std/primitive.never.html) type is
    /// stabilized in Rust, as it can be a sane (and correct) default for the
    /// return type of coroutines that never return.
    fn returns<R2>(self) -> impl Coro<Y, R2, I>
    where
        R2: From<R>,
    {
        self.map_return(R2::from)
    }

    /// Calls the provided closure on each element *yielded* from this
    /// coroutine. The coroutine returned will yield the results of the closure.
    ///
    /// Compare to [`std::iter::Iterator::map()`](
    ///     https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.map
    /// ).
    ///
    /// Note that this is *lazy*, meaning that the closure does not execute
    /// until the coroutine is resumed. This is similar to how `Iterator::map()`
    /// works.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::{Coro, yield_with};
    ///
    /// let mut i = 0;
    /// yield_with(|()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<()>()
    /// .map_yield(|i| i * 2)
    /// .assert_yields(2, ())
    /// .assert_yields(4, ())
    /// .assert_yields(6, ());
    /// ```
    fn map_yield<Y2, F>(self, f: F) -> MapYield<Y, Self, F>
    where
        F: FnMut(Y) -> Y2,
    {
        MapYield::new(self, f)
    }

    /// Calls the provided closure on the *return value* of this coroutine. The
    /// coroutine provided will return the result of the closure.
    ///
    /// Because coroutines can only return once, the closure type is `FnOnce`.
    /// This allows you to do things in the closure that can only be done once,
    /// like free a resource even resume another coroutine.
    ///
    /// Compare to [`std::iter::Iterator::map()`](
    ///    https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.map
    /// ).
    ///
    /// Note that this is *lazy*, meaning that the closure does not execute
    /// until the coroutine is resumed. This is similar to how `Iterator::map()`
    /// works.
    ///
    /// It's possible to map the return value of a coroutine to a new coroutine
    /// and call `flatten()` on the result to get a coroutine that "chains" the
    /// two coroutines together. However, it's best to use the `flat_map()`
    /// combinator for this directly instead.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::{Coro, just_return};
    ///
    /// just_return(10)
    ///    .yields::<()>()
    ///    .map_return(|x| x * 2)
    ///    .assert_returns(20, ());
    /// ```
    fn map_return<R2, F>(self, f: F) -> MapReturn<R, Self, F>
    where
        F: FnOnce(R) -> R2,
    {
        MapReturn::new(self, f)
    }

    /// Flattens a coroutine that returns a coroutine into a single coroutine.
    /// 
    /// This is useful when you have a coroutine that returns another coroutine,
    /// and you want to treat the two coroutines as a single coroutine. This is
    /// similar to how `Option<Option<T>>` can be flattened into `Option<T>`.
    /// 
    /// You can "chain" coroutines together by having a coroutine return another
    /// and then calling `flatten()` on the result.
    /// 
    /// In cases where the first coroutine doesn't already return the second,
    /// you can use `flat_map()` to map the return value of the first coroutine
    /// to the second coroutine, and get a single coroutine that chains the two
    /// together.
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use cocoro::{Coro, just_return, yield_with};
    /// 
    /// just_return(yield_with(|()| 10).returns::<()>())
    ///     .flatten()
    ///     .assert_yields(10, ())
    ///     .assert_yields(10, ())
    ///     .assert_yields(10, ());
    /// ```
    fn flatten<R2>(self) -> FlattenImpl<Y, Self, R>
    where
        I: Copy,
        R: Coro<Y, R2, I>,
    {
        FlattenImpl::new(self)
    }

    /// Maps the return value of this coroutine to a new coroutine using the
    /// provided function, and returns a combined coroutine that appends the
    /// neew coroutine to this coroutine's yielded values.
    /// 
    /// This is the recommended way to extend a coroutine with another
    /// coroutine.
    /// 
    /// ```rust
    /// 
    /// ```
    fn flat_map<R2, K2, F>(self, f: F) -> impl Coro<Y, R2, I>
    where
        I: Copy,
        F: FnMut(R) -> K2,
        K2: Coro<Y, R2, I>,
    {
        self.map_return(f).flatten()
    }

    /// Extracts the next yielded value from the coroutine and asserts that it
    /// is equal to the expected value. Panics if the coroutine returns instead
    /// of yielding or if the yielded value is not equal to the expected value.
    ///
    /// This is most useful for testing. Since it returns the next state of the
    /// coroutine after the assertion, it can be chained with other assertions
    /// like so:
    ///
    /// ```rust
    /// use cocoro::{Coro, yield_with};
    ///
    /// let mut i = 0;
    /// yield_with(|()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<()>()
    /// .assert_yields(1, ())
    /// .assert_yields(2, ())
    /// .assert_yields(3, ());
    /// ```
    ///
    /// The `input` parameter is passed to the `resume()` method of the
    /// coroutine to drive it.
    ///
    /// ```rust
    /// use cocoro::{Coro, yield_with};
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .returns::<()>()
    /// .assert_yields(3, "foo")
    /// .assert_yields(6, "bar")
    /// .assert_yields(11, "hello");
    /// ```
    fn assert_yields(self, expected: Y, input: I) -> Self::Next
    where
        Y: PartialEq + std::fmt::Debug,
        R: std::fmt::Debug,
    {
        match self.resume(input) {
            Yield(actual, next) => {
                assert_eq!(
                    actual, expected,
                    "expected Yield({expected:?}), got Yield({actual:?})"
                );
                next
            }
            Return(actual) => {
                panic!("expected Yield{expected:?}, got Return({actual:?})")
            }
        }
    }

    /// Extracts the return value from the coroutine and asserts that it is
    /// equal to the expected value. Panics if the coroutine yields instead of
    /// returning or if the return value is not equal to the expected value.
    ///
    /// This is most useful for testing. This can be useful at the end of a
    /// chain of `assert_yields()` calls to ensure that the coroutine returns
    /// with the expected value at the end.
    ///
    /// The `input` parameter is passed to the `resume()` method of the
    /// coroutine to drive it.
    /// 
    /// ```rust
    /// use cocoro::{Coro, yield_with};
    /// 
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .returns::<()>()
    /// .take(2)
    /// .assert_yields(3, "foo")
    /// .assert_yields(6, "bar")
    /// .assert_returns(None, "hello");
    /// ```
    fn assert_returns(self, expected: R, input: I)
    where
        Y: std::fmt::Debug,
        R: PartialEq + std::fmt::Debug,
    {
        match self.resume(input) {
            Yield(actual, _) => {
                panic!("expected Return({expected:?}), got Yield({actual:?})")
            }
            Return(actual) => {
                assert_eq!(
                    actual, expected,
                    "expected Return({expected:?}), got Return({actual:?})"
                );
            }
        }
    }

    /// Executes the last step of the coroutine, returning the return value.
    ///
    /// This will never panic because it is only invocable on a coroutine that
    /// is statically known to never yiel again. This is expressed by setting
    /// the `Next`` associated type to `Void``.
    ///
    /// Compare to [`Result::into_ok()`](
    ///     https://doc.rust-lang.org/std/result/enum.Result.html#method.into_ok
    /// ), which uses the unstable `!` type to establish a similar precondition.
    fn into_return(self, input: I) -> R
    where
        Self: Coro<Y, R, I, Next = Void>,
    {
        match self.resume(input) {
            Return(r) => r,
            Yield(_, _) => unreachable!(),
        }
    }

    /// Creates a coroutine that applies `f` to each value yielded from this
    /// coroutine, and yields the result if `f` returns `ControlFlow::Continue`
    /// and returns with the result if `f` returns `ControlFlow::Break`.
    ///
    /// When `try_trait_v2` is stabilized, expect this method to be extended to
    /// work with closures that return any type that implements `Try`.
    fn try_map_yield<Y2, F>(self, f: F) -> impl Coro<Y2, R, I>
    where
        F: FnMut(Y) -> ControlFlow<R, Y2>,
    {
        TryMapYield::new(self, f)
    }

    /// Creates a coroutine that yields the first `n` elements yielded from this
    /// coroutine, then returns with `None` as the return value. If the original
    /// coroutine yields fewer than `n` elements, the new coroutine will yield
    /// all of them and then return with `Some` containing the return value of
    /// the original coroutine.
    fn take(self, mut n: usize) -> impl Coro<Y, Option<R>, I> {
        self.map_return(Some).try_map_yield(move |y| {
            if n == 0 {
                ControlFlow::Break(None)
            } else {
                n -= 1;
                ControlFlow::Continue(y)
            }
        })
    }

    /// Sends the values yielded from `self` into `other` as inputs, and yields
    /// the results. If `self` returns, the new coroutine will return with the
    /// return value of `Either::Left` containing the return value from `self`,
    /// and if `other` returns, the new coroutine will return with the return
    /// value of `Either::Right` containing the return value from `other`.
    /// 
    /// This kind of treats a coroutine as a function `I -> Y`, and composes it
    /// with a coroutine `Y -> Y2`, to get a coroutine `I -> Y2`, just like when
    /// composing functions. The difference is that when a coroutine "returns",
    /// it has vanished, and values cannot keep getting passed to it, so the
    /// composed coroutine must return if *either* component coroutine returns.
    /// 
    /// Note: if the other coroutine's return value `R2` is the same type as
    /// this coroutine's return value `R`, you can collapse the `Either` into
    /// `R` by using `.map_returns(Either::into_inner)`. If both `R` and `R2`
    /// are convertible into some third type `R3`, you might choose to use
    /// `.map_returns(|e| e.either(R3::from, R3::from))`.
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use cocoro::{Coro, yield_with, just_yield};
    /// 
    /// let mut i = 0;
    /// let mut j = 0;
    /// yield_with(move |()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<()>()
    /// .compose(
    ///     yield_with(|i| {
    ///         j += i;
    ///         j
    ///     })
    ///     .returns::<()>(),
    /// )
    /// .assert_yields(1, ())
    /// .assert_yields(3, ())
    /// .assert_yields(6, ())
    /// .assert_yields(10, ())
    /// .assert_yields(15, ());
    /// ``````
    fn compose<Y2, R2, K2>(self, other: K2) -> impl Coro<Y2, Either<R, R2>, I>
    where
        K2: Coro<Y2, R2, Y>,
    {
        Compose::new(self, other)
    }

    /// Drives this coroutine with default values until it returns, invoking the
    /// given function with each yielded value, and returning the closure's
    /// return value.
    /// 
    /// This will recur infinitely if the coroutine never returns!
    /// 
    /// # Examples
    /// 
    /// ```rust
    /// use cocoro::{Coro, Void, yield_with};
    /// 
    /// let mut i = 0;
    /// let sum = yield_with(move |()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<Void>()
    /// .take(10)
    /// .for_each(|i| {
    ///     println!("{i}");
    /// });
    /// ```
    fn for_each<F>(self, mut f: F) -> R
    where
        I: Default,
        F: FnMut(Y),
    {
        match self.resume(Default::default()) {
            Yield(y, next) => {
                f(y);
                next.for_each(f)
            }
            Return(r) => r,
        }
    }
}
