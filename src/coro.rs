use Suspend::Return;
use Suspend::Yield;

use crate::compose::Compose;
use crate::contramap_input::ContramapInput;
use crate::fixed_point::FixedPointCoro;
use crate::flatten::FlattenImpl;
use crate::map_return::MapReturn;
use crate::map_yield::MapYield;
use crate::metaprogramming::Is;
use crate::suspend::Suspend;
use crate::suspended::Suspended;
use crate::void::Void;
use crate::zip::Zip;

/// A coroutine that can be resumed with an input value of type `I`, returning
/// a suspended state that either "yields" a value of type `Y` or "returns" a
/// value of type `R`.
///
/// Unlike `std::ops::Coroutine`, the Rust standard library's coroutine
/// trait, this trait's `resume()` method consumes the coroutine. It gives the
/// caller a new coroutine instance to continue `resume()`-ing *only* if the
/// coroutine yielded a value. If the coroutine returned instead, no new
/// coroutine is provided back to the caller, so it is impossible to continue
/// the coroutine after it has returned.
///
/// Contrast this with `Iterator`, which lets you call `next()` as many
/// times as you want, even after it has returned `None`. Implementors of
/// `Iterator` are expected to return `None` every time after they have once
/// returned `None`, but there is no way to enforce this in the type system.
/// Meanwhile, consumers of `Iterator`s tend not to test for cases where the
/// iterators they are given violate this contract. This can lead to subtle bugs
/// that are difficult to track down.
///
/// The `std::iter` library provides the `Iterator::fuse()` method and the
/// `FusedIterator` trait to attempt to mitigate these risks, but there is no
/// substitute for type safety. The `Coro` trait is an experiment in using move
/// semantics to enforce the contracts implicit in the design of `Coroutine` and
/// similar traits like `Iterator` and `Future`.
///
/// Note that with this design, we don't need to use `Pin` at all, the way that
/// `std::ops::Coroutine` and `std::future::Future` do. `Coro`
/// implementations are not constructed with language-native features like
/// generator or async blocks. Instead, this library provides ways to create
/// rich, expressive coroutines using normal closures and combinators,
/// avoiding the need for self-referential data structures.
///
/// `cocoro` coroutines tend to be constructed by either implementing `Coro` for
/// a type, or, more commonly, by using functions like `yield_with()` or
/// `just_yield()` and chaining combinators like `map_yield()` and
/// `map_return()`.
pub trait Coro<I, Y, R>: Sized {
    /// The next state of the coroutine after a call to `resume()`, if the
    /// coroutine yields a value.
    ///
    /// If the coroutine can only return, the `Next` associated type may be
    /// `Void`.
    ///
    /// It is possible, and common, for `Next` to be `Self`
    /// if the coroutine's full state machine can be represented by a single
    /// type. When `Next` is `Self`, the coroutine is a
    /// "fixed-point coroutine" and automatically implements the `FixedPointCoro`
    /// subtrait.
    type Next: Coro<I, Y, R>;

    /// The type of the suspended state of the coroutine, which is either a
    /// "yield" state with a value of type `Y` and the next state of the
    /// coroutine, or a "return" state with a value of type `R`.
    ///
    /// Usually, this is `Suspend<Y, R, Self::Next>`, but it can be any type
    /// that implements the `Suspended` trait. This allows returning concrete
    /// types generic over yield or return types. For example, `just_return()`
    /// uses the `Returned` type, which is generic over the yield type because
    /// it's statically known to never yield.
    type Suspend: Suspended<I, Y, R, Next = Self::Next>;

    /// Advances the coroutine to the next state, returning a suspended state
    /// that either "yields" a value of type `Y` or "returns" a value of type
    /// `R`.
    fn resume(self, input: I) -> Self::Suspend;

    /// Fixes the yield type to a specific type.
    ///
    /// This is useful when the compiler is unable to infer the yield type of a
    /// coroutine, which can happen when using things like `just_return()`,
    /// which are generic over the yield type. In such cases, you can call this
    /// method with a type parameter specified by `::<Y>()`, which tells the
    /// compiler to pretend the coroutine yields values of type `Y`.
    ///
    /// This will be less necessary when the
    /// [`!`](https://doc.rust-lang.org/std/primitive.never.html) type is
    /// stabilized in Rust, as it can be a sane (and correct) default for the
    /// yield type of coroutines that never yield.
    ///
    /// In the meantime, you can use the `Void` type as the yield type of
    /// coroutines that are known to never yield. You can even use this as a
    /// constraint on input parameters to functions that take coroutines, to
    /// ensure that the coroutines passed to them never yield (and will only
    /// ever return when suspended).
    ///
    /// This has no runtime overhead; the result of this function is necessarily
    /// the same as `self`, just reinterpreted as a particular `Coro`
    /// implementation that has a specific yield type.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::just_return;
    ///
    /// just_return(10).yields::<Void>().assert_returns(10, ());
    /// ```
    fn yields<Y2>(
        self,
    ) -> impl Coro<I, Y, R, Next = Self::Next, Suspend = Self::Suspend>
    where
        Y2: Is<Type = Y>,
    {
        self
    }

    /// Fixes the return type to a specific type.
    ///
    /// Many methods of instantiating coroutines are generic over the return
    /// type, for example `yield_with()` and `just_yield()`, which are
    /// compatible with any return type because they never return. In other
    /// words, the coroutines returned by these functions implement
    /// `Coro<I, Y, R>` for *infinitely many* types `R`. When you try to use
    /// these coroutines in a way that doesn't allow the compiler to infer
    /// *which* `R` type to interpret the coroutine as, you can get a compiler
    /// error. When using generic combinators on generic coroutines that are
    /// generic over the return type, the compiler often isn't able to infer the
    /// return type and asks you to make it explicit.
    ///
    /// If you get a "type annotations needed" error involving the return
    /// type `R`, you likely need to specify it explicitly.
    ///
    /// This method provides an ergonomic way to do that.
    /// The most common use is to call it on a coroutine returned by a function
    /// like `yield_with()` with a type parameter specified by `::<()>`, which
    /// tells the compiler to pretend it returns with `()` as a value.
    ///
    /// This will be less necessary when the
    /// [`!`](https://doc.rust-lang.org/std/primitive.never.html) type is
    /// stabilized in Rust, as it can be a sane (and correct) default for the
    /// return type of coroutines that never return.
    ///
    /// In the meantime, you can use the `Void` type as the return type of
    /// coroutines that are known to never return. This has the benefit of
    /// enforcing at compile time that the coroutine indeed never returns,
    /// because the only possible implementation of a coroutine that returns
    /// `Void` is one that is generic over all return types (because `Void` is
    /// not instantiable), and a coroutine that is generic over all return types
    /// must never return because there is no way for a concrete type to
    /// generically create an instance of any type.
    ///
    /// This has no runtime overhead; the result of this function is necessarily
    /// the same as `self`, just reinterpreted as a particular `Coro`
    /// implementation that has a specific return type.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// yield_with(|_| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<Void>()
    /// .assert_yields(1, ())
    /// .assert_yields(2, ())
    /// .assert_yields(3, ());
    /// ```
    fn returns<R2>(self) -> impl Coro<I, Y, R>
    where
        R2: Is<Type = R>,
    {
        self
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
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// yield_with(|()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<Void>()
    /// .map_yield(|i| i * 2)
    /// .returns::<Void>()
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
    /// resulting coroutine will return the result of the closure.
    ///
    /// Because coroutines can only return once, the closure type is `FnOnce`.
    /// This allows you to do things in the closure that can only be done once,
    /// like freeing a resource or resuming another coroutine.
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
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::just_return;
    ///
    /// just_return(10)
    ///     .yields::<Void>()
    ///     .map_return(|x| x * 2)
    ///     .assert_returns(20, ());
    /// ```
    fn map_return<R2, F>(self, f: F) -> MapReturn<R, Self, F>
    where
        F: FnOnce(R) -> R2,
    {
        MapReturn::new(self, f)
    }

    /// Changes the input type of the coroutine with a function that outputs the
    /// input type of this coroutine and takes as input some other type.
    ///
    /// Unlike `map_yield()` and `map_return()`, this combinator changes an
    /// *input* type rather than an output type, and therefore it's the
    /// function's *output* type that needs to match this coroutine's input
    /// type.
    ///
    /// One use case is converting a coroutine requiring an input
    /// type, say `&str`, to one accepting `()`, which
    /// enables algorithms like `for_each()`. Just provide a function that
    /// takes `()` as input and returns some `&str` to pass to the original
    /// coroutine.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let words = ["foo", "bar", "baz"];
    /// let mut words_iter = words.iter().copied().cycle();
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .returns::<Void>()
    /// .contramap_input(|()| words_iter.next().unwrap())
    /// .assert_yields(3, ())
    /// .assert_yields(6, ())
    /// .assert_yields(9, ())
    /// .assert_yields(12, ())
    /// .assert_yields(15, ())
    /// .assert_yields(18, ());
    /// ```
    fn contramap_input<I2, F>(self, f: F) -> impl Coro<I2, Y, R>
    where
        F: FnMut(I2) -> I,
    {
        ContramapInput::new(self, f)
    }

    /// Flattens a coroutine that returns a coroutine into a single coroutine.
    ///
    /// This is useful when you have a coroutine that returns another coroutine,
    /// and you want to treat the two coroutines as a single coroutine. This is
    /// similar to how `Option<Option<T>>` can be flattened into `Option<T>`.
    ///
    /// You can "chain" coroutines by having one return another
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
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::just_return;
    /// use cocoro::yield_with;
    ///
    /// just_return(yield_with(|()| 10))
    ///     .flatten()
    ///     .assert_yields(10, ())
    ///     .assert_yields(10, ())
    ///     .assert_yields(10, ())
    ///     .returns::<Void>();
    /// ```
    fn flatten<R2>(self) -> impl Coro<I, Y, R2>
    where
        I: Copy,
        R: Coro<I, Y, R2>,
    {
        FlattenImpl::new(self)
    }

    /// Maps the return value of this coroutine to a new coroutine using the
    /// provided function, and returns a combined coroutine that appends the
    /// new coroutine to this coroutine's yielded values.
    ///
    /// This is the recommended way to extend a coroutine with another
    /// coroutine.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::take;
    /// use cocoro::yield_with;
    ///
    /// fn iota<R>() -> impl Coro<(), i32, R> {
    ///     let mut i = 0;
    ///     yield_with(move |()| {
    ///         i += 1;
    ///         i
    ///     })
    /// }
    ///
    /// let count_to_three_twice = iota()
    ///     .compose(take(3))
    ///     .flat_map(|_| iota().compose(take(3)));
    /// count_to_three_twice
    ///     .assert_yields(1, ())
    ///     .assert_yields(2, ())
    ///     .assert_yields(3, ())
    ///     .assert_yields(1, ())
    ///     .assert_yields(2, ())
    ///     .assert_yields(3, ())
    ///     .assert_returns((), ());
    /// ```
    fn flat_map<R2, K2, F>(self, f: F) -> impl Coro<I, Y, R2>
    where
        I: Copy,
        F: FnMut(R) -> K2,
        K2: Coro<I, Y, R2>,
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
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// yield_with(|()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<Void>()
    /// .assert_yields(1, ())
    /// .assert_yields(2, ())
    /// .assert_yields(3, ());
    /// ```
    ///
    /// The `input` parameter is passed to the `resume()` method of the
    /// coroutine to drive it.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .returns::<Void>()
    /// .assert_yields(3, "foo")
    /// .assert_yields(6, "bar")
    /// .assert_yields(11, "hello");
    /// ```
    fn assert_yields(self, expected: Y, input: I) -> Self::Next
    where
        Y: PartialEq + core::fmt::Debug,
        R: core::fmt::Debug,
    {
        match self.resume(input).into_enum() {
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
    /// use cocoro::Coro;
    /// use cocoro::take;
    /// use cocoro::yield_with;
    ///
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .compose(take(2))
    /// .assert_yields(3, "foo")
    /// .assert_yields(6, "bar")
    /// .assert_returns((), "hello");
    /// ```
    fn assert_returns(self, expected: R, input: I)
    where
        Y: core::fmt::Debug,
        R: PartialEq + core::fmt::Debug,
    {
        match self.resume(input).into_enum() {
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
    /// This method will never panic because it's only callable on a coroutine
    /// that is statically known to never yield again. This is expressed by
    /// setting the `Next` associated type to `Void`.
    ///
    /// Compare to [`Result::into_ok()`](
    ///     https://doc.rust-lang.org/std/result/enum.Result.html#method.into_ok
    /// ), which uses the unstable `!` type to establish a similar precondition.
    fn into_return(self, input: I) -> R
    where
        Self: Coro<I, Y, R, Next = Void>,
    {
        match self.resume(input).into_enum() {
            Return(r) => r,
            Yield(_, _) => unreachable!(),
        }
    }

    /// Sends the values yielded from `self` into `other` as inputs, and yields
    /// the results. If either `self` or `other` returns while resuming the
    /// composed coroutine, the composed coroutine will return. The two
    /// coroutines must have compatible types: the yield type of `self` must be
    /// the input type for `other`, and both `self` and `other` must have the
    /// same return type.
    ///
    /// This kind of treats a coroutine as a function `I -> Y`, and composes it
    /// with a coroutine `Y -> Y2`, to get a coroutine `I -> Y2`, just like when
    /// composing functions. The difference is that when a coroutine "returns",
    /// it has vanished, and values cannot keep getting passed to it, so the
    /// composed coroutine must return if *either* component coroutine returns.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::just_yield;
    /// use cocoro::yield_with;
    ///
    /// fn iota() -> impl Coro<(), i32, Void> {
    ///     let mut i = 0;
    ///     yield_with(move |()| {
    ///         i += 1;
    ///         i
    ///     })
    /// }
    ///
    /// fn sum() -> impl Coro<i32, i32, Void> {
    ///     let mut sum = 0;
    ///     yield_with(move |i| {
    ///         sum += i;
    ///         sum
    ///     })
    /// }
    /// iota()
    ///     .compose(sum())
    ///     .assert_yields(1, ())
    ///     .assert_yields(3, ())
    ///     .assert_yields(6, ())
    ///     .assert_yields(10, ())
    ///     .assert_yields(15, ());
    /// ```
    fn compose<Y2, K2>(self, other: K2) -> Compose<I, Y, Self, K2>
    where
        K2: Coro<Y, Y2, R>,
    {
        Compose::new(self, other)
    }

    /// Creates a coroutine that repeatedly processes this source coroutine by
    /// weaving it with the yielded values from the "processor" coroutine, which
    /// yields "workers" that interact with this coroutine to produce values to
    /// yield from the resulting coroutine.
    ///
    /// For each input to the resulting coroutine, it:
    /// 1. Resumes the processor coroutine to start a worker, or short-circuit
    ///    and end processing
    /// 2. If a worker was started, use the initial input value and the worker
    ///    to `weave()` with this source coroutine
    /// 3. Yields the result if the worker coroutine returns, or passes the
    ///    and the current state of the worker coroutine to the continuation if
    ///    the source coroutine returns to determine what to do with the state
    ///    of the interrupted worker
    /// 4. The continuation provides a new coroutine for this coroutine to
    ///    "become", superseding the above behavior after the source coroutine
    ///    is finished
    ///
    /// This enables powerful patterns like chunking, filtering, and "take many,
    /// yield one" operations in a principled, generic way.
    ///
    /// The processor coroutine yields worker coroutines, so it can be thought
    /// of as a coroutine that yields coroutines. However, note that instead of
    /// yielding coroutines directly, it yields a *suspension* of the worker
    /// coroutine, which is either a `Yield` value containing the initial input
    /// to the source coroutine and the worker, or a `Return` value containing
    /// the `X` value to yield from the result coroutine without interacting
    /// with the source coroutine.
    ///
    /// The processor's input type `A` is the input type of the result coroutine
    /// and can be used as a parameter to customize the worker for a chunk of
    /// yield values from the coroutine. A processor may decide, based on the
    /// input, to skip reading the source, in which case it can yield a returned
    /// `X` value instead of yielding a yielded `I` with a worker, and that `X`
    /// value will be yielded from the result coroutine directly. A processor
    /// could also decide to return an `R` directly, completely short-circuiting
    /// the entire result coroutine. Or, in the most common case, it chooses an
    /// initial input for the source coroutine, creates a worker coroutine, and
    /// yields a `Yield` with that input and worker.
    ///
    /// There are two ways for the result coroutine to return:
    ///
    /// 1. The source coroutine returns. This only happens while weaving with a
    ///    worker coroutine, so the return value from the source and the state
    ///    of the worker is passed to the continuation, which determines how to
    ///    proceed (e.g. yield anything from the state of the worker before
    ///    returning, or combining the return value with the state of the worker
    ///    to produce a new return value).
    /// 2. The processor returns directly, instead of spawning a worker. In this
    ///    case the entire coroutine is short-circuited. If this happens, the
    ///    continuation is not invoked, because there is no leftover worker that
    ///    may contain anything else left to yield.
    ///
    /// The continuation is used to transition from consuming the source
    /// coroutine to processing the leftover intermediate state of the last
    /// worker. For example, you can use `process()` to process a stream of
    /// tokens (e.g. `Coro<(), Token, EOF>`) with workers that accumulate
    /// expression trees (e.g. `Coro<Token, (), Result<Ast, ParseError>>`),
    /// and an `EOF` may need to be handled by the continuation depending on
    /// what state the worker, which might be in a state with unmatched
    /// delimiters (which may cause a parse error), or yield the expression from
    /// the final tokens.
    ///
    /// # Example
    ///
    /// ```rust
    /// use core::ops::ControlFlow::*;
    ///
    /// use cocoro::Coro;
    /// use cocoro::FixedPointCoro;
    /// use cocoro::IntoCoro;
    /// use cocoro::Returned;
    /// use cocoro::Yielded;
    /// use cocoro::from_control_flow;
    /// use cocoro::yield_with;
    ///
    /// // Filtering example: collect items until we find one matching a
    /// // predicate.
    /// fn filter_until<T>(
    ///     predicate: impl Fn(&T) -> bool,
    /// ) -> impl FixedPointCoro<T, (), T> {
    ///     from_control_flow(move |input| {
    ///         if predicate(&input) {
    ///             Break(input)
    ///         } else {
    ///             Continue(())
    ///         }
    ///     })
    /// }
    ///
    /// /// Helper to create a simple processor that yields a worker for each input
    /// fn process_simple<I: Default, A, W, R>(
    ///     mut f: impl FnMut(A) -> W,
    /// ) -> impl Coro<A, Yielded<I, W>, R> {
    ///     yield_with(move |a| Yielded(Default::default(), f(a)))
    /// }
    ///
    /// let filtered = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].into_coro().process(
    ///     process_simple(|max| filter_until(move |&x| x > max)),
    ///     // No need to yield anything from the worker; filter_until() is
    ///     // stateless.
    ///     |r: (), _| Returned(r),
    /// );
    /// filtered
    ///     .assert_yields(6, 5) // First number > 5
    ///     .assert_yields(8, 7) // First number > 7
    ///     .assert_yields(10, 9); // First number > 9
    /// ```
    ///
    /// For now, the source coroutine and the workers must be `FixedPointCoro`,
    /// but the result is not necessarily `FixedPointCoro`. The source and
    /// worker coroutines need to be `FixedPointCoro` for `weave()` to work, and
    /// the `weave_cps()` function doesn't conserve the type of the worker
    /// coroutine which needs to be passed to `cont`.
    ///
    /// It may be possible to guarantee that the result coroutine is fixed-point
    /// if and only if the processor and `Finish` coroutines are also
    /// fixed-point, but for now this is kept hidden from the type system.
    fn process<A, X, Start, Worker, Transition>(
        self,
        processor: impl Coro<A, Start, R>,
        cont: impl FnOnce(R, Worker) -> Transition,
    ) -> impl Coro<A, X, R>
    where
        Self: FixedPointCoro<I, Y, R>,
        Worker: FixedPointCoro<Y, I, X>,
        Start: Suspended<Y, I, X, Next = Worker>,
        Transition: Suspended<A, X, R>,
    {
        crate::process::process(self, processor, cont)
    }

    /// Drive the coroutine to completion, using the `driver` function to choose
    /// the next input based on the yielded output. This will loop forever (and
    /// probably overflow the stack) if the source coroutine yields infinitely.
    ///
    /// This is a more general version of `for_each()` that works for coroutines
    /// with input parameters that don't implement `Default`.
    ///
    /// # Example 1: perform an action for each yielded value
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::IntoCoro;
    ///
    /// [1, 2, 3, 4, 5]
    ///     .into_iter()
    ///     .into_coro()
    ///     .drive((), |n| println!("{n}")); // Prints "1", "2", "3", "4", "5"
    /// ```
    ///
    /// # Example 2: read a a buffer to completion
    ///
    /// ```rust
    /// use core::ops::ControlFlow::*;
    ///
    /// use cocoro::Coro;
    /// use cocoro::from_control_flow;
    ///
    /// // A coroutine whose input determines how many bytes to read from a
    /// // buffer, and yields successive slices of the buffer based on the given
    /// // sizes, until finally returning the last slice of the buffer.
    /// fn chunkify(buffer: &[u8]) -> impl Coro<usize, &[u8], &[u8]> {
    ///     let mut cursor = 0usize;
    ///     let max = buffer.len();
    ///     from_control_flow(move |bytes_to_read| {
    ///         let prev = cursor;
    ///         cursor += bytes_to_read;
    ///         if cursor < max {
    ///             Break(&buffer[prev..max])
    ///         } else {
    ///             Continue(&buffer[prev..cursor])
    ///         }
    ///     })
    /// }
    ///
    /// chunkify("hello, world".as_bytes()).drive(5, |chunk| {
    ///     println!("{chunk:?}");
    ///     if chunk.contains(&b',') { 5 } else { 2 }
    /// });
    /// ```
    fn drive(self, input: I, mut driver: impl FnMut(Y) -> I) -> R {
        match self.resume(input).into_enum() {
            Yield(y, n) => n.drive(driver(y), driver),
            Return(r) => r,
        }
    }

    /// Drives this coroutine with default input values until it returns. It
    /// invokes the given function `f` for each yielded value. Finally, it
    /// returns the return value `R` of the original coroutine.
    ///
    /// This will recur infinitely if the coroutine never returns!
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::take;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// let sum = yield_with(move |()| {
    ///     i += 1;
    ///     i
    /// })
    /// .compose(take(52142))
    /// .for_each(|i| {
    ///     println!("{i}");
    /// });
    /// ```
    fn for_each<F>(self, mut f: F) -> R
    where
        I: Default,
        F: FnMut(Y),
    {
        self.drive(Default::default(), |y| {
            f(y);
            Default::default()
        })
    }

    /// Zips the yielded values of this coroutine with the yielded values of
    /// `other`, yielding pairs of values from pairs of inputs until either
    /// coroutine returns.
    ///
    /// This is similar to the `zip()` method on iterators, but it works with
    /// coroutines that yield values instead of iterators that produce values.
    ///
    /// The return type `R` of both coroutines must be the same, and the return
    /// value of the composed coroutine will be the return value of the first
    /// coroutine to return.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// let mut j = 0;
    /// yield_with(move |()| {
    ///     i += 1;
    ///     i
    /// })
    /// .zip(yield_with(move |()| {
    ///     j += 2;
    ///     j
    /// }))
    /// .returns::<Void>()
    /// .assert_yields((1, 2), ((), ()))
    /// .assert_yields((2, 4), ((), ()))
    /// .assert_yields((3, 6), ((), ()));
    /// ```
    ///
    /// Note that the input type of the zipped coroutine is a tuple of the
    /// input types of the two coroutines being zipped, which is why we need to
    /// pass `((), ()` as the input. There are a couple of easy techniques to
    /// simplify this:
    ///
    /// 1. Use `Default::default()` as the input, if the input type is `Default`.
    /// 2. Use `contramap_input()` to change the input type.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// let mut j = 0;
    /// yield_with(move |()| {
    ///     i += 1;
    ///     i
    /// })
    /// .zip(yield_with(move |()| {
    ///     j += 2;
    ///     j
    /// }))
    /// // Contramap the tuple of inputs ((), ()) to just ().
    /// .contramap_input(|()| ((), ()))
    /// .returns::<Void>()
    /// .assert_yields((1, 2), ())
    /// .assert_yields((2, 4), ())
    /// .assert_yields((3, 6), ());
    /// ```
    fn zip<I2, Y2>(
        self,
        other: impl Coro<I2, Y2, R>,
    ) -> impl Coro<(I, I2), (Y, Y2), R> {
        Zip::new(self, other)
    }

    /// Drives this coroutine by resuming first with the initial value, and then
    /// with the value yielded by the coroutine, until the coroutine returns.
    ///
    /// This is called "bootstrap" because it starts the coroutine from just a
    /// single value and drives it to completion using the coroutine's own
    /// yielded values. This is a reference to the idea of "pulling oneself up
    /// by one's bootstraps." The idea is that the coroutine is able to drive
    /// itself to completion using its own yielded values, rather than needing
    /// an external driver.
    ///
    /// Of course, this is not possible without an initial value to start the
    /// process. But once the coroutine has yielded a value, it can be resumed
    /// with that value, yield a new value, resume with that, and so on, until
    /// it returns.
    ///
    /// This can only be called on coroutines that yield the same type that they
    /// take as input.
    ///
    /// If this is called on a coroutine that never returns, then this will
    /// recur infinitely.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::Return;
    /// use cocoro::Void;
    /// use cocoro::Yield;
    /// use cocoro::recursive;
    ///
    /// let count_to_10 = recursive(&|rec, i| {
    ///     if i < 10 { Yield(i + 1, rec) } else { Return(i) }
    /// });
    /// let count = count_to_10.bootstrap(0);
    /// assert_eq!(count, 10);
    /// ```
    fn bootstrap(self, init: Y) -> R
    where
        Self: Coro<Y, Y, R>,
    {
        self.drive(init, |y| y)
    }

    /// Creates a lazy, in-place iterator over the values yielded by this
    /// coroutine.
    ///
    /// In order for the iterator type to be the same for each state of the
    /// coroutine, the coroutine must be a fixed-point coroutine (i.e.,
    /// `Self::Next` is `Self`).
    ///
    /// TODO: it should be possible to implement `into_iter()` for all
    /// coroutines, not just fixed-point ones. The idea would be to use an enum
    /// to track whether we need to store the first coroutine or an iterator
    /// over its successor after it suspended the first time, and recur. The
    /// problem is figuring out how to handle fixed-point coroutines differently
    /// from non-fixed-point coroutines, because fixed-point coroutines would
    /// recur infinitely and overflow the type checker, whereas non-fixed-point
    /// coroutines are necessarily finite, with `Next` chains that end either in
    /// `Void` or a fixed point coroutine.
    fn into_iter(self) -> impl Iterator<Item = Y>
    where
        I: Default,
        Self: FixedPointCoro<I, Y, R> + Sized,
    {
        let mut src = Some(self);
        core::iter::from_fn(move || match src.take() {
            Some(coro) => match coro.resume(Default::default()).into_enum() {
                Yield(y, next) => {
                    src = Some(next);
                    Some(y)
                }
                Return(_) => {
                    src = None;
                    None
                }
            },
            None => None,
        })
    }

    /// Combines two coroutines with the same input and yield types by
    /// interleaving their yielded values, alternating which coroutine is
    /// delegated to each time the composed coroutine is resumed. If one
    /// coroutine returns, the composed coroutine will yield the remaining
    /// values from the other coroutine, then return with the return values of
    /// both coroutines as a tuple.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::IntoCoro;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let a = ["a1", "a2"].into_coro().map_return(|()| "A");
    /// let b = ["b1", "b2", "b3"].into_coro().map_return(|()| "B");
    /// a.join(b)
    ///     .assert_yields("a1", ())
    ///     .assert_yields("b1", ())
    ///     .assert_yields("a2", ())
    ///     .assert_yields("b2", ())
    ///     .assert_yields("b3", ())
    ///     .assert_returns(("A", "B"), ());
    /// ```
    fn join<R2>(self, other: impl Coro<I, Y, R2>) -> impl Coro<I, Y, (R, R2)>
    where
        I: Copy,
    {
        crate::join::join(self, other)
    }
}
