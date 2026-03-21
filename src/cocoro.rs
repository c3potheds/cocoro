use crate::coro::Coro;

/// The consumer of a `Suspended` coroutine, which may either receive a yielded
/// value along with the remainder of the coroutine, or the final value returned
/// from the coroutine. It's up to the `Cocoro` implementation to decide what
/// to do next when it receives either one of these cases: it may "continue" the
/// coroutine by calling `resume()` again with a new input value on the `Coro`
/// it received in `on_yield()`, or it might "return early" with some value of
/// its `Out` type.
///
/// This trait can be thought of as a generalization of the `match` expression
/// on a `Suspend` enum, where the `Out` type is the type that the `match`
/// expression evaluates to, and the `on_yield()` and `on_return()`
/// implementations are case handlers for the `Yield` and `Return` variants of
/// the `Suspend` enum.
///
/// In many cases, you may want to simply use normal pattern-matching with a
/// `match` expression on a `Suspend` enum, which can be obtained from any
/// `Suspended` implementation with the `into_enum()` method. But in some cases,
/// it may be worthwhile to return a `Cocoro` instance from a function, or use
/// combinators to transform such instances before using them to consume a
/// suspended coroutine.
///
/// For example, **parsers** of an input stream of some type `I` that ends with
/// some type `End` can be written as an `impl Cocoro<I, (), End, N,
/// Out=Result<T, ParseError>>` where `N` is any input stream type implementing
/// `Coro<(), I, End>`, `T` is the parsed type, and `ParseError` is some
/// representation of a parse error. A parser consumes either the next input of
/// type `I` in `on_yield()` or receives a notification that the stream has
/// terminated in `on_return()`, and either way will return a `Result` that
/// contains the successfully parsed value or an error, either by calling
/// `resume()` on the `next` coroutine it received in `on_yield()` or returning
/// the parsed value.
///
/// In use cases such as parsing, the combinators and transformations may be
/// best applied to `Cocoro` instances that define the building blocks of
/// parsers rather than `Coro` instances that define the input streams being
/// parsed.
pub trait Cocoro<I, Y, R, N>
where
    N: Coro<I, Y, R>,
{
    type Out;
    fn on_yield(self, y: Y, next: N) -> Self::Out;
    fn on_return(self, r: R) -> Self::Out;

    /// Transform the output of this `Cocoro` by applying `f` to whatever
    /// `on_yield` or `on_return` produces.
    ///
    /// This is the primary combinator on `Cocoro`, covariant in `Out`. It
    /// mirrors the role of `map` on `Iterator` or `Result`: it says nothing
    /// about *how* the coroutine is consumed, only about *what is done with
    /// the result*.
    ///
    /// # Example
    ///
    /// ```rust
    /// use cocoro::Cocoro;
    /// use cocoro::Coro;
    /// use cocoro::Suspend;
    /// use cocoro::Suspend::Return;
    /// use cocoro::Suspend::Yield;
    /// use cocoro::Suspended;
    ///
    /// // A minimal stream that yields exactly one char then returns.
    /// // Implementing `Coro` inline gives us a concrete `Next` type,
    /// // which makes `IsDigit.map(...)` unambiguous.
    /// struct OneChar(Option<char>);
    /// impl Coro<(), char, ()> for OneChar {
    ///     type Next = Self;
    ///     type Suspend = Suspend<char, (), Self>;
    ///     fn resume(self, _: ()) -> Self::Suspend {
    ///         match self.0 {
    ///             Some(c) => Yield(c, OneChar(None)),
    ///             None => Return(()),
    ///         }
    ///     }
    /// }
    ///
    /// struct IsDigit;
    /// impl Cocoro<(), char, (), OneChar> for IsDigit {
    ///     type Out = bool;
    ///     fn on_yield(self, c: char, _: OneChar) -> bool {
    ///         c.is_ascii_digit()
    ///     }
    ///     fn on_return(self, _: ()) -> bool {
    ///         false
    ///     }
    /// }
    ///
    /// // Use map() to convert the bool to a string label.
    /// let label = IsDigit.map(|b| if b { "digit" } else { "other" });
    /// let result = OneChar(Some('7')).resume(()).visit(label);
    /// assert_eq!(result, "digit");
    /// ```
    fn map<B, F>(self, f: F) -> MapCocoro<Self, F>
    where
        Self: Sized,
        F: FnOnce(Self::Out) -> B,
    {
        MapCocoro(self, f)
    }
}

/// A `Cocoro` that transforms the output of an inner `Cocoro` with a function.
///
/// Produced by [`Cocoro::map`]; see its documentation for details.
#[derive(Clone)]
pub struct MapCocoro<C, F>(C, F);

impl<I, Y, R, N, C, F, B> Cocoro<I, Y, R, N> for MapCocoro<C, F>
where
    N: Coro<I, Y, R>,
    C: Cocoro<I, Y, R, N>,
    F: FnOnce(C::Out) -> B,
{
    type Out = B;

    fn on_yield(self, y: Y, next: N) -> B {
        let Self(inner, f) = self;
        f(inner.on_yield(y, next))
    }

    fn on_return(self, r: R) -> B {
        let Self(inner, f) = self;
        f(inner.on_return(r))
    }
}
