use crate::coro::Coro;

/// An eliminator for [`Suspend`](crate::Suspend): a first-class representation
/// of case analysis on a suspended coroutine step.
///
/// Where [`Coro`] *produces* values by yielding and eventually returning,
/// `Cocoro` *consumes* one step of that stream: it handles the two possible
/// outcomes of [`Suspend`](crate::Suspend) — a yielded value with the next
/// coroutine state, or a final return value — and produces some output of type
/// `Out`.
///
/// This trait can be thought of as a generalization of the `match` expression
/// on a [`Suspend`](crate::Suspend) enum, where the `Out` type is the type
/// that the `match`
/// expression evaluates to, and the `on_yield()` and `on_return()`
/// implementations are case handlers for the `Yield` and `Return` variants.
///
/// In many cases, you may want to simply use normal pattern-matching with a
/// `match` expression on a `Suspend` enum, which can be obtained from any
/// `Suspended` implementation with the `into_enum()` method. But having a
/// first-class eliminator is valuable when you want to compose or transform
/// the handler itself before using it — for instance using [`Cocoro::map`] to
/// post-process the output, or passing a `Cocoro` as a parameter.
///
/// # Duality with `Coro`
///
/// For the special case of `I = ()`, `Coro` and `Cocoro` are categorical
/// duals:
///
/// - `Coro<(), Y, R>` is an **F-coalgebra** for `F(X) = Y×X + R`: given a
///   state, it produces either a yielded value plus a next state, or a final
///   return value.
/// - `Cocoro<(), Y, R, N>` is an **F-algebra** for the same functor: given the
///   formula `Y×N + R` (encoded as either `on_yield(y, next)` or
///   `on_return(r)`), it produces an output of type `Out`.
///
/// This is the precise categorical sense in which `Cocoro` is the "co-"
/// counterpart to `Coro`. With non-trivial input types (`I ≠ ()`), the duality
/// is approximate: `Cocoro` is more accurately an eliminator for `Suspend`
/// than a strict categorical dual.
///
/// # Example: parsers
///
/// A single parser step over a token stream of type `Token` that terminates
/// with a value of type `End` can be represented as a
/// `Cocoro<(), Token, End, N, Out = Result<T, ParseError>>`, where
/// `N: Coro<(), Token, End>` is the remaining input stream. The `on_yield()`
/// method receives the next token and the remaining stream; `on_return()` is
/// called when the stream is exhausted.
///
/// In use cases like parsing, the combinators and transformations are often
/// best applied to `Cocoro` instances that define the building blocks of
/// parsers, rather than to the `Coro` instances that define the input streams
/// being parsed.
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
