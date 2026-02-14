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
}
