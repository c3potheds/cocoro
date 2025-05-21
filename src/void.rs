use crate::coro::Coro;
use crate::suspended::Suspended;
use crate::suspended::SuspendedVisitor;

/// A stand-in for the 'never' type `!`, which can be used as the `Next` type
/// for a coroutine that will never yield again.
///
/// This type cannot be instantiated, so it is impossible to `Yield` from a
/// coroutine whose `Next` type is `Void`. Likewise, it is impossible to
/// `Return` from a coroutine whose return type is `Void` and it is impossible
/// to `Yield` from a coroutine whose yield type is `Void`.
///
/// It is useful to mark coroutines that never return as having a return type of
/// `Void`, and coroutines that never yield as having a yield type of `Void`.
///
/// ```rust
/// use cocoro::from_fn;
/// use cocoro::just_return;
/// use cocoro::just_yield;
/// use cocoro::Coro;
/// use cocoro::Void;
///
/// #[derive(Clone, Copy)]
/// struct Foo;
///
/// // A coroutine that never yields.
/// fn never_yield() -> impl Coro<(), Void, Foo> {
///     just_return(Foo)
/// }
///
/// // A coroutine that never returns.
/// fn never_return() -> impl Coro<(), Foo, Void> {
///     just_yield(Foo)
/// }
///
/// // A coroutine that can never be resumed.
/// fn never_resume() -> impl Coro<Void, (), ()> {
///     // The input type to the closure is `Void`, so it can never be called.
///     // Because `Void` implements `Suspended`, it can be used as the result
///     // of the closure to meet the requirements of `from_fn()`.
///     from_fn(|void| void)
/// }
/// ```
#[derive(Debug, PartialEq, Eq)]
pub enum Void {}

impl<T, R, I> Suspended<T, R, I> for Void {
    type Next = Void;
    fn visit<X>(
        self,
        _: impl SuspendedVisitor<T, R, I, Self::Next, Out = X>,
    ) -> X {
        unreachable!()
    }
}

impl<I, Y, R> Coro<I, Y, R> for Void {
    type Next = Void;
    type Suspend = Void;
    fn resume(self, _: I) -> Self::Suspend {
        unreachable!()
    }
}
