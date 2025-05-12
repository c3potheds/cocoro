use crate::coro::Coro;
use crate::suspended::Suspended;

#[derive(Clone, Debug)]
pub struct FromFn<F>(F);

impl<I, Y, R, N, S, F> Coro<I, Y, R> for FromFn<F>
where
    F: FnOnce(I) -> S,
    S: Suspended<I, Y, R, Next = N>,
    N: Coro<I, Y, R>,
{
    type Next = N;
    type Suspend = S;
    fn resume(self, input: I) -> Self::Suspend {
        let Self(f) = self;
        f(input)
    }
}

/// Creates a coroutine from a function that returns a `Suspend`.
///
/// This is the most straightforward way to implement `Coro` without manually
/// implementing the trait for your own data type.
///
/// Using `from_fn()` exclusively to define a coroutine guarantees its entire
/// state machine is known at compile time and can only proceed in the correct
/// order. Each iteration of such a coroutine has a unique type whose `Coro`
/// implementation (specifically, its `Next` associated type) points to the
/// next state.
///
/// This function is an implementation detail in many combinators. However,
/// it's often preferable to use higher-level functions and combinators like
/// `yield_with()` and `recursive()` to implement coroutines.
///
/// # Examples
///
/// ```rust
/// use cocoro::{from_fn, Coro, Returned, Void, Yield};
///
/// // A coroutine that yields 3, 2, 1, and then returns with "Blastoff!"
/// #[rustfmt::skip]
/// let countdown = from_fn(|()| {
///     Yield(3, from_fn(|()| {
///     Yield(2, from_fn(|()| {
///     Yield(1, from_fn(|()| {
///     Returned("Blastoff!") })) })) }))
/// });
/// countdown
///     .assert_yields(3, ())
///     .assert_yields(2, ())
///     .assert_yields(1, ())
///     .assert_returns("Blastoff!", ());
/// ```
pub fn from_fn<I, Y, R, N, S, F>(
    f: F,
) -> impl Coro<I, Y, R, Next = N, Suspend = S>
where
    F: FnOnce(I) -> S,
    S: Suspended<I, Y, R, Next = N>,
    N: Coro<I, Y, R>,
{
    FromFn(f)
}
