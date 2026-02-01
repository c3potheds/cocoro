use crate::FixedPointCoro;
use crate::from_control_flow;
use crate::with_state;

/// A coroutine that yields its inputs `n` times, then returns `()`.
///
/// This is most commonly used as the argument to the `compose()` operator to
/// limit how many times the source coroutine yields.
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
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
/// // iota() by itself will continue yielding forever.
/// iota()
///     // Limit the yielded values to 3.
///     .compose(take(3))
///     .assert_yields(1, ())
///     .assert_yields(2, ())
///     .assert_yields(3, ())
///     .assert_returns((), ());
/// ```
///
/// If the source coroutine returns something other than `()`, you must remap
/// the source coroutine's return type or the `take()` coroutine's return type
/// to match the other. One easy way to do this is to lift the return value
/// into an `Option`:
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::Yield;
/// use cocoro::from_fn;
/// use cocoro::just_return;
/// use cocoro::take;
///
/// from_fn(|()| Yield(1, from_fn(|()| Yield(2, just_return("done")))))
///     // Map the source coroutine's return value to `Some` to indicate that it
///     // returned.
///     .map_return(Some)
///     // Map the `take()` coroutine's return value to `None` to indicate that
///     // the limit was reached, and to match the type of the source.
///     .compose(take(5).map_return(|_| None))
///     .assert_yields(1, ())
///     .assert_yields(2, ())
///     .assert_returns(Some("done"), ());
/// ```
pub fn take<T>(n: usize) -> impl FixedPointCoro<T, T, ()> {
    use core::ops::ControlFlow::*;
    with_state(
        n,
        from_control_flow(|(n, t): (usize, T)| {
            if n == 0 {
                Break(())
            } else {
                Continue((n - 1, t))
            }
        }),
    )
}
