use crate::FixedPointCoro;
use crate::from_control_flow;
use crate::with_state;

/// Creates a coroutine that yields its inputs while they satisfy the predicate,
/// and returns the first input that doesn't.
///
/// This can be useful as the argument of the `.compose()` operator to implement
/// a "do-until" algorithm, or used with `weave()` to filter a stream of values
/// yielded by another `Coro`.
///
/// # Example 1: truncate a coroutine
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::IntoCoro;
/// use cocoro::continue_while;
///
/// (1..)
///     .into_coro()
///     .compose(continue_while(|&n| n < 5).map_return(|_| ()))
///     .assert_yields((), 1)
///     .assert_yields((), 2)
///     .assert_yields((), 3)
///     .assert_yields((), 4)
///     .assert_returns((), ());
/// ```
///
/// # Example 2: take elements matching a pattern
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::IntoCoro;
/// use cocoro::continue_while;
///
/// // Extract consecutive alphabetic tokens
/// ["hello", "world", "123", "foo", "bar"]
///     .into_coro()
///     .compose(
///         continue_while(|s: &&str| s.chars().all(|c| c.is_alphabetic()))
///             .map_return(|_| ()),
///     )
///     .assert_yields((), "hello")
///     .assert_yields((), "world")
///     .assert_returns((), ());
/// ```
pub fn continue_while<T, F>(f: F) -> impl FixedPointCoro<T, T, T>
where
    F: FnMut(&T) -> bool,
{
    use core::ops::ControlFlow::*;
    with_state(
        f,
        from_control_flow(
            |(mut f, t): (F, T)| {
                if f(&t) { Continue((f, t)) } else { Break(t) }
            },
        ),
    )
}
