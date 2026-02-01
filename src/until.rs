use crate::FixedPointCoro;
use crate::from_control_flow;
use crate::with_state;

/// A `Coro` that takes some type as input, and yields the input back until it
/// receives a value that satisfies the predicate `f`, whereupon it returns that
/// value.
///
/// This can be useful as the argument of the `.compose()` operator to implement
/// a "find-first" algorithm, or used with `weave()` to filter a stream of
/// values yielded from another `Coro`.
///
/// # Example 1: find the first element matching a predicate
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::IntoCoro;
/// use cocoro::until;
///
/// fn find_first<Y, R>(
///     src: impl Coro<(), Y, R>,
///     predicate: impl FnMut(&Y) -> bool,
/// ) -> Option<Y> {
///     src.map_return(|_| None)
///         .compose(until(predicate).map_return(Some))
///         .drive((), |_| ())
/// }
///
/// assert_eq!(Some(10), find_first((1..).into_coro(), |&n| n >= 10))
/// ```
///
/// # Example 2: filter a stream
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::IntoCoro;
/// use cocoro::Returned;
/// use cocoro::Yielded;
/// use cocoro::until;
/// use cocoro::yield_with;
///
/// [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
///     .into_coro()
///     .process(
///         yield_with(|()| {
///             Yielded((), until(|&n| n % 5 == 0).map_yield(|_| ()))
///         }),
///         |r, _| Returned(r),
///     )
///     .assert_yields(5, ())
///     .assert_yields(10, ())
///     .assert_yields(15, ())
///     .assert_returns((), ());
/// ```
pub fn until<T, F>(f: F) -> impl FixedPointCoro<T, T, T>
where
    F: FnMut(&T) -> bool,
{
    use core::ops::ControlFlow::*;
    with_state(
        f,
        from_control_flow(
            |(mut f, t): (F, T)| {
                if f(&t) { Break(t) } else { Continue((f, t)) }
            },
        ),
    )
}
