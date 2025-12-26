use core::marker::PhantomData;
use core::ops::ControlFlow;

use ControlFlow::*;
use Suspend::*;

use crate::coro::Coro;
use crate::suspend::Suspend;

#[derive(Clone)]
pub struct FromControlFlow<F, I, Y, R> {
    f: F,
    _marker: PhantomData<(I, Y, R)>,
}

/// Creates a new coroutine from a function that returns [`ControlFlow`].
///
/// The provided function `f` is called each time the coroutine is resumed
/// with an input of type `I`.
/// - If `f` returns `ControlFlow::Continue(y)`, the coroutine yields `y`.
/// - If `f` returns `ControlFlow::Break(r)`, the coroutine returns `r`.
///
/// This constructor is useful for creating coroutines with state that
/// determine dynamically whether to yield or return based on the input.
///
/// NOTE: when the `Try` trait is stabilized, this function will likely be
/// renamed to `try_with` and be extended to support anything that implements
/// `Try`.
///
/// # Examples
///
/// ```rust
/// use core::ops::ControlFlow;
///
/// use cocoro::Coro;
/// use cocoro::Void;
/// use cocoro::from_control_flow;
///
/// let mut countdown = 3;
/// from_control_flow(move |input: i32| {
///     if countdown > 0 {
///         let old_countdown = countdown;
///         countdown -= 1;
///         ControlFlow::Continue(format!(
///             "Input: {}, Countdown: {}",
///             input, old_countdown
///         ))
///     } else {
///         ControlFlow::Break(format!("Final input: {}", input))
///     }
/// })
/// .assert_yields("Input: 10, Countdown: 3".to_string(), 10)
/// .assert_yields("Input: 20, Countdown: 2".to_string(), 20)
/// .assert_yields("Input: 30, Countdown: 1".to_string(), 30)
/// .assert_returns("Final input: 40".to_string(), 40);
/// ```
pub fn from_control_flow<I, Y, R, F>(f: F) -> FromControlFlow<F, I, Y, R>
where
    F: FnMut(I) -> ControlFlow<R, Y>,
{
    FromControlFlow {
        f,
        _marker: PhantomData,
    }
}

impl<I, Y, R, F> Coro<I, Y, R> for FromControlFlow<F, I, Y, R>
where
    F: FnMut(I) -> ControlFlow<R, Y>,
{
    type Next = Self; // It's a fixed point coroutine
    type Suspend = Suspend<Y, R, Self::Next>;

    fn resume(mut self, input: I) -> Self::Suspend {
        match (self.f)(input) {
            Continue(y) => Yield(y, self),
            Break(r) => Return(r),
        }
    }
}
