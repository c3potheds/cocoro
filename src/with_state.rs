use crate::Coro;
use crate::Suspend;
use crate::Suspend::Return;
use crate::Suspend::Yield;
use crate::Suspended;

pub struct WithState<S, C> {
    pub state: S,
    coro: C,
}

impl<S, I, Y, R, C> Coro<I, Y, R> for WithState<S, C>
where
    C: Coro<(S, I), (S, Y), R>,
{
    type Next = WithState<S, C::Next>;
    type Suspend = Suspend<Y, R, Self::Next>;

    fn resume(self, input: I) -> Self::Suspend {
        let Self { state, coro, .. } = self;
        match coro.resume((state, input)).into_enum() {
            Yield((state, y), coro) => Yield(y, WithState { state, coro }),
            Return(r) => Return(r),
        }
    }
}

/// Creates a coroutine that carries state through its execution.
///
/// Usually, if you want a stateful coroutine, you can reach for other helper
/// functions like `yield_with()` or `from_control_flow()`. However, there are
/// situations where you need a `FixedPointCoro` that returns its state, without
/// copying it, in which case you'll find that `from_control_flow()` will be
/// unable to move the state out of its closure because the closure has to
/// implement `FnMut`, and `yield_with()` is not suitable for that and because
/// it never returns.
///
/// The `with_state()` helper lets you feed some initial state into a coroutine
/// that must accept the state as part of its input and return the state back
/// when it yields. The resulting coroutine encapsulates the act of passing the
/// from the initial state to the coroutine's input and back from the yield
/// values to the next input, making the final shape of the coroutine look like
/// `Coro<I, Y, R>`.
///
/// This is particularly useful when the input coroutine is a `FixedPointCoro`,
/// such as one implemented by `recursive()`, which is otherwise unable to
/// capture state because of `'static` restrictions and the requirement that
/// the closure implements `Fn`, rather than `FnOnce` or `FnMut`. If you use
/// `recursive()` to make a `Coro<(S, I), (S, Y), R>`, you can then use
/// `with_state()` to create a `Coro<I, Y, R>` that carries the state through
/// its execution without copying it, getting the best of both worlds.
///
/// # Example
///
/// Suppose you want to create a coroutine that fills up a buffer with inputs
/// until it reaches a specified size, and then returns the buffer.
///
/// Naively, you might try to use `from_control_flow()`, but you'll run into a
/// problem because the closure passed to `from_control_flow()` cannot move
/// the `Vec` back out of its closure:
///
/// ```compile_fail
/// use cocoro::{Coro, from_control_flow};
/// use core::ops::ControlFlow::{Continue, Break};
///
/// // Fill up a buffer with the coroutine inputs until it reaches a specified
/// // size, then return the buffer.
/// fn fill(n: usize) -> impl Coro<i32, (), Vec<i32>> {
///     let mut buffer = Vec::new();
///     from_control_flow(move |input: i32| {
///         if buffer.len() < n {
///             buffer.push(input);
///             Continue(())
///         } else {
///             // ERROR: cannot move out of `buffer`, a captured variable in an
///             // `FnMut` closure
///             Break(buffer)  
///         }
///     })
/// }
/// ```
///
/// Instead of capturing the `Vec` in the closure, you can instead pass the
/// `Vec` in and out of the coroutine through the input and yield values, and
/// let `with_state()` handle passing the state through the coroutine:
///
/// ```rust
/// use core::ops::ControlFlow::Break;
/// use core::ops::ControlFlow::Continue;
///
/// use cocoro::Coro;
/// use cocoro::from_control_flow;
/// use cocoro::with_state;
///
/// // Fill up a buffer with the coroutine inputs until it reaches a specified
/// // size, then return the buffer.
/// fn fill(n: usize) -> impl Coro<i32, (), Vec<i32>> {
///     let buffer = Vec::new();
///     with_state(
///         (buffer, n),
///         from_control_flow(
///             move |((mut buffer, n), input): ((Vec<_>, _), _)| {
///                 if buffer.len() < n {
///                     buffer.push(input);
///                     Continue(((buffer, n), ()))
///                 } else {
///                     // OK: `buffer` is owned locally, rather than by the
///                     // closure, so it can be returned.
///                     Break(buffer)
///                 }
///             },
///         ),
///     )
/// }
/// ```
pub fn with_state<I, Y, R, S, C>(state: S, coro: C) -> WithState<S, C>
where
    C: Coro<(S, I), (S, Y), R>,
{
    WithState { state, coro }
}
