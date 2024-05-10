/// The state of a `Coro` coroutine after a call to `resume` as finished.
///
/// After a call to `resume()` finishes, the coroutine is in one of two states:
///
///   * `Yield(y, n)`: The coroutine yielded a value `y` and is ready to be
///     resumed with the next input value. The next state of the coroutine is
///    `n`, which necessarily implements the `Coro` trait.
///   * `Return(r)`: The coroutine has finished and returned a value `r`. The
///     coroutine cannot be resumed again, because the call to `resume()` has
///     consumed it without giving back a `Coro` value to resume.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Suspended<Y, R, N> {
    Yield(Y, N),
    Return(R),
}

use Suspended::*;

impl<Y, R, N> Suspended<Y, R, N> {
    /// Returns the yielded value and the next state of the coroutine, if the
    /// coroutine is in the `Yield` state, or `None` otherwise.
    ///
    /// Compare to `Result::ok()` or `ControlFlow::continue_value()`.
    pub fn as_yield(self) -> Option<(Y, N)> {
        match self {
            Yield(y, n) => Some((y, n)),
            Return(_) => None,
        }
    }

    /// Returns the return value of the coroutine, if the coroutine is in the
    /// `Return` state, or `None` otherwise.
    ///
    /// Compare to `Result::err()` or `ControlFlow::break_value()`.
    pub fn as_return(self) -> Option<R> {
        match self {
            Yield(_, _) => None,
            Return(r) => Some(r),
        }
    }
}
