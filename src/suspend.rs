use crate::coro::Coro;
use crate::suspended::Suspended;
use crate::suspended::SuspendedVisitor;

/// The state of a `Coro` coroutine after a call to `resume` as finished.
///
/// After a call to `resume()` finishes, the coroutine is in one of two states:
///
///   * `Yield(y, n)`: The coroutine yielded a value `y` and is ready to be
///     resumed with the next input value. The next state of the coroutine is
///     `n`, which necessarily implements the `Coro` trait.
///   * `Return(r)`: The coroutine has finished and returned a value `r`. The
///     coroutine cannot be resumed again, because the call to `resume()` has
///     consumed it without giving back a `Coro` value to resume.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Suspend<Y, R, N> {
    Yield(Y, N),
    Return(R),
}

use Suspend::*;

impl<Y, R, N> Suspend<Y, R, N> {
    /// Returns the yielded value and the next state of the coroutine, if the
    /// coroutine is in the `Yield` state, or `None` otherwise.
    ///
    /// Compare to `Result::ok()` or `ControlFlow::continue_value()`.
    pub fn into_yield(self) -> Option<(Y, N)> {
        match self {
            Yield(y, n) => Some((y, n)),
            Return(_) => None,
        }
    }

    /// Returns the return value of the coroutine, if the coroutine is in the
    /// `Return` state, or `None` otherwise.
    ///
    /// Compare to `Result::err()` or `ControlFlow::break_value()`.
    pub fn into_return(self) -> Option<R> {
        match self {
            Yield(_, _) => None,
            Return(r) => Some(r),
        }
    }
}

/// `Suspend` implements the `Suspended` trait; indeed, most coroutines simply
/// use `Suspend` as their associated `Suspend` type.
impl<I, Y, R, N> Suspended<I, Y, R> for Suspend<Y, R, N>
where
    N: Coro<I, Y, R>,
{
    type Next = N;
    fn visit<X>(
        self,
        visitor: impl SuspendedVisitor<I, Y, R, N, Out = X>,
    ) -> X {
        match self {
            Yield(y, next) => visitor.on_yield(y, next),
            Return(r) => visitor.on_return(r),
        }
    }

    fn into_enum(self) -> Suspend<Y, R, N> {
        self
    }
}
