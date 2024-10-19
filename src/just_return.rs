use crate::coro::Coro;
use crate::suspended::Suspended;
use crate::suspended::SuspendedVisitor;
use crate::void::Void;

/// A coroutine that just returns a value.
///
/// See [`just_return`](fn.just_return.html).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JustReturn<T>(T);

/// An implementation of `Suspended` that wraps a return value.
///
/// A coroutine that uses this struct as its `Suspend` associated type will be
/// known at compile time to always return, and never yield.
pub struct Returned<T>(pub T);

impl<I, Y, R> Suspended<I, Y, R> for Returned<R> {
    type Next = Void;
    fn visit<X>(
        self,
        visitor: impl SuspendedVisitor<I, Y, R, Void, Out = X>,
    ) -> X {
        visitor.on_return(self.0)
    }
}

impl<Y, T, I> Coro<I, Y, T> for JustReturn<T> {
    type Next = Void;
    type Suspend = Returned<T>;

    fn resume(self, _: I) -> Self::Suspend {
        Returned(self.0)
    }
}

/// Create a coroutine that just returns a value.
///
/// A coroutine created with `just_return` is compatible with any function that
/// expects a coroutine with any yield type, as long as the return type of the
/// coroutine is the same as the value passed to `just_return`.
pub fn just_return<T>(t: T) -> JustReturn<T> {
    JustReturn(t)
}
