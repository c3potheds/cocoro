use super::coro::Coro;
use super::suspended::Suspended;
use super::void::Void;
use Suspended::Return;

/// A coroutine that just returns a value.
///
/// See [`just_return`](fn.just_return.html).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JustReturn<T>(T);
impl<Y, T, I> Coro<Y, T, I> for JustReturn<T> {
    type Next = Void;
    fn resume(self, _: I) -> Suspended<Y, T, Self::Next> {
        Return(self.0)
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
