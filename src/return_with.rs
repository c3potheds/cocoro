use crate::Void;
use crate::coro::Coro;
use crate::just_return::Returned;

pub struct ReturnWith<F> {
    f: F,
}

impl<I, Y, R, F> Coro<I, Y, R> for ReturnWith<F>
where
    F: FnOnce(I) -> R,
{
    type Next = Void;
    type Suspend = Returned<R>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self { f } = self;
        Returned(f(input))
    }
}

/// Creates a coroutine that returns a value computed by a function of the input.
///
/// The state of a `return_with()` coroutine is owned by the `FnOnce`, which is
/// responsible for computing the return value based on the input.
///
/// A coroutine created with `return_with()` will never yield.
pub fn return_with<Y, I, F>(f: F) -> ReturnWith<F>
where
    F: FnOnce(I) -> Y,
{
    ReturnWith { f }
}
