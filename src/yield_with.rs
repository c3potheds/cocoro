use crate::coro::Coro;
use crate::just_yield::Yielded;

pub struct YieldWith<F> {
    f: F,
}

impl<I, Y, R, F> Coro<I, Y, R> for YieldWith<F>
where
    F: FnMut(I) -> Y,
{
    type Next = Self;
    type Suspend = Yielded<Y, Self>;
    fn resume(mut self, input: I) -> Self::Suspend {
        Yielded((self.f)(input), self)
    }
}

/// Creates a coroutine that yields values computed by a mutable function.
///
/// The state of a `yield_with()` coroutine is owned by the `FnMut`, which is
/// responsible for updating its internal state and returning the next value to
/// yield.
///
/// A `yield_with()` coroutine is a `FixedPointCoro`, which means that its
/// `Next` state is always `Self`.
pub fn yield_with<Y, I, F>(f: F) -> YieldWith<F>
where
    F: FnMut(I) -> Y,
{
    YieldWith { f }
}
