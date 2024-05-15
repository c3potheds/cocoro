use crate::coro::Coro;
use crate::just_yield::Yielded;

pub struct YieldWith<F> {
    f: F,
}

impl<Y, R, I, F> Coro<Y, R, I> for YieldWith<F>
where
    F: FnMut(I) -> Y,
{
    type Next = Self;
    type Suspend = Yielded<Y, Self>;
    fn resume(mut self, input: I) -> Self::Suspend {
        Yielded((self.f)(input), self)
    }
}

/// Yield with a value computed from a mutable function.
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
