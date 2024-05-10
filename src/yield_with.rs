use crate::coro::Coro;
use crate::suspended::Suspended;
use Suspended::Yield;

pub struct YieldWith<F> {
    f: F,
}

impl<Y, R, I, F> Coro<Y, R, I> for YieldWith<F>
where
    F: FnMut(I) -> Y,
{
    type Next = Self;
    fn resume(mut self, input: I) -> Suspended<Y, R, Self::Next> {
        Yield((self.f)(input), self)
    }
}

pub fn yield_with<Y, I, F>(f: F) -> YieldWith<F>
where
    F: FnMut(I) -> Y,
{
    YieldWith { f }
}
