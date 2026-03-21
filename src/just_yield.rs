use crate::cocoro::Cocoro;
use crate::coro::Coro;
use crate::suspended::Suspended;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JustYield<T>(T);

/// Represents a suspended state where a coroutine has yielded a value (`T`)
/// and includes the next state of the coroutine (`N`).
pub struct Yielded<T, N>(pub T, pub N);

impl<T, N, R, I> Suspended<I, T, R> for Yielded<T, N>
where
    N: Coro<I, T, R>,
{
    type Next = N;
    fn visit<X>(self, visitor: impl Cocoro<I, T, R, N, Out = X>) -> X {
        let Self(y, n) = self;
        visitor.on_yield(y, n)
    }
}

impl<T, R, I> Coro<I, T, R> for JustYield<T>
where
    T: Copy,
{
    type Next = Self;
    type Suspend = Yielded<T, Self>;
    fn resume(self, _: I) -> Self::Suspend {
        let Self(t) = self;
        Yielded(t, Self(t))
    }
}

pub fn just_yield<T>(t: T) -> JustYield<T> {
    JustYield(t)
}
