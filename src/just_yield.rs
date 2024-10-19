use crate::coro::Coro;
use crate::suspended::Suspended;
use crate::suspended::SuspendedVisitor;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JustYield<T>(T);

/// An implementation of `Suspended` that wraps a yield value.
///
/// A coroutine that uses this struct as its `Suspend` associated type will be
/// known at compile time to always return, and never yield.
pub struct Yielded<T, N>(pub T, pub N);

impl<T, N, R, I> Suspended<I, T, R> for Yielded<T, N>
where
    N: Coro<I, T, R>,
{
    type Next = N;
    fn visit<X>(
        self,
        visitor: impl SuspendedVisitor<I, T, R, N, Out = X>,
    ) -> X {
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
        Yielded(self.0, self)
    }
}

pub fn just_yield<T>(t: T) -> JustYield<T> {
    JustYield(t)
}
