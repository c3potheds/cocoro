use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;
use core::marker::PhantomData;
use core::ops::ControlFlow;
use ControlFlow::{Break, Continue};
use Suspend::{Return, Yield};

pub struct TryMapYield<Y, K, F> {
    coro: K,
    f: F,
    _phantom: PhantomData<Y>,
}

impl<Y, K, F> TryMapYield<Y, K, F> {
    pub fn new<R, I, Y2>(coro: K, f: F) -> Self
    where
        K: Coro<I, Y, R>,
        F: FnMut(Y) -> ControlFlow<R, Y2>,
    {
        TryMapYield {
            coro,
            f,
            _phantom: PhantomData,
        }
    }
}

impl<I, Y, K, F, Y2, R> Coro<I, Y2, R> for TryMapYield<Y, K, F>
where
    K: Coro<I, Y, R>,
    F: FnMut(Y) -> ControlFlow<R, Y2>,
{
    type Next = TryMapYield<Y, K::Next, F>;
    type Suspend = Suspend<Y2, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self { coro, mut f, .. } = self;
        match coro.resume(input).into_enum() {
            Yield(y, next) => match f(y) {
                Continue(y2) => Yield(y2, TryMapYield::new(next, f)),
                Break(r) => Return(r),
            },
            Return(r) => Return(r),
        }
    }
}
