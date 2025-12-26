use core::marker::PhantomData;

use Suspend::*;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

pub struct MapYield<Y, K, F> {
    coro: K,
    f: F,
    _phantom: PhantomData<Y>,
}

impl<Y, K, F> MapYield<Y, K, F> {
    pub fn new<R, I, Y2>(coro: K, f: F) -> Self
    where
        K: Coro<I, Y, R>,
        F: FnMut(Y) -> Y2,
    {
        MapYield {
            coro,
            f,
            _phantom: PhantomData,
        }
    }
}

impl<I, Y, K, F, Y2, R> Coro<I, Y2, R> for MapYield<Y, K, F>
where
    K: Coro<I, Y, R>,
    F: FnMut(Y) -> Y2,
{
    type Next = MapYield<Y, K::Next, F>;
    type Suspend = Suspend<Y2, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self { coro, mut f, .. } = self;
        match coro.resume(input).into_enum() {
            Yield(y, next) => Yield(f(y), MapYield::new(next, f)),
            Return(r) => Return(r),
        }
    }
}
