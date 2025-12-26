use core::marker::PhantomData;

use Suspend::*;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

pub struct MapReturn<R, K, F> {
    coro: K,
    f: F,
    _phantom: PhantomData<R>,
}

impl<R, K, F> MapReturn<R, K, F> {
    pub fn new<Y, I, R2>(coro: K, f: F) -> Self
    where
        K: Coro<I, Y, R>,
        F: FnOnce(R) -> R2,
    {
        MapReturn {
            coro,
            f,
            _phantom: PhantomData,
        }
    }
}

impl<I, Y, K, F, R, R2> Coro<I, Y, R2> for MapReturn<R, K, F>
where
    K: Coro<I, Y, R>,
    F: FnOnce(R) -> R2,
{
    type Next = MapReturn<R, K::Next, F>;
    type Suspend = Suspend<Y, R2, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self { coro, f, .. } = self;
        match coro.resume(input).into_enum() {
            Yield(y, next) => Yield(y, MapReturn::new(next, f)),
            Return(r) => Return(f(r)),
        }
    }
}
