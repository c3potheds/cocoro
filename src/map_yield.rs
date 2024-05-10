use crate::Coro;
use crate::Suspended;
use Suspended::*;

pub struct MapYield<Y, K, F> {
    coro: K,
    f: F,
    _phantom: std::marker::PhantomData<Y>,
}

impl<Y, K, F> MapYield<Y, K, F> {
    pub fn new<R, I, Y2>(coro: K, f: F) -> Self
    where
        K: Coro<Y, R, I>,
        F: FnMut(Y) -> Y2,
    {
        MapYield {
            coro,
            f,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Y, K, F, Y2, R, I> Coro<Y2, R, I> for MapYield<Y, K, F>
where
    K: Coro<Y, R, I>,
    F: FnMut(Y) -> Y2,
{
    type Next = MapYield<Y, K::Next, F>;

    fn resume(self, input: I) -> Suspended<Y2, R, Self::Next> {
        let Self { coro, mut f, .. } = self;
        match coro.resume(input) {
            Yield(y, next) => Yield(f(y), MapYield::new(next, f)),
            Return(r) => Return(r),
        }
    }
}
