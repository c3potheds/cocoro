use crate::Coro;
use crate::Suspended;
use Suspended::*;

pub struct MapReturn<R, K, F> {
    coro: K,
    f: F,
    _phantom: std::marker::PhantomData<R>,
}

impl<R, K, F> MapReturn<R, K, F> {
    pub fn new<Y, I, R2>(coro: K, f: F) -> Self
    where
        K: Coro<Y, R, I>,
        F: FnOnce(R) -> R2,
    {
        MapReturn {
            coro,
            f,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Y, K, F, R, I, R2> Coro<Y, R2, I> for MapReturn<R, K, F>
where
    K: Coro<Y, R, I>,
    F: FnOnce(R) -> R2,
{
    type Next = MapReturn<R, K::Next, F>;

    fn resume(self, input: I) -> Suspended<Y, R2, Self::Next> {
        let Self { coro, f, .. } = self;
        match coro.resume(input) {
            Yield(y, next) => Yield(y, MapReturn::new(next, f)),
            Return(r) => Return(f(r)),
        }
    }
}
