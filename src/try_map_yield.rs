// Substitute the following closure for the `try_map_yield` method with a full
// struct and `Coro` implementation:
//
// move |input| match self.resume(input) {
//     Yield(y, next) => match f(y) {
//         Continue(y2) => Yield(y2, next.try_map_yield(f)),
//         Break(r) => Return(r),
//     },
//     Return(r) => Return(r),
// }

use crate::Coro;
use crate::Suspended;
use crate::Suspended::{Return, Yield};
use std::ops::ControlFlow;
use ControlFlow::{Continue, Break};

pub struct TryMapYield<Y, K, F> {
    coro: K,
    f: F,
    _phantom: std::marker::PhantomData<Y>,
}

impl<Y, K, F> TryMapYield<Y, K, F> {
    pub fn new<R, I, Y2>(coro: K, f: F) -> Self
    where
        K: Coro<Y, R, I>,
        F: FnMut(Y) -> ControlFlow<R, Y2>,
    {
        TryMapYield {
            coro,
            f,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Y, K, F, Y2, R, I> Coro<Y2, R, I> for TryMapYield<Y, K, F>
where
    K: Coro<Y, R, I>,
    F: FnMut(Y) -> ControlFlow<R, Y2>,
{
    type Next = TryMapYield<Y, K::Next, F>;

    fn resume(self, input: I) -> Suspended<Y2, R, Self::Next> {
        let Self { coro, mut f, .. } = self;
        match coro.resume(input) {
            Yield(y, next) => match f(y) {
                Continue(y2) => Yield(y2, TryMapYield::new(next, f)),
                Break(r) => Return(r),
            },
            Return(r) => Return(r),
        }
    }
}