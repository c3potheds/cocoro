use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;
use core::marker::PhantomData;

pub struct Compose<I, Y1, A, B> {
    a: A,
    b: B,
    _phantom: PhantomData<(I, Y1)>,
}

impl<I, Y1, A, B> Compose<I, Y1, A, B> {
    pub fn new<R, Y2>(a: A, b: B) -> Self
    where
        A: Coro<Y1, R, I>,
        B: Coro<Y2, R, Y1>,
    {
        Compose {
            a,
            b,
            _phantom: PhantomData,
        }
    }
}

impl<I, Y1, Y2, R, A, B> Coro<Y2, R, I> for Compose<I, Y1, A, B>
where
    A: Coro<Y1, R, I>,
    B: Coro<Y2, R, Y1>,
{
    type Next = Compose<I, Y1, A::Next, B::Next>;
    type Suspend = Suspend<Y2, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Suspend::{Return, Yield};
        let Compose { a, b, .. } = self;
        match a.resume(input).into_enum() {
            Yield(y, next) => match b.resume(y).into_enum() {
                Yield(y2, next2) => Yield(y2, Compose::new(next, next2)),
                Return(r) => Return(r),
            },
            Return(r) => Return(r),
        }
    }
}
