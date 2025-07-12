use crate::FixedPointCoro;
use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::weave::weave;
use core::marker::PhantomData;
use either::Either;

pub struct FeedWith<C, F, Y> {
    src: C,
    f: F,
    _phantom: PhantomData<Y>,
}

impl<C, F, Y> FeedWith<C, F, Y> {
    pub fn new(src: C, factory: F) -> Self {
        FeedWith {
            src,
            f: factory,
            _phantom: PhantomData,
        }
    }
}

impl<A, I, Y, R, X, C1, F, C2> Coro<A, X, (R, C2)> for FeedWith<C1, F, Y>
where
    C1: FixedPointCoro<I, Y, R>,
    C2: FixedPointCoro<Y, I, X>,
    F: FnMut(A) -> (I, C2),
{
    type Next = Self;
    type Suspend = Suspend<X, (R, C2), Self::Next>;

    fn resume(self, input: A) -> Self::Suspend {
        use Either::{Left, Right};
        use Suspend::{Return, Yield};
        let Self { src, mut f, .. } = self;
        let (initial_input, processor) = f(input);
        match weave(src, processor, initial_input) {
            Left((r, feed)) => Return((r, feed)),
            Right((x, src)) => Yield(
                x,
                FeedWith {
                    src,
                    f,
                    _phantom: PhantomData,
                },
            ),
        }
    }
}
