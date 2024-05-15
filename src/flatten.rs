use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;
use either::Either;

pub struct FlattenImpl<Y, Outer, Inner> {
    outer: Outer,
    _phantom: core::marker::PhantomData<(Y, Inner)>,
}

impl<Y, Outer, Inner> FlattenImpl<Y, Outer, Inner> {
    pub fn new(outer: Outer) -> Self {
        FlattenImpl {
            outer,
            _phantom: core::marker::PhantomData,
        }
    }
}

impl<Y, R, I, Outer, Inner> Coro<Y, R, I> for FlattenImpl<Y, Outer, Inner>
where
    I: Copy,
    Outer: Coro<Y, Inner, I>,
    Inner: Coro<Y, R, I>,
{
    type Next = Either<FlattenImpl<Y, Outer::Next, Inner>, Inner::Next>;
    type Suspend = Suspend<Y, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use either::{Left, Right};
        use Suspend::{Return, Yield};
        let Self { outer, .. } = self;
        match outer.resume(input).into_enum() {
            Yield(inner, next) => Yield(inner, Left(FlattenImpl::new(next))),
            Return(r) => match r.resume(input).into_enum() {
                Yield(y, next) => Yield(y, Right(next)),
                Return(r) => Return(r),
            },
        }
    }
}
