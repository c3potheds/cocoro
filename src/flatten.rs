use crate::Coro;
use crate::Suspended;
use crate::Suspended::{Return, Yield};
use either::Either;
use either::Left;
use either::Right; // Import the Yield variant

pub struct FlattenImpl<Y, Outer, Inner> {
    outer: Outer,
    _phantom: std::marker::PhantomData<(Y, Inner)>,
}

impl<Y, Outer, Inner> FlattenImpl<Y, Outer, Inner> {
    pub fn new(outer: Outer) -> Self {
        FlattenImpl {
            outer,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<Y, R, I, Outer, Inner> Coro<Y, R, I> for FlattenImpl<Y, Outer, Inner>
where
    Outer: Coro<Y, Inner, I>,
    Inner: Coro<Y, R, I>,
    I: Copy,
{
    type Next = Either<FlattenImpl<Y, Outer::Next, Inner>, Inner::Next>;
    fn resume(self, input: I) -> Suspended<Y, R, Self::Next> {
        let Self { outer, .. } = self;
        match outer.resume(input) {
            Yield(inner, next) => Yield(inner, Left(next.flatten())),
            Return(r) => match r.resume(input) {
                Yield(y, next) => Yield(y, Right(next)),
                Return(r) => Return(r),
            },
        }
    }
}
