// Implements the compose() method for `Coro`, substituting the closure-based
// impl below:
//
// move |input| match self.resume(input) {
//     Yield(y, next) => match other.resume(y) {
//         Yield(y2, next2) => Yield(y2, next.compose(next2)),
//         Return(r2) => Return(Right(r2)),
//     },
//     Return(r) => Return(Left(r)),
// }

use either::Either;

use crate::Coro;
use crate::Suspended;
use Suspended::{Return, Yield};
use Either::{Left, Right};

pub struct Compose<I, Y1, A, B> {
    a: A,
    b: B,
    _phantom: std::marker::PhantomData<(I, Y1)>,
}

impl<I, Y1, A, B> Compose<I, Y1, A, B> {
    pub fn new<R1, R2, Y2>(a: A, b: B) -> Self
    where
        A: Coro<Y1, R1, I>,
        B: Coro<Y2, R2, Y1>,
    {
        Compose {
            a,
            b,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<I, Y1, Y2, R1, R2, A, B> Coro<Y2, Either<R1, R2>, I> for Compose<I, Y1, A, B>
where
    A: Coro<Y1, R1, I>,
    B: Coro<Y2, R2, Y1>,
{
    type Next = Compose<I, Y1, A::Next, B::Next>;
    fn resume(self, input: I) -> Suspended<Y2, Either<R1, R2>, Self::Next> {
        match self.a.resume(input) {
            Yield(y, next) => match self.b.resume(y) {
                Yield(y2, next2) => Yield(y2, Compose::new(next, next2)),
                Return(r2) => Return(Right(r2)),
            },
            Return(r) => Return(Left(r)),
        }
    }
}
