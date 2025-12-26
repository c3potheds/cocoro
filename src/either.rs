use Either::Left;
use Either::Right;
use Suspend::Return;
use Suspend::Yield;
use either::Either;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

/// Implements the `Coro` trait for the `Either` type when both variants
/// themselves implement `Coro` for the same input and output types.
impl<I, Y, R, A, B> Coro<I, Y, R> for Either<A, B>
where
    A: Coro<I, Y, R>,
    B: Coro<I, Y, R>,
{
    type Next = Either<A::Next, B::Next>;
    type Suspend = Suspend<Y, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        match self {
            Left(a) => match a.resume(input).into_enum() {
                Yield(y, next) => Yield(y, Left(next)),
                Return(r) => Return(r),
            },
            Right(b) => match b.resume(input).into_enum() {
                Yield(y, next) => Yield(y, Right(next)),
                Return(r) => Return(r),
            },
        }
    }
}
