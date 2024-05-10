use either::Either;
use crate::Coro;
use crate::Suspended;
use Suspended::{Return, Yield};
use Either::Left;
use Either::Right;

/// Implement the `Coro`` trait for the `Either`` type when both variants
/// themselves implement `Coro` for the same input and output types.
impl<Y, R, I, A, B> Coro<Y, R, I> for Either<A, B>
where
    A: Coro<Y, R, I>,
    B: Coro<Y, R, I>,
{
    type Next = Either<A::Next, B::Next>;
    fn resume(self, input: I) -> Suspended<Y, R, Self::Next> {
        match self {
            Left(a) => match a.resume(input) {
                Yield(y, next) => Yield(y, Left(next)),
                Return(r) => Return(r),
            },
            Right(b) => match b.resume(input) {
                Yield(y, next) => Yield(y, Right(next)),
                Return(r) => Return(r),
            },
        }
    }
}
