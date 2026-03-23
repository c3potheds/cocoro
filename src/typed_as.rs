use core::marker::PhantomData;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

/// A wrapper that pins the input type `I`, yield type `Y`, and return type `R`
/// into the concrete struct type, preserving all other type information
/// including the `FixedPointCoro` property.
///
/// Produced by [`Coro::typed_as`]; see its documentation for details.
pub struct TypedAs<C, I, Y, R>(pub C, pub PhantomData<(I, Y, R)>);

impl<I, Y, R, C> Coro<I, Y, R> for TypedAs<C, I, Y, R>
where
    C: Coro<I, Y, R>,
{
    type Next = TypedAs<C::Next, I, Y, R>;
    type Suspend = Suspend<Y, R, Self::Next>;

    fn resume(self, input: I) -> Self::Suspend {
        use Suspend::Return;
        use Suspend::Yield;
        let Self(coro, _) = self;
        match coro.resume(input).into_enum() {
            Yield(y, next) => Yield(y, TypedAs(next, PhantomData)),
            Return(r) => Return(r),
        }
    }
}
