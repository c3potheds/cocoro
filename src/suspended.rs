use crate::coro::Coro;
use crate::suspend::Suspend;

pub trait SuspendedVisitor<I, Y, R, N>
where
    N: Coro<I, Y, R>,
{
    type Out;
    fn on_yield(self, y: Y, next: N) -> Self::Out;
    fn on_return(self, r: R) -> Self::Out;
}

/// A trait for types that represent a suspended coroutine.
///
/// The `visit()` method can be thought of as equivalent to pattern-matching
/// on the `Suspend` enum. In fact, the `Suspend` enum implements `Suspended`
/// trait, and any implementation of `Suspended` can be converted to a `Suspend`
/// enum using the `into_enum()` method, so the two are isomorphic.
///
/// The reason to use a trait instead of a concrete enum is to allow for
/// implementations of `Coro` to specify `Suspend` associated types that are
/// more specific than `Suspend<Y, R, N>`. For example, `just_yield()` knows
/// that it never returns so it can return the `Yielded` struct, which
/// implements `Suspended` but doesn't ever invoke `on_return()` on its visitor.
pub trait Suspended<I, Y, R>: Sized {
    type Next: Coro<I, Y, R>;
    fn visit<X>(
        self,
        visitor: impl SuspendedVisitor<I, Y, R, Self::Next, Out = X>,
    ) -> X;

    fn into_enum(self) -> Suspend<Y, R, Self::Next> {
        self.visit({
            use Suspend::*;
            struct AsEnum;
            impl<I, Y, R, N> SuspendedVisitor<I, Y, R, N> for AsEnum
            where
                N: Coro<I, Y, R>,
            {
                type Out = Suspend<Y, R, N>;
                fn on_yield(self, y: Y, next: N) -> Self::Out {
                    Yield(y, next)
                }
                fn on_return(self, r: R) -> Self::Out {
                    Return(r)
                }
            }
            AsEnum
        })
    }

    fn into_yield(self) -> Option<(Y, Self::Next)> {
        self.into_enum().into_yield()
    }

    fn into_return(self) -> Option<R> {
        self.into_enum().into_return()
    }
}
