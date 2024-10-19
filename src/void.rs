use crate::coro::Coro;
use crate::suspended::Suspended;
use crate::suspended::SuspendedVisitor;

// Private struct to make Void uninstantiable outside this module. Do not
// instantiate this struct. Do not derive any traits like `Default` or `Clone`.
#[derive(Debug, PartialEq, Eq)]
struct Uninstantiable;

/// A stand-in for the 'never' type `!`, which can be used as the `Next` type
/// for a coroutine that will never yield again.
///
/// This type cannot be instantiated, so it is impossible to `Yield` from a
/// coroutine whose `Next` type is `Void`.
#[derive(Debug, PartialEq, Eq)]
pub struct Void(Uninstantiable);

impl<T, R, I> Suspended<T, R, I> for Void {
    type Next = Void;
    fn visit<X>(
        self,
        _: impl SuspendedVisitor<T, R, I, Self::Next, Out = X>,
    ) -> X {
        unreachable!()
    }
}

impl<I, Y, R> Coro<I, Y, R> for Void {
    type Next = Void;
    type Suspend = Void;
    fn resume(self, _: I) -> Self::Suspend {
        unreachable!()
    }
}
