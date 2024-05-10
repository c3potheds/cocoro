use crate::Coro;
use crate::Suspended;

// Private struct to make Void uninstantiable outside this module. Do not
// instantiate this struct.
struct Uninstantiable;

/// A stand-in for the 'never' type `!`, which can be used as the `Next` type
/// for a coroutine that will never yield again.
///
/// This type cannot be instantiated, so it is impossible to `Yield` from a
/// coroutine whose `Next` type is `Void`.
pub struct Void(Uninstantiable);
impl<Y, R, I> Coro<Y, R, I> for Void {
    type Next = Void;
    fn resume(self, _: I) -> Suspended<Y, R, Self> {
        unreachable!()
    }
}
