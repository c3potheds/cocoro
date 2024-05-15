use crate::coro::Coro;

/// A refinement of the `Coro` trait that specifies that the `Next` associated
/// type must be `Self`.
///
/// When a coroutine implements `FixedPointCoro`, certain algorithms that
/// require in-place mutation of a coroutine can be implemented. For example,
/// an iterator can be constructed from any `FixedPointCoro` that accepts `()`
/// (or anything else that implements `Default`) as an input type.
///
/// # Safety
///
/// This is a marker trait that tells the compiler that the coroutine's `Next`
/// associated type is `Self`. Functions that take a fixed point coroutine as
/// an input should typically use a constraint like the following:
///
/// ```rust
/// use cocoro::FixedPointCoro;
///
/// fn take_coro(k: impl FixedPointCoro<i32, (), ()>) {
///     k.into_iter().for_each(|i| {
///         println!("{i}");
///     });
/// }
/// ```
///
/// It should not ever be necessary to implement this trait manually, because
/// all `Coro` implementations that have `Next = Self` will automatically
/// implement `FixedPointCoro`.
pub unsafe trait FixedPointCoro<Y, R, I>:
    Coro<Y, R, I, Next = Self>
{
}

/// `FixedPointcoro`` is implemented automatically for all coroutines whose
/// `Next` associated type is `Self`. No other implementations should be
/// necessary, and the trait should not be implemented manually.
unsafe impl<Y, R, I, C> FixedPointCoro<Y, R, I> for C where
    C: Coro<Y, R, I, Next = C>
{
}
