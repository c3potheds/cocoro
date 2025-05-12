use crate::coro::Coro;

/// A refinement of the `Coro` trait that specifies that the `Next` associated
/// type must be `Self`.
///
/// Implementing `FixedPointCoro` allows algorithms that require in-place
/// mutation of a coroutine, such as creating an iterator. For example, an
/// iterator can be constructed from any `FixedPointCoro` that accepts `()`
/// (or anything else that implements `Default`) as an input type.
///
/// # Safety
///
/// This is a marker trait that tells the compiler that the coroutine's `Next`
/// associated type is `Self`. Functions that take a fixed-point coroutine as
/// input should typically use a constraint like this:
///
/// ```rust
/// use cocoro::FixedPointCoro;
///
/// fn take_coro(k: impl FixedPointCoro<(), i32, ()>) {
///     k.into_iter().for_each(|i| {
///         println!("{i}");
///     });
/// }
/// ```
///
/// It should never be necessary to implement this trait manually, as all
/// `Coro` implementations where `Next = Self` will automatically implement
/// `FixedPointCoro`.
pub unsafe trait FixedPointCoro<I, Y, R>:
    Coro<I, Y, R, Next = Self>
{
}

/// `FixedPointCoro` is implemented automatically for all coroutines whose
/// `Next` associated type is `Self`. No other implementations should be
/// necessary, and the trait should not be implemented manually.
unsafe impl<I, Y, R, C> FixedPointCoro<I, Y, R> for C where
    C: Coro<I, Y, R, Next = C>
{
}
