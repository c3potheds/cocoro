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
#[cfg_attr(docsrs, doc(notable_trait))]
pub unsafe trait FixedPointCoro<I, Y, R>:
    Coro<I, Y, R, Next = Self>
{
    /// This method is a no-op, but is useful for compile-time debugging to
    /// ensure that the type is indeed a fixed-point coroutine. If you have a
    /// complex pipeline of combinators like `map_yield()` or `compose()`, and
    /// for some reason the final type is not `FixedPointCoro` as you expect,
    /// you can insert `.fixed_point()` calls in between the combinators to
    /// see where the property is lost.
    ///
    /// If you attempt to call this on a non-fixed-point coroutine, the compiler
    /// will raise an error looking like this:
    ///
    /// ```text
    ///  error[E0599]: the method `fixed_point` exists for opaque type
    /// `impl Coro<I, Y, R>`, but its trait bounds were not satisfied
    /// ```
    fn fixed_point(self) -> Self {
        self
    }
}

/// `FixedPointCoro` is implemented automatically for all coroutines whose
/// `Next` associated type is `Self`. No other implementations should be
/// necessary, and the trait should not be implemented manually.
unsafe impl<I, Y, R, C> FixedPointCoro<I, Y, R> for C where
    C: Coro<I, Y, R, Next = C>
{
}
