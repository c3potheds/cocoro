use crate::coro::Coro;
use crate::suspended::Suspended;

/// Extension trait providing assertion methods for testing coroutines.
///
/// This trait is separate from [`Coro`] to keep the core trait focused on
/// essential operations. Import this trait in your tests to access assertion
/// methods:
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::Void;
/// use cocoro::yield_with;
///
/// let mut i = 0;
/// yield_with(|()| {
///     i += 1;
///     i
/// })
/// .returns::<Void>()
/// .assert_yields((), 1)
/// .assert_yields((), 2)
/// .assert_yields((), 3);
/// ```
pub trait CoroAssertions<I, Y, R>: Coro<I, Y, R> {
    /// Extracts the next yielded value from the coroutine and asserts that it
    /// is equal to the expected value. Panics if the coroutine returns instead
    /// of yielding or if the yielded value is not equal to the expected value.
    ///
    /// This is most useful for testing. Since it returns the next state of the
    /// coroutine after the assertion, it can be chained with other assertions
    /// like so:
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::CoroAssertions;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    ///
    /// let mut i = 0;
    /// yield_with(|()| {
    ///     i += 1;
    ///     i
    /// })
    /// .returns::<Void>()
    /// .assert_yields((), 1)
    /// .assert_yields((), 2)
    /// .assert_yields((), 3);
    /// ```
    ///
    /// The `input` parameter is passed to the `resume()` method of the
    /// coroutine to drive it.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::CoroAssertions;
    /// use cocoro::Void;
    /// use cocoro::yield_with;
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .returns::<Void>()
    /// .assert_yields("foo", 3)
    /// .assert_yields("bar", 6)
    /// .assert_yields("hello", 11);
    /// ```
    fn assert_yields(self, input: I, expected: Y) -> Self::Next
    where
        Y: PartialEq + core::fmt::Debug,
        R: core::fmt::Debug,
    {
        use crate::Return;
        use crate::Yield;

        match self.resume(input).into_enum() {
            Yield(actual, next) => {
                assert_eq!(
                    actual, expected,
                    "expected Yield({expected:?}), got Yield({actual:?})"
                );
                next
            }
            Return(actual) => {
                panic!("expected Yield({expected:?}), got Return({actual:?})")
            }
        }
    }

    /// Extracts the return value from the coroutine and asserts that it is
    /// equal to the expected value. Panics if the coroutine yields instead of
    /// returning or if the return value is not equal to the expected value.
    ///
    /// This is most useful for testing. This can be useful at the end of a
    /// chain of `assert_yields()` calls to ensure that the coroutine returns
    /// with the expected value at the end.
    ///
    /// The `input` parameter is passed to the `resume()` method of the
    /// coroutine to drive it.
    ///
    /// ```rust
    /// use cocoro::Coro;
    /// use cocoro::CoroAssertions;
    /// use cocoro::take;
    /// use cocoro::yield_with;
    ///
    /// let mut length = 0;
    /// yield_with(move |s: &str| {
    ///     length += s.len();
    ///     length
    /// })
    /// .compose(take(2))
    /// .assert_yields("foo", 3)
    /// .assert_yields("bar", 6)
    /// .assert_returns("hello", ());
    /// ```
    fn assert_returns(self, input: I, expected: R)
    where
        Y: core::fmt::Debug,
        R: PartialEq + core::fmt::Debug,
    {
        use crate::Return;
        use crate::Yield;

        match self.resume(input).into_enum() {
            Yield(actual, _) => {
                panic!("expected Return({expected:?}), got Yield({actual:?})")
            }
            Return(actual) => {
                assert_eq!(
                    actual, expected,
                    "expected Return({expected:?}), got Return({actual:?})"
                );
            }
        }
    }
}

/// Blanket implementation of [`CoroAssertions`] for all types implementing
/// [`Coro`].
impl<I, Y, R, T> CoroAssertions<I, Y, R> for T where T: Coro<I, Y, R> {}
