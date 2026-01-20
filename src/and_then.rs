use core::marker::PhantomData;

use either::Either;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

/// A coroutine that sequences two coroutines with explicit control over the
/// boundary. When the first coroutine returns, a closure is called to produce
/// a `Suspended` state that determines what happens next.
///
/// This differs from `flat_map()` in that:
/// - The closure returns a `Suspended` rather than a `Coro`, giving explicit
///   control over the yield/return boundary
/// - The input to the first coroutine is not copied to the second - the
///   programmer explicitly controls what state the second coroutine starts in
/// - This preserves linearity of inputs, making it suitable for parsers and
///   other scenarios where each input should be consumed exactly once
pub struct AndThen<A, F, R, B> {
    first: A,
    continuation: F,
    _phantom: PhantomData<fn() -> (R, B)>,
}

impl<A, F, R, B> AndThen<A, F, R, B> {
    pub(crate) fn new(coro: A, f: F) -> Self {
        Self {
            first: coro,
            continuation: f,
            _phantom: PhantomData,
        }
    }
}

impl<I, Y, R, R2, A, F, S, B> Coro<I, Y, R2> for AndThen<A, F, R, B>
where
    A: Coro<I, Y, R>,
    F: FnOnce(R) -> S,
    S: Suspended<I, Y, R2, Next = B>,
    B: Coro<I, Y, R2>,
{
    type Next = Either<AndThen<A::Next, F, R, B>, B>;
    type Suspend = Suspend<Y, R2, Self::Next>;

    fn resume(self, input: I) -> Self::Suspend {
        use either::Left;
        use either::Right;

        use crate::Return;
        use crate::Yield;
        let Self {
            first,
            continuation,
            _phantom,
        } = self;

        // Resume the first coroutine
        match first.resume(input).into_enum() {
            // First coroutine yielded - keep going with it
            Yield(y, a_next) => Yield(
                y,
                Left(AndThen {
                    first: a_next,
                    continuation,
                    _phantom: PhantomData,
                }),
            ),
            // First coroutine returned - call the continuation function
            Return(r) => match continuation(r).into_enum() {
                // Continuation yielded - transition to second coroutine
                Yield(y, b) => Yield(y, Right(b)),
                // Continuation returned immediately - we're done
                Return(r2) => Return(r2),
            },
        }
    }
}
