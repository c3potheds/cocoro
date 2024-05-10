use crate::coro::Coro;
use crate::suspended::Suspended;

impl<I, Y, R, N, F> Coro<Y, R, I> for F
where
    F: FnOnce(I) -> Suspended<Y, R, N>,
    N: Coro<Y, R, I>,
{
    type Next = N;
    fn resume(self, input: I) -> Suspended<Y, R, N> {
        self(input)
    }
}
