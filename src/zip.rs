use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

pub struct Zip<K1, K2>(K1, K2);

impl<K1, K2> Zip<K1, K2> {
    pub fn new(k1: K1, k2: K2) -> Self {
        Zip(k1, k2)
    }
}

impl<I, Y1, Y2, R, K1, K2> Coro<I, (Y1, Y2), R> for Zip<K1, K2>
where
    I: Copy,
    K1: Coro<I, Y1, R>,
    K2: Coro<I, Y2, R>,
{
    type Next = Zip<K1::Next, K2::Next>;
    type Suspend = Suspend<(Y1, Y2), R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Suspend::{Return, Yield};
        let Zip(a, b) = self;
        match a.resume(input).into_enum() {
            Yield(y1, next_a) => match b.resume(input).into_enum() {
                Yield(y2, next_b) => Yield((y1, y2), Zip(next_a, next_b)),
                Return(r) => Return(r),
            },
            Return(r) => Return(r),
        }
    }
}
