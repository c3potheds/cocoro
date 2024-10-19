use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

pub struct Zip<K1, K2>(K1, K2);

impl<K1, K2> Zip<K1, K2> {
    pub fn new(k1: K1, k2: K2) -> Self {
        Zip(k1, k2)
    }
}

impl<I1, I2, Y1, Y2, R, K1, K2> Coro<(I1, I2), (Y1, Y2), R> for Zip<K1, K2>
where
    K1: Coro<I1, Y1, R>,
    K2: Coro<I2, Y2, R>,
{
    type Next = Zip<K1::Next, K2::Next>;
    type Suspend = Suspend<(Y1, Y2), R, Self::Next>;
    fn resume(self, input: (I1, I2)) -> Self::Suspend {
        use Suspend::{Return, Yield};
        let Zip(a, b) = self;
        let (i1, i2) = input;
        match a.resume(i1).into_enum() {
            Yield(y1, next_a) => match b.resume(i2).into_enum() {
                Yield(y2, next_b) => Yield((y1, y2), Zip(next_a, next_b)),
                Return(r) => Return(r),
            },
            Return(r) => Return(r),
        }
    }
}
