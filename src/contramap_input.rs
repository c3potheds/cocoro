use Suspend::Return;
use Suspend::Yield;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;

pub struct ContramapInput<Src, F>(Src, F);
impl<Src, F> ContramapInput<Src, F> {
    pub fn new(coro: Src, f: F) -> Self {
        ContramapInput(coro, f)
    }
}
impl<I1, Y, R, I2, Src, F> Coro<I1, Y, R> for ContramapInput<Src, F>
where
    Src: Coro<I2, Y, R>,
    F: FnMut(I1) -> I2,
{
    type Next = ContramapInput<Src::Next, F>;
    type Suspend = Suspend<Y, R, Self::Next>;
    fn resume(self, input: I1) -> Self::Suspend {
        let Self(coro, mut f) = self;
        match coro.resume(f(input)).into_enum() {
            Yield(y, next) => Yield(y, ContramapInput(next, f)),
            Return(r) => Return(r),
        }
    }
}
