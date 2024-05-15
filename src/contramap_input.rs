use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::Suspended;
use Suspend::Return;
use Suspend::Yield;

pub struct ContramapInput<Src, F>(Src, F);
impl<Src, F> ContramapInput<Src, F> {
    pub fn new(coro: Src, f: F) -> Self {
        ContramapInput(coro, f)
    }
}
impl<Y, R, I1, I2, Src, F> Coro<Y, R, I1> for ContramapInput<Src, F>
where
    Src: Coro<Y, R, I2>,
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
