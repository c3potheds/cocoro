use crate::coro::Coro;
use crate::suspended::Suspended;
use Suspended::Yield;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct JustYield<T>(T);
impl<T, R, I> Coro<T, R, I> for JustYield<T>
where
    T: Copy,
{
    type Next = Self;
    fn resume(self, _: I) -> Suspended<T, R, Self> {
        Yield(self.0, self)
    }
}

pub fn just_yield<T>(t: T) -> JustYield<T> {
    JustYield(t)
}
