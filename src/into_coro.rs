use crate::coro::Coro;
use crate::suspend::Suspend;
use Suspend::{Return, Yield};

/// Implemented by types that can be converted into a coroutine.
///
/// Notably, all types that implement `IntoIterator` can be converted into a
/// coroutine that takes the unit type `()` as input, yields the elements of the
/// iterator, and returns `()` when the iterator is exhausted.
pub trait IntoCoro<Y, R, I> {
    type IntoCoro: Coro<Y, R, I>;
    fn into_coro(self) -> Self::IntoCoro;
}

impl<T, I: IntoIterator<Item = T>> IntoCoro<T, (), ()> for I {
    type IntoCoro = IteratorCoro<I::IntoIter>;
    fn into_coro(self) -> Self::IntoCoro {
        IteratorCoro(self.into_iter())
    }
}

pub struct IteratorCoro<I>(I);
impl<T, I: Iterator<Item = T>> Coro<T, (), ()> for IteratorCoro<I> {
    type Next = Self;
    type Suspend = Suspend<T, (), Self>;
    fn resume(mut self, _: ()) -> Self::Suspend {
        match self.0.next() {
            Some(x) => Yield(x, Self(self.0)),
            None => Return(()),
        }
    }
}
