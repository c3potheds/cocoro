use either::Either;

use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::Suspended;

struct JoinL<K1, K2> {
    k1: K1,
    k2: K2,
}

struct JoinR<K1, K2> {
    k1: K1,
    k2: K2,
}

struct JoinGotL<R1, K2> {
    r1: R1,
    k2: K2,
}

struct JoinGotR<K1, R2> {
    k1: K1,
    r2: R2,
}

impl<I, Y, R1, R2, K1, K2> Coro<I, Y, (R1, R2)> for JoinL<K1, K2>
where
    I: Copy,
    K1: Coro<I, Y, R1>,
    K2: Coro<I, Y, R2>,
{
    type Next = Either<JoinR<K1::Next, K2>, JoinGotL<R1, K2::Next>>;
    type Suspend = Suspend<Y, (R1, R2), Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Either::{Left, Right};
        use Suspend::{Return, Yield};
        let JoinL { k1, k2 } = self;
        match k1.resume(input).into_enum() {
            Yield(y, k1) => Yield(y, Left(JoinR { k1, k2 })),
            Return(r1) => match k2.resume(input).into_enum() {
                Yield(y, k2) => Yield(y, Right(JoinGotL { r1, k2 })),
                Return(r2) => Return((r1, r2)),
            },
        }
    }
}

impl<I, Y, R1, R2, K1, K2> Coro<I, Y, (R1, R2)> for JoinR<K1, K2>
where
    I: Copy,
    K1: Coro<I, Y, R1>,
    K2: Coro<I, Y, R2>,
{
    type Next = Either<JoinL<K1, K2::Next>, JoinGotR<K1::Next, R2>>;
    type Suspend = Suspend<Y, (R1, R2), Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Either::{Left, Right};
        use Suspend::{Return, Yield};
        let JoinR { k1, k2 } = self;
        match k2.resume(input).into_enum() {
            Yield(y, k2) => Yield(y, Left(JoinL { k1, k2 })),
            Return(r2) => match k1.resume(input).into_enum() {
                Yield(y, k1) => Yield(y, Right(JoinGotR { k1, r2 })),
                Return(r1) => Return((r1, r2)),
            },
        }
    }
}

impl<I, Y, R1, R2, K2> Coro<I, Y, (R1, R2)> for JoinGotL<R1, K2>
where
    I: Copy,
    K2: Coro<I, Y, R2>,
{
    type Next = JoinGotL<R1, K2::Next>;
    type Suspend = Suspend<Y, (R1, R2), Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Suspend::{Return, Yield};
        let JoinGotL { r1, k2 } = self;
        match k2.resume(input).into_enum() {
            Yield(y, k2) => Yield(y, JoinGotL { r1, k2 }),
            Return(r2) => Return((r1, r2)),
        }
    }
}

impl<I, Y, R1, R2, K1> Coro<I, Y, (R1, R2)> for JoinGotR<K1, R2>
where
    I: Copy,
    K1: Coro<I, Y, R1>,
{
    type Next = JoinGotR<K1::Next, R2>;
    type Suspend = Suspend<Y, (R1, R2), Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use Suspend::{Return, Yield};
        let JoinGotR { k1, r2 } = self;
        match k1.resume(input).into_enum() {
            Yield(y, k1) => Yield(y, JoinGotR { k1, r2 }),
            Return(r1) => Return((r1, r2)),
        }
    }
}

pub fn join<I, Y, R1, R2, K1, K2>(k1: K1, k2: K2) -> impl Coro<I, Y, (R1, R2)>
where
    I: Copy,
    K1: Coro<I, Y, R1>,
    K2: Coro<I, Y, R2>,
{
    JoinL { k1, k2 }
}
