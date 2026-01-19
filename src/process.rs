use core::marker::PhantomData;

use either::Either;

use crate::Coro;
use crate::FixedPointCoro;
use crate::Suspend;
use crate::Suspend::Return;
use crate::Suspend::Yield;
use crate::Suspended;
use crate::weave;

struct Weaving<Src, Processor, Cont, P> {
    src: Src,
    processor: Processor,
    cont: Cont,
    phantom: P,
}

impl<I, Y, A, X, R, Src, Worker, Start, Processor, Finish, Transition, Cont>
    Coro<A, X, R>
    for Weaving<
        Src,
        Processor,
        Cont,
        PhantomData<(I, Y, Worker, Start, Transition)>,
    >
where
    Src: FixedPointCoro<I, Y, R>,
    Worker: FixedPointCoro<Y, I, X>,
    Start: Suspended<Y, I, X, Next = Worker>,
    Processor: Coro<A, Start, R>,
    Finish: Coro<A, X, R>,
    Transition: Suspended<A, X, R, Next = Finish>,
    Cont: FnOnce(R, Worker) -> Transition,
{
    type Next = Either<
        Weaving<
            Src,
            Processor::Next,
            Cont,
            PhantomData<(I, Y, Worker, Start, Transition)>,
        >,
        Finish,
    >;
    type Suspend = Suspend<X, R, Self::Next>;
    fn resume(self, a: A) -> Self::Suspend {
        use Either::*;
        let Self {
            src,
            processor,
            cont,
            phantom,
        } = self;
        match processor.resume(a).into_enum() {
            Yield(start, processor) => match start.into_enum() {
                Yield(i, worker) => match weave(src, worker, i) {
                    Left((r, worker)) => match cont(r, worker).into_enum() {
                        Yield(x, finish) => Yield(x, Right(finish)),
                        Return(r) => Return(r),
                    },
                    Right((x, src)) => Yield(
                        x,
                        Left(Weaving {
                            src,
                            processor,
                            cont,
                            phantom,
                        }),
                    ),
                },
                Return(x) => Yield(
                    x,
                    Left(Weaving {
                        src,
                        processor,
                        cont,
                        phantom,
                    }),
                ),
            },
            Return(r) => Return(r),
        }
    }
}

pub fn process<
    I,
    Y,
    R,
    A,
    X,
    Src,
    Worker,
    Start,
    Processor,
    Finish,
    Transition,
>(
    src: Src,
    processor: Processor,
    cont: impl FnOnce(R, Worker) -> Transition,
) -> impl Coro<A, X, R>
where
    Src: FixedPointCoro<I, Y, R>,
    Worker: FixedPointCoro<Y, I, X>,
    Start: Suspended<Y, I, X, Next = Worker>,
    Processor: Coro<A, Start, R>,
    Finish: Coro<A, X, R>,
    Transition: Suspended<A, X, R, Next = Finish>,
{
    Weaving {
        src,
        processor,
        cont,
        phantom: PhantomData,
    }
}
