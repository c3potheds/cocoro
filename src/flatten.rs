use crate::coro::Coro;
use crate::suspend::Suspend;
use crate::suspended::{Suspended, SuspendedVisitor};
use core::marker::PhantomData;
use either::Either;

pub struct FlattenImpl<Y, Outer, Inner> {
    outer: Outer,
    _phantom: core::marker::PhantomData<(Y, Inner)>,
}

impl<Y, Outer, Inner> FlattenImpl<Y, Outer, Inner> {
    pub fn new(outer: Outer) -> Self {
        FlattenImpl {
            outer,
            _phantom: core::marker::PhantomData,
        }
    }
}

impl<I, Y, R, Outer, Inner> Coro<I, Y, R> for FlattenImpl<Y, Outer, Inner>
where
    I: Copy,
    Outer: Coro<I, Y, Inner>,
    Inner: Coro<I, Y, R>,
{
    type Next = Either<FlattenImpl<Y, Outer::Next, Inner>, Inner::Next>;
    type Suspend = Suspend<Y, R, Self::Next>;
    fn resume(self, input: I) -> Self::Suspend {
        use either::{Left, Right};
        use Suspend::{Return, Yield};

        let Self { outer, .. } = self;

        struct OuterVisitor<I, Y, R, Inner> {
            input: I,
            _phantom: PhantomData<(Y, R, Inner)>,
        }

        impl<I, Y, R, Inner, Outer> SuspendedVisitor<I, Y, Inner, Outer>
            for OuterVisitor<I, Y, R, Inner>
        where
            I: Copy,
            Inner: Coro<I, Y, R>,
            Outer: Coro<I, Y, Inner>,
        {
            type Out = Suspend<
                Y,
                R,
                Either<FlattenImpl<Y, Outer, Inner>, Inner::Next>,
            >;

            fn on_yield(self, y: Y, next_outer: Outer) -> Self::Out {
                Yield(y, Left(FlattenImpl::new(next_outer)))
            }

            fn on_return(self, inner: Inner) -> Self::Out {
                struct InnerVisitor<I, Y, R, Outer, Inner> {
                    _phantom: PhantomData<(I, Y, R, Outer, Inner)>,
                }

                impl<I, Y, R, Outer, InnerCoro, InnerNext>
                    SuspendedVisitor<I, Y, R, InnerNext>
                    for InnerVisitor<I, Y, R, Outer, InnerCoro>
                where
                    I: Copy,
                    InnerNext: Coro<I, Y, R>,
                {
                    type Out = Suspend<
                        Y,
                        R,
                        Either<FlattenImpl<Y, Outer, InnerCoro>, InnerNext>,
                    >;

                    fn on_yield(
                        self,
                        y: Y,
                        next_inner: InnerNext,
                    ) -> Self::Out {
                        Yield(y, Right(next_inner))
                    }

                    fn on_return(self, r: R) -> Self::Out {
                        Return(r)
                    }
                }

                inner.resume(self.input).visit(InnerVisitor {
                    _phantom: PhantomData,
                })
            }
        }

        outer.resume(input).visit(OuterVisitor {
            input,
            _phantom: PhantomData,
        })
    }
}
