use crate::coro::Coro;
use crate::fixed_point::FixedPointCoro;
use crate::suspend::Suspend;

type RecursiveFn<'a, I, Y, R> =
    dyn Fn(Recursive<'a, I, Y, R>, I) -> Suspend<Y, R, Recursive<'a, I, Y, R>>;

pub struct Recursive<'a, I, Y, R>(&'a RecursiveFn<'a, I, Y, R>);
impl<I, Y, R> Coro<I, Y, R> for Recursive<'_, I, Y, R> {
    type Next = Self;
    type Suspend = Suspend<Y, R, Self>;
    fn resume(self, input: I) -> Self::Suspend {
        let Self(f) = self;
        f(Self(f), input)
    }
}

/// Creates a coroutine using a recursive function.
///
/// The function takes a wrapper of a reference to itself alongside the input
/// type to the coroutine, and returns either `Yield` with a yielded value and
/// the recursive wrapper, or `Return` with the return value.
///
/// The function must be `'static` because it is referenced as a trait object
/// to get around the limitations of
/// <https://github.com/rust-lang/rust/issues/97680>. Rust does not allow
/// closure objects to take themselves, or types that own references to
/// themselves, as inputs. However, it is possible for a closure to take a
/// reference to a `dyn Fn` object that happens to be itself.
///
/// This is also why `f` is passed by reference: the wrapped closure object must
/// not own the `Fn` because it must pass "itself" (actually a copy that wraps
/// the same reference to the `Fn`) to the function, and if the wrapper were to
/// own the `Fn`, then it would be impossible to invoke the `Fn` with the value
/// after the wrapper had been constructed.
///
/// Whereas `yield_with()` is useful for infinite coroutines that never return,
/// and `from_fn()` is useful for guaranteed-finite coroutines that must return
/// within a knowable finite number of steps, `recursive()` is useful for
/// creating coroutines that may return, but without a finite bound known at
/// compile time.
///
/// The result of `recursive()` is a `FixedPointCoro`, which means that its
/// `Next` type is itself. This allows the coroutine to be looped over
/// iteratively without recursion, by assigning the result of `resume()` back to
/// the coroutine variable when the `Suspend` result matches `Yield`. It also
/// enables methods like `into_iter()` when the input type `I` implements
/// `Default`.
///
/// # Example
///
/// ```rust
/// use cocoro::Coro;
/// use cocoro::CoroAssertions;
/// use cocoro::Return;
/// use cocoro::Void;
/// use cocoro::Yield;
/// use cocoro::recursive;
///
/// recursive(&|recur, n| Yield(n + 1, recur))
///     .returns::<Void>()
///     .assert_yields(1, 0)
///     .assert_yields(2, 1)
///     .assert_yields(3, 2);
/// ```
pub fn recursive<'a, I, Y, R, F>(f: &'a F) -> impl FixedPointCoro<I, Y, R> + 'a
where
    Y: 'a,
    R: 'a,
    I: 'a,
    F: Fn(Recursive<'_, I, Y, R>, I) -> Suspend<Y, R, Recursive<'_, I, Y, R>>
        + 'static,
{
    Recursive(f)
}
