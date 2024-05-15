/// A trait that is implemented by every type, with an associated type that
/// is the type itself. This is used in trait bounds in generic functions to
/// specify that some generic type parameter is the same type as some other
/// generic type parameter.
///
/// In particular, this is used by `returns()` and `yields()` combinators on
/// the `Coro` trait to allow the user to specify the return type or yield type
/// of a coroutine when type annotations on function parameters or return types
/// cannot be used to constrain a coroutine that is generic over yield or
/// return types. For example, `just_yields(10i32)` implements `Coro<i32, R, I>`
/// for all possible `R` and `I`, but `just_yields(10i32).returns::<i32>()` will
/// be treated as a `Coro<i32, i32, I>`, and the mechanism by which the type
/// provided to `returns()` is propagated to the `Coro` implementation is by
/// using the `Is` trait.
pub trait Is {
    type Type;
}

impl<T> Is for T {
    type Type = T;
}
