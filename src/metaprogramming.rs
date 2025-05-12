/// A trait implemented by all types, where `Self::Type` is `Self`.
///
/// This is used in trait bounds in generic functions to specify that a
/// generic type parameter is the same as another, effectively aiding type
/// inference or specialization.
///
/// For example, the `Coro::returns()` and `Coro::yields()` combinators use `Is`
/// to allow users to explicitly set a coroutine's yield or return type.
/// This is useful when type annotations on function parameters or return
/// types are insufficient to constrain a coroutine generic over its yield or
/// return types. For instance, `just_yield(10i32)` implements
/// `Coro<I, i32, R>` for any input `I` and any return type `R`. Calling
/// `just_yield(10i32).returns::<()>()` would result in a coroutine of type
/// `impl Coro<I, i32, ()>`. The `Is` trait is part of the mechanism that
/// propagates the specified type (e.g., `()`) to the `Coro` implementation.
pub trait Is {
    type Type;
}

impl<T> Is for T {
    type Type = T;
}
