extern crate alloc;

use alloc::format;
use alloc::string::String;
use alloc::string::ToString;
use core::ops::ControlFlow;
use core::ops::ControlFlow::*;

use super::*;
use crate::Coro;
use crate::from_control_flow;

#[test]
fn just_yield_i32() {
    just_yield(10)
        .returns::<Void>()
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ());
}

#[test]
fn just_yield_i32_map_yield() {
    just_yield(10)
        .returns::<Void>()
        .map_yield(|x| x * 2)
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ());
}

#[test]
fn just_yield_i32_map_yield_map_twice() {
    just_yield(10)
        .returns::<Void>()
        .map_yield(|x| x * 2)
        .map_yield(|x| x + 1)
        .assert_yields(21, ())
        .assert_yields(21, ())
        .assert_yields(21, ())
        .assert_yields(21, ());
}

#[test]
fn just_yield_just_yield_i32() {
    just_yield(just_yield(10))
        .returns::<Void>()
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ());
}

#[test]
fn just_return_just_yield_flatten() {
    just_return(just_yield(10))
        .flatten()
        .returns::<Void>()
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ());
}

#[test]
fn just_return_just_yield_map_yield_flatten() {
    just_return(just_yield(10).returns::<()>().map_yield(|x: i32| x * 2))
        .flatten()
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ());
}

#[test]
fn yield_cumulative_length_of_inputs() {
    let mut length = 0;
    yield_with(|s: &str| {
        length += s.len();
        length
    })
    .returns::<Void>()
    .assert_yields(3, "foo")
    .assert_yields(6, "bar")
    .assert_yields(9, "baz");
}

#[test]
fn test_from_control_flow_simple_yield() {
    from_control_flow(|x| Continue(x * 2))
        .returns::<Void>()
        .assert_yields(2, 1)
        .assert_yields(4, 2)
        .assert_yields(6, 3); // This coro never returns
}

#[test]
fn test_from_control_flow_yield_then_break() {
    let mut count = 0;
    let coro = from_control_flow(move |x: i32| {
        use ControlFlow::*;
        count += 1;
        if count <= 2 {
            Continue(x + count)
        } else {
            Break("done")
        }
    });

    coro.assert_yields(1, 0) // input 0, count becomes 1, yields 0+1 = 1
        .assert_yields(3, 1) // input 1, count becomes 2, yields 1+2 = 3
        .assert_returns("done", 2); // input 2, count becomes 3, breaks
}

#[test]
fn test_from_control_flow_immediate_break() {
    // Specify Continue type for ControlFlow when it's not used.
    let coro = from_control_flow(|_x: ()| Break(42_i32)).yields::<Void>();
    coro.assert_returns(42, ());
}

#[test]
fn test_from_control_flow_non_trivial_types() {
    #[derive(Debug, PartialEq, Clone)]
    struct MyStruct {
        val: String,
        num: i32,
    }

    let mut counter = 0;
    let coro = from_control_flow(move |input_struct: MyStruct| {
        counter += 1;
        if counter <= 1 {
            ControlFlow::Continue(MyStruct {
                val: format!("{} processed", input_struct.val),
                num: input_struct.num + counter,
            })
        } else {
            ControlFlow::Break(format!("Final count: {counter}"))
        }
    });

    coro.assert_yields(
        MyStruct {
            val: "hello processed".to_string(),
            num: 10 + 1,
        },
        MyStruct {
            val: "hello".to_string(),
            num: 10,
        },
    )
    .assert_returns(
        "Final count: 2".to_string(),
        MyStruct {
            val: "world".to_string(),
            num: 20,
        },
    );
}

// Tests for the take() method
#[test]
fn test_take_fewer_than_available() {
    yield_with(|()| 1)
        .compose(take(3))
        .assert_yields(1, ())
        .assert_yields(1, ())
        .assert_yields(1, ())
        .assert_returns((), ());
}

#[test]
fn test_take_more_than_available() {
    from_fn(|()| Yield(1, from_fn(|()| Yield(2, just_return("done")))))
        .compose(take(5).map_return(|_| "break"))
        .assert_yields(1, ())
        .assert_yields(2, ())
        .assert_returns("done", ());
}

#[test]
fn test_take_zero_elements() {
    yield_with(|()| 1).compose(take(0)).assert_returns((), ());
}

#[test]
fn test_take_from_empty_coro_returns() {
    just_return("empty")
        .yields::<Void>()
        .compose(take(5).map_return(|_| "nonempty"))
        .assert_returns("empty", ());
}

#[test]
fn test_take_from_empty_coro_yields_nothing() {
    let coro = just_return("empty").yields::<Void>();
    coro.compose(take(5).map_return(|_| "nonempty"))
        .assert_returns("empty", ());
}

#[test]
fn iota_take_3() {
    let mut i = 0;
    yield_with(move |()| {
        i += 1;
        i
    })
    .compose(take(3))
    .assert_yields(1, ())
    .assert_yields(2, ())
    .assert_yields(3, ())
    .assert_returns((), ());
}

#[test]
fn map_yield_preserves_fixed_point() {
    // just_yield is a FixedPointCoro, and map_yield should preserve that
    use crate::just_yield::JustYield;

    let coro = <JustYield<i32> as Coro<(), i32, Void>>::map_yield(
        just_yield(42),
        |x| x * 2,
    );

    // The fixed_point() call will only compile if coro is FixedPointCoro
    // The fact that this compiles proves MapYield preserves FixedPointCoro
    FixedPointCoro::<(), i32, Void>::fixed_point(coro)
        .returns::<Void>()
        .assert_yields(84, ())
        .assert_yields(84, ());
}

#[test]
fn map_return_preserves_fixed_point() {
    // just_yield is a FixedPointCoro, and map_return should preserve that
    use crate::just_yield::JustYield;

    let coro = <JustYield<i32> as Coro<(), i32, ()>>::map_return(
        just_yield(42),
        |_| "done",
    );

    // The fixed_point() call will only compile if coro is FixedPointCoro
    // The fact that this compiles proves MapReturn preserves FixedPointCoro
    FixedPointCoro::<(), i32, &str>::fixed_point(coro)
        .returns::<&str>()
        .assert_yields(42, ())
        .assert_yields(42, ());
}

#[test]
fn compose_preserves_fixed_point() {
    // Both just_yield coroutines are FixedPointCoro
    use crate::just_yield::JustYield;

    let composed = <JustYield<i32> as Coro<(), i32, Void>>::compose(
        just_yield(10),
        just_yield(20),
    );

    // The fixed_point() call will only compile if composed is FixedPointCoro
    // The fact that this compiles proves Compose preserves FixedPointCoro
    // compose yields from the second coroutine first, then the first
    FixedPointCoro::<(), i32, Void>::fixed_point(composed)
        .returns::<Void>()
        .assert_yields(20, ())
        .assert_yields(20, ());
}

#[test]
fn contramap_input_preserves_fixed_point() {
    // just_yield is a FixedPointCoro, and contramap_input should preserve that
    use crate::just_yield::JustYield;

    let coro = <JustYield<i32> as Coro<(), i32, Void>>::contramap_input(
        just_yield(42),
        |(): ()| (),
    );

    // The fixed_point() call will only compile if coro is FixedPointCoro
    // The fact that this compiles proves ContramapInput preserves
    // FixedPointCoro
    FixedPointCoro::<(), i32, Void>::fixed_point(coro)
        .returns::<Void>()
        .assert_yields(42, ())
        .assert_yields(42, ());
}

#[test]
fn and_then_basic() {
    use crate::Suspend::Yield;

    just_return(5)
        .and_then(|n| Yield(n * 2, just_return(n * 3)))
        .assert_yields(10, ())
        .assert_returns(15, ());
}

#[test]
fn and_then_with_multiple_yields() {
    use crate::Suspend::Yield;

    just_return(3)
        .and_then(|n| {
            Yield(n, from_fn(move |_: ()| Yield(n * 2, just_return(n * 3))))
        })
        .assert_yields(3, ())
        .assert_yields(6, ())
        .assert_returns(9, ());
}

#[test]
fn and_then_chaining() {
    use crate::Suspend::Yield;

    // Chain multiple and_then calls
    just_return(2)
        .and_then(|n| Yield(n, just_return(n + 2)))
        .and_then(|n| Yield(n, just_return(n * 3)))
        .assert_yields(2, ())
        .assert_yields(4, ())
        .assert_returns(12, ());
}
