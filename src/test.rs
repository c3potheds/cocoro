extern crate alloc;
use super::*;
use alloc::string::{String, ToString};
use alloc::format;

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
    use crate::from_control_flow;
    use crate::Coro;
    use core::ops::ControlFlow;

    // Specify Break type for ControlFlow when it's not used.
    let coro = from_control_flow(|x: i32| ControlFlow::<Void, _>::Continue(x * 2));
    coro.assert_yields(2, 1)
        .assert_yields(4, 2)
        .assert_yields(6, 3); // This coro never returns
}

#[test]
fn test_from_control_flow_yield_then_break() {
    use crate::from_control_flow;
    use crate::Coro;
    use core::ops::ControlFlow;

    let mut count = 0;
    let coro = from_control_flow(move |x: i32| {
        count += 1;
        if count <= 2 {
            ControlFlow::Continue(x + count)
        } else {
            ControlFlow::Break("done".to_string())
        }
    });

    coro.assert_yields(0 + 1, 0) // input 0, count becomes 1, yields 0+1
        .assert_yields(1 + 2, 1) // input 1, count becomes 2, yields 1+2
        .assert_returns("done".to_string(), 2); // input 2, count becomes 3, breaks
}

#[test]
fn test_from_control_flow_immediate_break() {
    use crate::from_control_flow;
    use crate::Coro;
    use core::ops::ControlFlow;

    // Specify Continue type for ControlFlow when it's not used.
    let coro = from_control_flow(|_x: ()| ControlFlow::<_, Void>::Break(42_i32));
    coro.assert_returns(42, ());
}

#[test]
fn test_from_control_flow_non_trivial_types() {
    use crate::from_control_flow;
    use crate::Coro;
    use core::ops::ControlFlow;

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
            ControlFlow::Break(format!("Final count: {}", counter))
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
    let coro = yield_with(|()| 1).map_return(|_: Void| "never".to_string()); // Infinite sequence of 1s
    coro.take(3)
        .assert_yields(1, ())
        .assert_yields(1, ())
        .assert_yields(1, ())
        .assert_returns(None, ()); // Should return None because it completed by taking n elements
}

#[test]
fn test_take_more_than_available() {
    let coro = from_fn(|()| Yield(1, from_fn(|()| Yield(2, from_fn::<(), i32, &str, Void, _, _>(|()| Return("done"))))));
    coro.take(5)
        .assert_yields(1, ())
        .assert_yields(2, ())
        .assert_returns(Some("done"), ()); // Should return Some(R) because the underlying coro returned
}

#[test]
fn test_take_zero_elements() {
    let coro = yield_with(|()| 1).map_return(|_: Void| "never".to_string());
    coro.take(0).assert_returns(None, ());
}

#[test]
fn test_take_from_empty_coro_returns() {
    let coro = from_fn::<(), Void, &str, Void, _, _>(|()| Return("empty"));
    coro.take(5).assert_returns(Some("empty"), ());
}

#[test]
fn test_take_from_empty_coro_yields_nothing() {
    let coro = from_fn::<(), Void, &str, Void, _, _>(|()| Return("empty")).map_yield(|v: Void| v); // Yield type is Void
    coro.take(5).assert_returns(Some("empty"), ());
}

#[test]
fn iota_take_3() {
    let mut i = 0;
    yield_with(move |()| {
        i += 1;
        i
    })
    .map_return(|_: Void| "finished") // Added a return type to avoid Void issues with Option<R>
    .take(3)
    .assert_yields(1, ())
    .assert_yields(2, ())
    .assert_yields(3, ())
    .assert_returns(None, ());
}
