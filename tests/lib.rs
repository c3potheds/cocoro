// Integration tests for the public API of cocoro.
//
// The cocoro crate is `no_std`, but this test crate may exercise integrations
// with `std` features, such as collecting into `Vec`s.

use std::ops::ControlFlow;

use cocoro::*;

fn iota() -> impl FixedPointCoro<(), i32, Void> {
    let mut i = 0;
    yield_with(move |_| {
        i += 1;
        i
    })
}

#[test]
fn into_iterator() {
    let actual = iota().into_iter().take(5).collect::<Vec<_>>();
    let expected = vec![1, 2, 3, 4, 5];
    assert_eq!(actual, expected);
}

#[test]
fn from_iterator() {
    fn make_coro() -> impl Coro<(), i32, ()> {
        [1, 2, 3].into_coro()
    }
    make_coro()
        .assert_yields(1, ())
        .assert_yields(2, ())
        .assert_yields(3, ())
        .assert_returns((), ());
}

#[test]
fn join_just_return_and_just_return() {
    just_return(1)
        .join(just_return(2))
        .yields::<Void>()
        .assert_returns((1, 2), ());
}

#[test]
fn join_just_yield_and_just_yield() {
    join(just_yield(1), just_yield(2))
        .returns::<(Void, Void)>()
        .assert_yields(1, ())
        .assert_yields(2, ())
        .assert_yields(1, ())
        .assert_yields(2, ());
}

#[test]
fn join_left_is_longer() {
    join(
        ["a1", "a2", "a3"].into_coro().map_return(|()| "A"),
        ["b1", "b2"].into_coro().map_return(|()| "B"),
    )
    .assert_yields("a1", ())
    .assert_yields("b1", ())
    .assert_yields("a2", ())
    .assert_yields("b2", ())
    .assert_yields("a3", ())
    .assert_returns(("A", "B"), ());
}

#[test]
fn join_right_is_longer() {
    join(
        ["a1", "a2"].into_coro().map_return(|()| "A"),
        ["b1", "b2", "b3"].into_coro().map_return(|()| "B"),
    )
    .assert_yields("a1", ())
    .assert_yields("b1", ())
    .assert_yields("a2", ())
    .assert_yields("b2", ())
    .assert_yields("b3", ())
    .assert_returns(("A", "B"), ());
}

#[test]
fn join_left_is_empty() {
    join(
        [].into_coro().map_return(|()| "A"),
        ["b1", "b2", "b3"].into_coro().map_return(|()| "B"),
    )
    .assert_yields("b1", ())
    .assert_yields("b2", ())
    .assert_yields("b3", ())
    .assert_returns(("A", "B"), ());
}

#[test]
fn join_right_is_empty() {
    join(
        ["a1", "a2", "a3"].into_coro().map_return(|()| "A"),
        [].into_coro().map_return(|()| "B"),
    )
    .assert_yields("a1", ())
    .assert_yields("a2", ())
    .assert_yields("a3", ())
    .assert_returns(("A", "B"), ());
}

#[test]
fn test_weave_cps_basic() {
    struct TakeWinner;
    impl WeaveConsumer<i32, i32, &'static str, &'static str> for TakeWinner {
        type Output = &'static str;

        fn on_left<B: Coro<i32, i32, &'static str>>(
            self,
            result: &'static str,
            _: B,
        ) -> &'static str {
            result
        }

        fn on_right<A: Coro<i32, i32, &'static str>>(
            self,
            result: &'static str,
            _: A,
        ) -> &'static str {
            result
        }
    }

    // Simple race between two algorithms
    let doubler = from_control_flow(|x: i32| {
        let next = x * 2;
        if next > 50 {
            ControlFlow::Break("doubler")
        } else {
            ControlFlow::Continue(next)
        }
    });

    let incrementer = from_control_flow(|x: i32| {
        let next = x + 7;
        if next > 50 {
            ControlFlow::Break("incrementer")
        } else {
            ControlFlow::Continue(next)
        }
    });

    let winner = weave_cps(doubler, incrementer, 5, TakeWinner);
    assert_eq!(winner, "doubler");
}

// Tests for weave_cps with flatten coroutines to confirm ICE fix
#[test]
fn test_weave_cps_with_flatten() {
    struct TakeWinner;
    impl WeaveConsumer<(), (), i32, i32> for TakeWinner {
        type Output = i32;
        fn on_left<B: Coro<(), (), i32>>(self, result: i32, _: B) -> i32 {
            result
        }
        fn on_right<A: Coro<(), (), i32>>(self, result: i32, _: A) -> i32 {
            result
        }
    }

    // Test flatten coroutine with weave_cps
    let flattened = just_return(just_return(100)).flatten();
    let simple = just_return(150);

    let winner = weave_cps(flattened, simple, (), TakeWinner);
    assert_eq!(winner, 100);
}

#[test]
fn test_weave_cps_with_flat_map() {
    struct TakeFirst;
    impl WeaveConsumer<(), (), &'static str, &'static str> for TakeFirst {
        type Output = &'static str;
        fn on_left<B: Coro<(), (), &'static str>>(
            self,
            result: &'static str,
            _: B,
        ) -> &'static str {
            result
        }
        fn on_right<A: Coro<(), (), &'static str>>(
            self,
            result: &'static str,
            _: A,
        ) -> &'static str {
            result
        }
    }

    // Test flat_map coroutine (which uses FlattenImpl internally) with weave_cps
    // Use simpler coroutines to avoid deep recursion in flat_map
    let flat_mapped =
        just_return("outer").flat_map(|_: &str| just_return("flat_mapped"));
    let competitor = just_return("competitor");

    let winner = weave_cps(flat_mapped, competitor, (), TakeFirst);
    assert_eq!(winner, "flat_mapped");
}

// Regression tests for flat_map and weave_cps interaction

#[test]
fn test_complex_flat_map_compilation() {
    // Ensures complex flat_map coroutines compile (regression test for
    // FlattenImpl visitor pattern)
    from_control_flow(|x: i32| {
        if x > 10 {
            ControlFlow::Break("done")
        } else {
            ControlFlow::Continue(x + 1)
        }
    })
    .flat_map(|_: &str| just_return("flat_mapped"))
    .assert_yields(2, 1)
    .assert_yields(3, 2)
    .assert_yields(4, 3)
    .assert_yields(5, 4)
    .assert_yields(6, 5)
    .assert_yields(7, 6)
    .assert_yields(8, 7)
    .assert_yields(9, 8)
    .assert_yields(10, 9)
    .assert_yields(11, 10)
    .assert_returns("flat_mapped", 11);
}

#[test]
fn test_complex_weave_cps_without_flat_map() {
    // Ensures complex coroutines work with weave_cps when not using flat_map
    let c1 = from_control_flow(|x: i32| {
        if x > 10 {
            ControlFlow::Break("c1")
        } else {
            ControlFlow::Continue(x + 1)
        }
    });

    let c2 = from_control_flow(|x: i32| {
        if x > 5 {
            ControlFlow::Break("c2")
        } else {
            ControlFlow::Continue(x + 2)
        }
    });

    let winner = weave_cps(c1, c2, 1, {
        struct TakeFirst;
        impl WeaveConsumer<i32, i32, &'static str, &'static str> for TakeFirst {
            type Output = &'static str;
            fn on_left<B: Coro<i32, i32, &'static str>>(
                self,
                result: &'static str,
                _: B,
            ) -> &'static str {
                result
            }
            fn on_right<A: Coro<i32, i32, &'static str>>(
                self,
                result: &'static str,
                _: A,
            ) -> &'static str {
                result
            }
        }
        TakeFirst
    });
    assert_eq!(winner, "c2");
}
