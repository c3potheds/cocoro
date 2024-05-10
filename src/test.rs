use cocoro::*;

#[test]
fn just_yield_i32() {
    just_yield(10)
        .returns::<()>()
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ())
        .assert_yields(10, ());
}

#[test]
fn just_yield_i32_map_yield() {
    just_yield(10)
        .returns::<()>()
        .map_yield(|x| x * 2)
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ())
        .assert_yields(20, ());
}

#[test]
fn just_yield_i32_map_yield_map_twice() {
    just_yield(10)
        .returns::<()>()
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
        .returns::<()>()
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ())
        .assert_yields(just_yield(10), ());
}

#[test]
fn just_return_just_yield_flatten() {
    just_return(just_yield(10).returns::<()>())
        .flatten()
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
    .returns::<()>()
    .assert_yields(3, "foo")
    .assert_yields(6, "bar")
    .assert_yields(9, "baz");
}
