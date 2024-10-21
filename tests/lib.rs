// Integration tests for the public API of cocoro.
//
// The cocoro crate is `no_std`, but this test crate may exercise integrations
// with `std` features, such as collecting into `Vec`s.

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
