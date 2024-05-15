// Integration tests for the public API of cocoro.
//
// The cocoro crate is `no_std`, but this test crate may exercise integrations
// with `std` features, such as collecting into `Vec`s.

use cocoro::*;

fn iota() -> impl FixedPointCoro<i32, Void, ()> {
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
    fn make_coro() -> impl Coro<i32, (), ()> {
        vec![1, 2, 3].into_coro()
    }
    make_coro()
        .assert_yields(1, ())
        .assert_yields(2, ())
        .assert_yields(3, ())
        .assert_returns((), ());
}
