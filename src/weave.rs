use crate::{suspended::Suspended, FixedPointCoro};
use either::Either;

/// Weaves two fixed-point coroutines together bidirectionally, feeding the
/// yields of one as inputs to the other until one of them returns.
///
/// This function executes a bidirectional composition between two coroutines:
/// - `coro_a` takes inputs of type `I` and yields values of type `Y`
/// - `coro_b` takes inputs of type `Y` and yields values of type `I`
///
/// The weaving process:
/// 1. Feeds `initial_input` to `coro_a`
/// 2. If `coro_a` yields `y`, passes it to `coro_b`
/// 3. If `coro_b` yields `i`, passes it back to `coro_a`
/// 4. Continues until either coroutine returns
///
/// # Returns
///
/// An `Either` indicating which coroutine completed first:
/// - `Either::Left((r1, remaining_b))` if `coro_a` returned `r1` first
/// - `Either::Right((r2, remaining_a))` if `coro_b` returned `r2` first
///
/// The "remaining" coroutine is left in whatever state it was in when the
/// other coroutine completed, allowing it to potentially be resumed later.
///
/// # Requirements
///
/// Both coroutines must be fixed-point coroutines (`FixedPointCoro`), meaning
/// their `Next` type is `Self`. This constraint is necessary for the result
/// type of the remainder coroutines to be well-defined. If either coroutine
/// were not fixed-point, then it would not be known whether the remainder of
/// that coroutine, at the time the other returns, would be e.g. `A`, or
/// `A::Next`, or `A::Next::Next`, etc.
///
/// # Examples
///
/// ## Producer-consumer with backpressure
///
/// ```rust
/// use cocoro::{from_control_flow, weave, yield_with, FixedPointCoro, Void};
/// use core::ops::ControlFlow;
/// use either::Either;
///
/// // Data producer that yields items when requested
/// fn data_producer() -> impl FixedPointCoro<(), &'static str, Void> {
///     let items = ["task1", "task2", "task3", "task4"];
///     let mut index = 0;
///     yield_with(move |()| {
///         let item = items[index % items.len()];
///         index += 1;
///         item
///     })
/// }
///
/// // Worker that processes items and signals readiness for more
/// fn worker() -> impl FixedPointCoro<&'static str, (), &'static str> {
///     let mut processed = 0;
///     from_control_flow(move |task: &str| {
///         processed += 1;
///         // Simulate processing time/cost - stop after 3 items
///         if processed >= 3 {
///             ControlFlow::Break("worker finished")
///         } else {
///             // Signal ready for next item
///             ControlFlow::Continue(())
///         }
///     })
/// }
///
/// // Because the producer never returns, the worker will always finish first
/// let Either::Right((msg, _producer)) = weave(data_producer(), worker(), ());
/// assert_eq!(msg, "worker finished");
/// ```
///
/// ## Request-response protocol
///
/// ```rust
/// use cocoro::{from_control_flow, weave, FixedPointCoro};
/// use core::ops::ControlFlow;
/// use either::Either;
///
/// #[derive(PartialEq, Debug)]
/// enum Request {
///     Get(u32),
///     Put(u32, &'static str),
/// }
///
/// #[derive(PartialEq, Debug)]
/// enum Response {
///     Value(Option<&'static str>),
///     Ok,
///     Error(&'static str),
/// }
///
/// // Client that sends requests and processes responses
/// fn client() -> impl FixedPointCoro<Response, Request, &'static str> {
///     let mut step = 0;
///     from_control_flow(move |response: Response| {
///         match step {
///             0 => {
///                 // First request: put a value
///                 step = 1;
///                 ControlFlow::Continue(Request::Put(42, "hello"))
///             }
///             1 => {
///                 // Expect OK response, then send get request
///                 if response == Response::Ok {
///                     step = 2;
///                     ControlFlow::Continue(Request::Get(42))
///                 } else {
///                     ControlFlow::Break("expected Ok response")
///                 }
///             }
///             2 => {
///                 // Process the retrieved value
///                 if response == Response::Value(Some("hello")) {
///                     ControlFlow::Break("success")
///                 } else {
///                     ControlFlow::Break("unexpected value")
///                 }
///             }
///             _ => ControlFlow::Break("protocol error"),
///         }
///     })
/// }
///
/// // Server that processes requests and sends responses
/// fn server() -> impl FixedPointCoro<Request, Response, &'static str> {
///     let mut storage = None;
///     from_control_flow(move |request: Request| match request {
///         Request::Put(42, value) => {
///             storage = Some(value);
///             ControlFlow::Continue(Response::Ok)
///         }
///         Request::Get(42) => ControlFlow::Continue(Response::Value(storage)),
///         _ => ControlFlow::Break("server error"),
///     })
/// }
///
/// // Start with a dummy response to kick off the client state machine
/// match weave(client(), server(), Response::Error("")) {
///     Either::Left((msg, _)) => assert_eq!(msg, "success"),
///     Either::Right((msg, _)) => {
///         panic!("server should not finish first: {}", msg)
///     }
/// }
/// ```
///
/// ## Filtering with early termination
///
/// ```rust
/// use cocoro::{from_control_flow, weave, yield_with, FixedPointCoro, Void};
/// use core::ops::ControlFlow;
/// use either::Either;
///
/// // Stream of data items
/// fn data_stream() -> impl FixedPointCoro<(), i32, Void> {
///     let items = [1, 7, 3, 12, 5, 8, 15, 2];
///     let mut index = 0;
///     yield_with(move |()| {
///         let item = items[index % items.len()];
///         index += 1;
///         item
///     })
/// }
///
/// // Filter that stops when it finds the first item > 10
/// fn find_first_large() -> impl FixedPointCoro<i32, (), i32> {
///     from_control_flow(move |item: i32| {
///         if item > 10 {
///             ControlFlow::Break(item)  // Found it!
///         } else {
///             ControlFlow::Continue(())  // Keep searching
///         }
///     })
/// }
///
/// // Since the data stream never returns, the filter will always finish first
/// let data_stream = data_stream();
/// let Either::Right((item, data_stream)) = weave(data_stream, find_first_large(), ());
/// assert_eq!(item, 12);  // First item > 10 in the sequence
/// // And you can do it again with the remaining data stream:
/// let Either::Right((item, _)) = weave(data_stream, find_first_large(), ());
/// assert_eq!(item, 15);  // Next item > 10 in the remaining sequence
/// ```
///
/// ## Predicate-based filtering
///
/// ```rust
/// use cocoro::{from_control_flow, weave, yield_with, FixedPointCoro, Void};
/// use core::ops::ControlFlow;
/// use either::Either;
///
/// // Source of strings
/// fn string_source() -> impl FixedPointCoro<(), &'static str, Void> {
///     let words = ["apple", "banana", "apricot", "cherry", "avocado"];
///     let mut index = 0;
///     yield_with(move |()| {
///         let word = words[index % words.len()];
///         index += 1;
///         word
///     })
/// }
///
/// // Collector that gathers words starting with 'a' until it has 3
/// fn collect_a_words(
/// ) -> impl FixedPointCoro<&'static str, (), Vec<&'static str>> {
///     let mut collected = Vec::new();
///     from_control_flow(move |word: &str| {
///         if word.starts_with('a') {
///             collected.push(word);
///             if collected.len() >= 3 {
///                 ControlFlow::Break(collected.clone())
///             } else {
///                 ControlFlow::Continue(())
///             }
///         } else {
///             ControlFlow::Continue(()) // Skip non-matching items
///         }
///     })
/// }
///
/// // The collector will finish first with exactly 3 'a' words
/// let Either::Right((words, _source)) =
///     weave(string_source(), collect_a_words(), ());
/// assert_eq!(words, vec!["apple", "apricot", "avocado"]);
/// ```
pub fn weave<A, B, I, Y, R1, R2>(
    coro_a: A,
    coro_b: B,
    initial_input: I,
) -> Either<(R1, B), (R2, A)>
where
    A: FixedPointCoro<I, Y, R1>,
    B: FixedPointCoro<Y, I, R2>,
{
    use crate::{Return, Yield};
    use Either::{Left, Right};
    match coro_a.resume(initial_input).into_enum() {
        Yield(y, next_a) => match coro_b.resume(y).into_enum() {
            Yield(i, next_b) => weave(next_a, next_b, i),
            Return(r2) => Right((r2, next_a)),
        },
        Return(r1) => Left((r1, coro_b)),
    }
}
