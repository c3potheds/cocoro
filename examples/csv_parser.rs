//! A streaming CSV parser example using cocoro coroutines.
//!
//! This example demonstrates how to use coroutines to parse CSV data
//! incrementally, yielding parsed records one at a time while maintaining
//! parser state.
//!
//! Key concepts demonstrated:
//! - Using `from_control_flow()` for stateful parsing
//! - `ControlFlow::Continue` vs `ControlFlow::Break` semantics  
//! - Closure state capture for maintaining parser state
//! - Lifetime annotations with `impl Trait + '_`
//! - Composing coroutines with combinators like `map_yield()`, `take()`,
//!   `for_each()`
//!
//! Run with: `cargo run --example csv_parser`

extern crate alloc;
use alloc::string::String;
use alloc::vec::Vec;
use core::ops::ControlFlow;

use cocoro::Coro;
use cocoro::Return;
use cocoro::Suspended;
use cocoro::Yield;
use cocoro::from_control_flow;

/// Represents a parsed CSV record
#[derive(Debug, Clone, PartialEq)]
pub struct CsvRecord {
    pub fields: Vec<String>,
    pub line_number: usize,
}

/// Creates a CSV parser coroutine that yields records and properly returns when
/// done.
pub fn parse_csv(input: &str) -> impl Coro<(), CsvRecord, ()> + '_ {
    // Use an iterator to consume characters one by one
    // The + '_ lifetime annotation allows borrowing from input
    let mut chars = input.chars();
    let mut line_number = 1;
    let mut current_field = String::new();
    let mut current_record = Vec::new();
    let mut in_quotes = false;

    from_control_flow(move |_: ()| {
        use ControlFlow::Break;
        use ControlFlow::Continue;
        loop {
            let ch = match chars.next() {
                Some(ch) => ch,
                None => {
                    // End of input - handle final record if needed
                    if !current_field.is_empty() || !current_record.is_empty() {
                        current_record.push(current_field.clone());
                        let record = CsvRecord {
                            fields: current_record.clone(),
                            line_number,
                        };
                        current_field.clear();
                        current_record.clear();
                        return Continue(record);
                    } else {
                        return Break(());
                    }
                }
            };

            match ch {
                '"' => {
                    in_quotes = !in_quotes;
                }
                ',' if !in_quotes => {
                    // End of field
                    current_record.push(current_field.clone());
                    current_field.clear();
                }
                '\n' if !in_quotes => {
                    // End of record - yield it and continue parsing
                    current_record.push(current_field.clone());
                    current_field.clear();

                    let record = CsvRecord {
                        fields: current_record.clone(),
                        line_number,
                    };

                    current_record.clear();
                    line_number += 1;

                    return Continue(record);
                }
                '\r' if !in_quotes => {
                    // Skip carriage returns
                }
                _ => {
                    current_field.push(ch);
                }
            }
        }
    })
}

fn main() {
    println!("CSV Parser Example using cocoro coroutines\n");

    // Example CSV data
    let csv_data = r#"Name,Age,City
"John Doe",30,New York
"Jane Smith",25,"Los Angeles"
Bob Johnson,35,Chicago
"Mary Wilson",28,Boston"#;

    println!("Input CSV data:");
    println!("{csv_data}\n");

    // Example 1: Simple usage with for_each combinator
    println!("Example 1: Basic parsing with for_each:");
    parse_csv(csv_data).for_each(|record| {
        println!("  Line {}: {:?}", record.line_number, record.fields);
    });
    println!("  Result: {:?}\n", ());

    // Example 2: Manual iteration showing proper control flow
    println!(
        "Example 2: Manual iteration with explicit yield/return handling:"
    );

    // Use a recursive function to handle the iteration without type issues
    fn manual_iterate<C: Coro<(), CsvRecord, ()>>(parser: C, count: usize) {
        let suspended = parser.resume(());
        match suspended.into_enum() {
            Yield(record, next) => {
                println!("  Record {}: {:?}", count + 1, record.fields);
                manual_iterate(next, count + 1);
            }
            Return(result) => {
                println!("  Parser finished with: {result:?}");
                println!("  Processed {count} records manually\n");
            }
        }
    }

    manual_iterate(parse_csv(csv_data), 0);

    // Example 3: Using combinators for filtering and transformation
    println!("Example 3: Combinator chaining - filtering people over 30:");

    // Transform records to include age info (pure transformation)
    let with_age_info = parse_csv(csv_data).map_yield(|record| {
        // Skip header line
        if record.line_number == 1 {
            return (record, None); // Header has no age
        }

        // Try to parse age from second field
        let age = if record.fields.len() >= 2 {
            record.fields[1].parse::<u32>().ok()
        } else {
            None
        };

        (record, age)
    });

    // Filter and perform I/O in the consumer
    with_age_info.for_each(|(record, age_opt)| {
        if let Some(age) = age_opt {
            if age > 30 {
                println!("  Over 30: {:?} (age: {})", record.fields, age);
            }
        }
    });
    println!("  Finished filtering records\n");

    // Example 4: Using take() combinator
    println!("Example 4: Using take() combinator to limit output:");
    match parse_csv(csv_data)
        .take(3) // Only take first 3 records
        .for_each(|record| {
            println!(
                "  Limited: Line {}: {:?}",
                record.line_number, record.fields
            );
        }) {
        Some(original_result) => {
            println!(
                "  Original parser would have returned: {original_result:?}"
            )
        }
        None => println!("  Stopped after taking 3 records"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_csv_parsing() {
        let csv = "a,b,c\n1,2,3";
        let parser = parse_csv(csv);

        let suspended = parser.resume(());
        if let Yield(record, next) = suspended.into_enum() {
            assert_eq!(record.fields, vec!["a", "b", "c"]);
            assert_eq!(record.line_number, 1);

            let suspended2 = next.resume(());
            if let Yield(record, next2) = suspended2.into_enum() {
                assert_eq!(record.fields, vec!["1", "2", "3"]);
                assert_eq!(record.line_number, 2);

                // Should return after the last record
                let suspended3 = next2.resume(());
                if let Return(result) = suspended3.into_enum() {
                    assert_eq!(result, ());
                }
            }
        }
    }

    #[test]
    fn test_quoted_fields() {
        let csv = r#""hello, world",test"#;
        let parser = parse_csv(csv);

        let suspended = parser.resume(());
        if let Yield(record, _) = suspended.into_enum() {
            assert_eq!(record.fields, vec!["hello, world", "test"]);
        }
    }

    #[test]
    fn test_for_each_combinator() {
        let csv = "a,b\n1,2\n3,4";
        let parser = parse_csv(csv);

        let mut records = Vec::new();
        let result = parser.for_each(|record| {
            records.push(record);
        });

        assert_eq!(records.len(), 3);
        assert_eq!(records[0].fields, vec!["a", "b"]);
        assert_eq!(records[1].fields, vec!["1", "2"]);
        assert_eq!(records[2].fields, vec!["3", "4"]);
        assert_eq!(result, ());
    }

    #[test]
    fn test_empty_csv() {
        let csv = "";
        let parser = parse_csv(csv);

        let suspended = parser.resume(());
        if let Return(result) = suspended.into_enum() {
            assert_eq!(result, ());
        }
    }

    #[test]
    fn test_take_combinator() {
        let csv = "a,b\n1,2\n3,4\n5,6";
        let parser = parse_csv(csv);

        let mut records = Vec::new();
        let result = parser.take(2).for_each(|record| {
            records.push(record);
        });

        assert_eq!(records.len(), 2);
        assert_eq!(records[0].fields, vec!["a", "b"]);
        assert_eq!(records[1].fields, vec!["1", "2"]);
        // Should return None because we stopped early
        assert_eq!(result, None);
    }

    #[test]
    fn test_parser_state_advancement() {
        // This test catches the stack overflow bug where the parser
        // gets stuck on the same record due to state not advancing
        let csv = "a,b\nc,d";
        let parser = parse_csv(csv);

        // Try to parse a few records manually
        let suspended1 = parser.resume(());
        if let Yield(record1, next) = suspended1.into_enum() {
            assert_eq!(record1.fields, vec!["a", "b"]);
            assert_eq!(record1.line_number, 1);

            let suspended2 = next.resume(());
            if let Yield(record2, next2) = suspended2.into_enum() {
                // If state advancement is broken, this will be the same record
                assert_eq!(record2.fields, vec!["c", "d"]);
                assert_eq!(record2.line_number, 2);
                assert_ne!(record1, record2); // Should be different records

                // Should finish after the second record
                let suspended3 = next2.resume(());
                if let Return(result) = suspended3.into_enum() {
                    assert_eq!(result, ());
                } else {
                    panic!("Expected parser to return after last record");
                }
            } else {
                panic!("Expected second yield, got return");
            }
        } else {
            panic!("Expected first yield, got return");
        }
    }

    #[test]
    fn test_infinite_loop_prevention() {
        // This test ensures the parser doesn't get stuck in an infinite loop
        // by limiting iterations and checking for progress
        let csv = "header\nrow1\nrow2";
        let parser = parse_csv(csv);

        let mut seen_records = Vec::new();
        let mut iterations = 0;
        let max_iterations = 10; // Safety limit

        // Use manual iteration to detect if we're stuck
        fn check_advancement<C: Coro<(), CsvRecord, ()>>(
            parser: C,
            seen_records: &mut Vec<CsvRecord>,
            iterations: &mut usize,
            max_iterations: usize,
        ) -> bool {
            if *iterations >= max_iterations {
                return false; // Hit safety limit - likely infinite loop
            }

            *iterations += 1;
            let suspended = parser.resume(());
            match suspended.into_enum() {
                Yield(record, next) => {
                    // Check if we've seen this exact record before
                    if seen_records.contains(&record) {
                        return false; // Duplicate record - parser is stuck
                    }
                    seen_records.push(record);
                    check_advancement(
                        next,
                        seen_records,
                        iterations,
                        max_iterations,
                    )
                }
                Return(_) => {
                    true // Successfully finished
                }
            }
        }

        let success = check_advancement(
            parser,
            &mut seen_records,
            &mut iterations,
            max_iterations,
        );

        assert!(success, "Parser got stuck or hit infinite loop");
        assert_eq!(seen_records.len(), 3); // Should have parsed 3 records
        assert!(
            iterations < max_iterations,
            "Parser took too many iterations"
        );

        // Verify records are actually different
        assert_eq!(seen_records[0].fields, vec!["header"]);
        assert_eq!(seen_records[1].fields, vec!["row1"]);
        assert_eq!(seen_records[2].fields, vec!["row2"]);

        // Verify line numbers advance
        assert_eq!(seen_records[0].line_number, 1);
        assert_eq!(seen_records[1].line_number, 2);
        assert_eq!(seen_records[2].line_number, 3);
    }
}
