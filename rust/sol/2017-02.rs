#![allow(special_module_name)]

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let (first, second) = input.lines().fold((0, 0), |(f_a, s_a), line| {
        let mut numbers = line
            .split_whitespace()
            .map(|num_str| num_str.parse::<u32>().unwrap())
            .collect::<Vec<_>>();
        numbers.sort();
        let l = numbers[0];
        let u = numbers[numbers.len() - 1];
        let mut quot = 0u32;
        'outer: for i in 1..numbers.len() {
            for j in 0..i {
                if numbers[i] % numbers[j] == 0 {
                    quot = numbers[i] / numbers[j];
                    break 'outer;
                }
            }
        }
        (f_a + u - l, s_a + quot)
    });
    Ok((first, second))
}

pub mod lib;
pub mod opts;

use lib::PrintablePair;
use std::io;

fn main() -> Result<(), ()> {
    let input = lib::read_entire_file(opts::INPUT_FILE_PATH)?;
    let res = solve(&input)?;
    res.print_to(&mut io::stdout())?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn aa() -> Result<(), ()> {
        let input = lib::read_entire_file(opts::TEST_INPUT_FILE_PATH)?;
        let expected = lib::read_entire_file(opts::TEST_OUTPUT_FILE_PATH)?;
        let res = solve(&input)?;
        let mut buffer = String::new();
        res.print_to(&mut buffer)?;
        assert_eq!(
            expected.trim(),
            buffer.trim(),
            r#"Expected:
--------------------------------
{}
--------------------------------
Actual:
--------------------------------
{}
--------------------------------
"#,
            expected.trim(),
            buffer.trim()
        );
        Ok(())
    }
}
