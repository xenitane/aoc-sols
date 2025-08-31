#![allow(special_module_name)]

fn process_garbage(idx: &mut usize, input: &[u8], garbage: &mut u32) {
    if *idx == input.len() {
        return;
    }
    *idx += 1;
    while *idx < input.len() && input[*idx] != 0x3e {
        if input[*idx] == 0x21 {
            *idx += 2;
        } else {
            *garbage += 1;
            *idx += 1;
        }
    }
}

fn calculate_score(idx: &mut usize, score: u32, input: &[u8], garbage: &mut u32) -> u32 {
    if *idx == input.len() {
        return 0;
    }

    *idx += 1;
    let mut res = score;
    while *idx < input.len() && input[*idx] != 0x7d {
        res += match input[*idx] {
            0x7b => calculate_score(idx, score + 1, input, garbage),
            0x2c => 0,
            0x3c => {
                process_garbage(idx, input, garbage);
                0
            }
            _ => unreachable!(),
        };
        *idx += 1;
    }
    res
}

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let mut idx = 0;
    let mut second = 0;
    let first = calculate_score(&mut idx, 1, input.as_bytes(), &mut second);
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
