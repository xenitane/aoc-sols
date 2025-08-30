#![allow(special_module_name)]

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let len = input.len();
    let (first, second) = input
        .bytes()
        .enumerate()
        .fold((0, 0), |(f_a, s_a), (i, x)| {
            let n_f_a = f_a
                + if input.as_bytes()[(i + 1) % len] == x {
                    (x as u32) - 48
                } else {
                    0
                };
            let n_s_a = s_a
                + if input.as_bytes()[(i + len / 2) % len] == x {
                    x as u32 - 48
                } else {
                    0
                };
            (n_f_a, n_s_a)
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
