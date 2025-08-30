#![allow(special_module_name)]

fn solve(input: &str) -> Result<((), ()), ()> {
    Ok(((), ()))
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
