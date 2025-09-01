#![allow(special_module_name)]

fn solve(input: &str) -> Result<(i32, i32), ()> {
    let moves: Vec<_> = input.split(',').collect();
    let mut second = 0i32;
    let first = {
        let (mut x, mut y, mut z) = (0i32, 0i32, 0i32);
        for mv in &moves {
            match mv {
                &"n" => x += 1,
                &"s" => x -= 1,
                &"se" => y += 1,
                &"nw" => y -= 1,
                &"sw" => z += 1,
                &"ne" => z -= 1,
                _ => unreachable!(),
            }
            second = second.max(x.max(y).max(z) - x.min(y).min(z));
        }
        x.max(y).max(z) - x.min(y).min(z)
    };
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
