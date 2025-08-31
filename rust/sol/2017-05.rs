#![allow(special_module_name)]

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let o_jumps: Vec<_> = input.lines().map(|v| v.parse::<i32>().unwrap()).collect();
    let len = o_jumps.len() as i32;

    let uo0 = |_: i32| 1;
    let uo1 = |offset: i32| if offset >= 3 { -1 } else { 1 };
    let uof = [uo0, uo1];

    let mut res = [0, 0];

    for (i, ff) in uof.iter().enumerate() {
        let mut jumps = o_jumps.clone();
        let mut idx = 0i32;
        let mut steps = 0u32;
        while idx >= 0 && idx < len {
            let offset = jumps[idx as usize];
            jumps[idx as usize] += ff(offset);
            idx += offset;
            steps += 1;
        }
        res[i] = steps;
    }

    Ok((res[0], res[1]))
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
