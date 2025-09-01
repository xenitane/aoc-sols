#![allow(special_module_name)]

fn is_caught(layers: &Vec<(u32, u32)>, delay: u32) -> bool {
    for (depth, range) in layers {
        if (depth + delay) % (2 * (range - 1)) == 0 {
            return true;
        }
    }
    false
}

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let layers: Vec<_> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ':']).filter(|s| !s.is_empty());
            let depth: u32 = tokens.next().unwrap().parse().unwrap();
            let range: u32 = tokens.next().unwrap().parse().unwrap();
            (depth, range)
        })
        .collect();
    let first = {
        let mut res = 0;
        for (depth, range) in &layers {
            if depth % (2 * (range - 1)) == 0 {
                res += depth * range;
            }
        }
        res
    };
    let second = {
        let mut delay = 0;
        while is_caught(&layers, delay) {
            delay += 1;
        }
        delay
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
