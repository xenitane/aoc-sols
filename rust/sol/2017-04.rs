#![allow(special_module_name)]

use std::iter::FromIterator;

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let is_valid_passphrase = |acc: (u32, u32), passphrase: &str| {
        let words: Vec<&str> = passphrase.split_whitespace().collect();
        let len = words.len();
        let st: std::collections::HashSet<&str> = words.into_iter().collect();
        let f_acc = acc.0 + if len == st.len() { 1 } else { 0 };
        let nst: std::collections::HashSet<String> = st
            .into_iter()
            .map(|ss| {
                let mut chars: Vec<_> = ss.chars().collect();
                chars.sort();
                String::from_iter(chars)
            })
            .collect();
        let s_acc = acc.1 + if len == nst.len() { 1 } else { 0 };
        (f_acc, s_acc)
    };
    let (first, second) = input.lines().fold((0, 0), is_valid_passphrase);
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
