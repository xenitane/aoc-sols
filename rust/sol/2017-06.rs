#![allow(special_module_name)]

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let o_banks: Vec<_> = input
        .split_whitespace()
        .map(|x| x.parse::<usize>().unwrap())
        .collect();
    let len = o_banks.len();

    let next_banks = |b: &mut Vec<usize>| {
        let mut idx = 0usize;
        let mut v = b[0];
        for i in 1..len {
            if b[i] > v {
                idx = i;
                v = b[i];
            }
        }
        b[idx] = 0;
        let q = v / len;
        let r = v % len;
        for i in 1..=len {
            b[(i + idx) % len] += q + if i <= (r as usize) { 1 } else { 0 };
        }
    };
    let vec_to_str = |v: &Vec<usize>| {
        let mut res = String::new();
        for x in v {
            res.push_str(&x.to_string());
            res.push_str(&",");
        }
        res
    };

    let (first, second) = {
        let mut steps = 0;
        let last_oc;
        let mut banks = o_banks.clone();
        let mut hm = std::collections::HashMap::new();

        loop {
            let vv = vec_to_str(&banks);
            if let Some(x) = hm.get(&vv) {
                last_oc = x;
                break;
            }
            hm.insert(vv, steps);
            steps += 1;
            next_banks(&mut banks);
        }
        (steps, steps - last_oc)
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
