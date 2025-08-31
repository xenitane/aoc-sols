#![allow(special_module_name)]

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let num = input.parse::<u32>().unwrap();

    let first = if num == 1 {
        0
    } else {
        let k = {
            let s = num.isqrt();
            s / 2 - if s % 2 == 0 || s * s == num { 1 } else { 0 }
        };
        let r = (num - (2 * k + 1) * (2 * k + 1) - 1) % (2 * k + 2);
        k + 1 + if r < k { k - r } else { r - k }
    };
    let second = {
        let mut pos = (0i32, 0i32);
        let mut hm = std::collections::HashMap::from([(pos, 1u32)]);
        let res: u32;
        let next = |(x, y), mp: &std::collections::HashMap<_, _>| {
            let mut sum = 0u32;
            for dx in -1..=1 {
                for dy in -1..=1 {
                    sum += mp.get(&(x + dx, y + dy)).unwrap_or(&0u32);
                }
            }
            sum
        };
        let mut step = 1;
        'outer: loop {
            let aaa = [
                (step, 1, 0),
                (step, 0, -1),
                (step + 1, -1, 0),
                (step + 1, 0, 1),
            ];
            for (s, dx, dy) in aaa {
                for _ in 0..s {
                    pos = (pos.0 + dx, pos.1 + dy);
                    let val = next(pos, &hm);
                    if val > num {
                        res = val;
                        break 'outer;
                    }
                    hm.insert(pos, val);
                }
            }
            step += 2;
        }
        res
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
