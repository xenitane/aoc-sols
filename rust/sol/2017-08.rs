#![allow(special_module_name)]

fn solve(input: &str) -> Result<(i32, i32), ()> {
    let instructions: Vec<_> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split_whitespace();
            let target_reg = tokens.next().unwrap();
            let amount = {
                let dir = tokens.next().unwrap();
                tokens.next().unwrap().parse::<i32>().unwrap() * if dir == "dec" { -1 } else { 1 }
            };
            tokens.next();
            let comp_reg = tokens.next().unwrap();
            let comp_op = tokens.next().unwrap();
            let comp_val: i32 = tokens.next().unwrap().parse().unwrap();
            (target_reg, amount, (comp_reg, comp_op, comp_val))
        })
        .collect();
    let mut second = i32::MIN;
    let first = {
        let mut map = std::collections::HashMap::new();
        for (target_reg, amount, (comp_reg, comp_op, comp_val)) in &instructions {
            if {
                let reg_val = map.get(comp_reg).unwrap_or(&0);
                match comp_op {
                    &"==" => reg_val == comp_val,
                    &"!=" => reg_val != comp_val,
                    &">" => reg_val > comp_val,
                    &">=" => reg_val >= comp_val,
                    &"<" => reg_val < comp_val,
                    &"<=" => reg_val <= comp_val,
                    _ => unreachable!(),
                }
            } {
                let reg_val = map.remove(target_reg).unwrap_or(0) + amount;
                second = if reg_val > second { reg_val } else { second };
                map.insert(target_reg, reg_val);
            }
        }
        map.iter()
            .fold(i32::MIN, |p, (_, v)| if *v > p { *v } else { p })
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
