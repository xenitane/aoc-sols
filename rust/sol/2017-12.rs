#![allow(special_module_name)]

fn count_reachable(
    adj: &std::collections::HashMap<u32, Vec<u32>>,
    i: u32,
    vis: &mut std::collections::HashSet<u32>,
) -> u32 {
    if vis.contains(&i) {
        return 0;
    }
    vis.insert(i);
    let mut res = 1;
    let Some(x) = adj.get(&i) else { unreachable!() };
    for next in x {
        res += count_reachable(adj, *next, vis);
    }
    res
}

fn solve(input: &str) -> Result<(u32, u32), ()> {
    let adj: std::collections::HashMap<_, _> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ',']).filter(|s| !s.is_empty());
            let id: u32 = tokens.next().unwrap().parse().unwrap();
            tokens.next();
            let mut v = vec![];
            while let Some(t) = tokens.next() {
                v.push(t.parse::<u32>().unwrap());
            }
            (id, v)
        })
        .collect();
    let mut vis = std::collections::HashSet::new();
    let first = count_reachable(&adj, 0, &mut vis);
    let second = {
        let mut groups = 1;
        for (i, _) in adj.iter() {
            if count_reachable(&adj, *i, &mut vis) > 0 {
                groups += 1;
            }
        }
        groups
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
