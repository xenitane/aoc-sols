use std::collections::HashMap;

pub fn solve(input: &str) -> Result<(i32, i32), ()> {
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
        let mut map = HashMap::new();
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
