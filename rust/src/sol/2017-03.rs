use std::collections::HashMap;

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let num: u32 = input.parse().unwrap();

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
        let mut hm = HashMap::from([(pos, 1u32)]);
        let res: u32;
        let next = |(x, y), mp: &HashMap<_, _>| {
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
            let meta_moves = [
                (step, 1, 0),
                (step, 0, -1),
                (step + 1, -1, 0),
                (step + 1, 0, 1),
            ];
            for (s, dx, dy) in meta_moves {
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
