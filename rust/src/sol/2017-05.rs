pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let o_jumps: Vec<_> = input.lines().map(|v| v.parse::<i32>().unwrap()).collect();
    let len = o_jumps.len() as i32;

    let uo0 = |_: i32| 1;
    let uo1 = |offset: i32| if offset >= 3 { -1 } else { 1 };
    let update_offset_fns = [uo0, uo1];

    let mut res = [0, 0];

    for (i, ff) in update_offset_fns.iter().enumerate() {
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
