fn process_garbage(idx: &mut usize, input: &[u8], garbage: &mut u32) {
    if *idx == input.len() {
        return;
    }
    *idx += 1;
    while *idx < input.len() && input[*idx] != /* > */ 0x3e {
        if input[*idx] == /* ! */ 0x21 {
            *idx += 1;
        } else {
            *garbage += 1;
        }
        *idx += 1;
    }
}

fn calculate_score(idx: &mut usize, score: u32, input: &[u8], garbage: &mut u32) -> u32 {
    if *idx == input.len() {
        return 0;
    }

    *idx += 1;
    let mut res = score;
    while *idx < input.len() && input[*idx] != /* } */ 0x7d {
        res += match input[*idx] {
            /* { */ 0x7b => calculate_score(idx, score + 1, input, garbage),
            /* , */ 0x2c => 0,
            /* < */
            0x3c => {
                process_garbage(idx, input, garbage);
                0
            }
            _ => unreachable!(),
        };
        *idx += 1;
    }
    res
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let mut idx = 0;
    let mut second = 0;
    let first = calculate_score(&mut idx, 1, input.as_bytes(), &mut second);
    Ok((first, second))
}
