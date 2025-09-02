const DELIMS: &[u8] = "{},<>!".as_bytes();

fn process_garbage(idx: &mut usize, input: &[u8], garbage: &mut u32) {
    if *idx == input.len() {
        return;
    }
    *idx += 1;
    while *idx < input.len() && input[*idx] != DELIMS[4] {
        if input[*idx] == DELIMS[5] {
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
    while *idx < input.len() && input[*idx] != DELIMS[1] {
        res += match input[*idx] {
            val if val == DELIMS[0] => calculate_score(idx, score + 1, input, garbage),
            val if val == DELIMS[2] => 0,
            val if val == DELIMS[3] => {
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
