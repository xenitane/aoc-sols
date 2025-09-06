const LEN: usize = 2018;
const LEN_F: usize = 50000000;

pub fn solve(input: &str) -> Result<(usize, usize), ()> {
    let step: usize = input.parse().unwrap();
    let mut buffer = [0usize; LEN];
    let mut pos = 0usize;
    let mut filled = 1usize;
    while filled < LEN {
        pos = 1 + (pos + step) % filled;
        for i in (pos..filled).rev() {
            buffer[i + 1] = buffer[i];
        }
        buffer[pos] = filled;
        filled += 1;
    }
    let first = buffer[(pos + 1) % LEN];
    let second = {
        let mut res: usize = 0;
        while filled < LEN_F {
            pos = 1 + (pos + step) % filled;
            if pos == 1 {
                res = filled;
            }
            filled += 1;
        }
        res
    };
    Ok((first, second))
}
