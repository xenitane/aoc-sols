pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let len = input.len();
    let (first, second) = input
        .bytes()
        .enumerate()
        .fold((0, 0), |(f_a, s_a), (i, x)| {
            let n_f_a = f_a
                + if input.as_bytes()[(i + 1) % len] == x {
                    (x - b'0') as u32
                } else {
                    0
                };
            let n_s_a = s_a
                + if input.as_bytes()[(i + len / 2) % len] == x {
                    (x - b'0') as u32
                } else {
                    0
                };
            (n_f_a, n_s_a)
        });

    Ok((first, second))
}
