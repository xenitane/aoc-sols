pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let (first, second) = input.lines().fold((0, 0), |(f_a, s_a), line| {
        let mut numbers: Vec<_> = line
            .split_whitespace()
            .map(|num_str| num_str.parse::<u32>().unwrap())
            .collect();
        numbers.sort();
        let l = numbers[0];
        let u = numbers[numbers.len() - 1];
        let mut quot = 0u32;
        'outer: for i in 1..numbers.len() {
            for j in 0..i {
                if numbers[i] % numbers[j] == 0 {
                    quot = numbers[i] / numbers[j];
                    break 'outer;
                }
            }
        }
        (f_a + u - l, s_a + quot)
    });
    Ok((first, second))
}
