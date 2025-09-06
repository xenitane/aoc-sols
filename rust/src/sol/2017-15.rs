fn next(cur: u32, factor: u32) -> u32 {
    let mut res = 1u64;
    res *= cur as u64;
    res *= factor as u64;
    res %= 2147483647;
    res as u32
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let (af, bf) = (16807, 48271);
    let (a, b) = {
        let mut tokens = input
            .lines()
            .map(|line| line.split_whitespace().rev().next().unwrap());
        let a: u32 = tokens.next().unwrap().parse().unwrap();
        let b: u32 = tokens.next().unwrap().parse().unwrap();
        (a, b)
    };
    let first = {
        let (mut a, mut b) = (a, b);
        let mut res = 0;
        for _ in 0..40_000_000 {
            a = next(a, af);
            b = next(b, bf);
            if (a & 0xffff) == (b & 0xffff) {
                res += 1;
            }
        }
        res
    };
    let second = {
        let (mut a, mut b) = (a, b);
        let mut res = 0;
        for _ in 0..5_000_000 {
            loop {
                a = next(a, af);
                if (a % 4) == 0 {
                    break;
                }
            }
            loop {
                b = next(b, bf);
                if (b % 8) == 0 {
                    break;
                }
            }
            if (a & 0xffff) == (b & 0xffff) {
                res += 1;
            }
        }
        res
    };

    Ok((first, second))
}
