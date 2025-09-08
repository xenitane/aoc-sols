const ITERATIONS0: usize = 40_000_000;
const ITERATIONS1: usize = 5_000_000;
const MOD: u64 = 2147483647;
const FACTOR_A: u32 = 16807;
const FACTOR_B: u32 = 48271;

fn next(cur: u32, factor: u32) -> u32 {
    let mut res = 1u64;
    res *= cur as u64;
    res *= factor as u64;
    res %= MOD;
    res as u32
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
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
        for _ in 0..ITERATIONS0 {
            a = next(a, FACTOR_A);
            b = next(b, FACTOR_B);
            if (a & 0xffff) == (b & 0xffff) {
                res += 1;
            }
        }
        res
    };
    let second = {
        let (mut a, mut b) = (a, b);
        let mut res = 0;
        for _ in 0..ITERATIONS1 {
            loop {
                a = next(a, FACTOR_A);
                if (a & 3) == 0 {
                    break;
                }
            }
            loop {
                b = next(b, FACTOR_B);
                if (b & 7) == 0 {
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
