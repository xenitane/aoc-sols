fn is_caught(layers: &Vec<(u32, u32)>, delay: u32) -> bool {
    for (depth, range) in layers {
        if (depth + delay) % (2 * (range - 1)) == 0 {
            return true;
        }
    }
    false
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let layers: Vec<_> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ':']).filter(|s| !s.is_empty());
            let depth: u32 = tokens.next().unwrap().parse().unwrap();
            let range: u32 = tokens.next().unwrap().parse().unwrap();
            (depth, range)
        })
        .collect();
    let first = {
        let mut res = 0;
        for (depth, range) in &layers {
            if depth % (2 * (range - 1)) == 0 {
                res += depth * range;
            }
        }
        res
    };
    let second = {
        let mut delay = 0;
        while is_caught(&layers, delay) {
            delay += 1;
        }
        delay
    };
    Ok((first, second))
}
