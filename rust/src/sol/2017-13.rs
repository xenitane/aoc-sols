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
            let tokens: Vec<_> = line
                .split(&[' ', ':'])
                .filter(|s| !s.is_empty())
                .map(|v| v.parse::<u32>().unwrap())
                .collect();
            (tokens[0], tokens[1])
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
