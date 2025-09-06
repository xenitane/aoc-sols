pub fn solve(input: &str) -> Result<(i32, i32), ()> {
    let moves: Vec<_> = input.split(',').collect();
    let mut second = 0i32;
    let first = {
        let (mut x, mut y, mut z) = (0i32, 0i32, 0i32);
        for mv in &moves {
            match mv {
                &"n" => x += 1,
                &"s" => x -= 1,
                &"se" => y += 1,
                &"nw" => y -= 1,
                &"sw" => z += 1,
                &"ne" => z -= 1,
                _ => unreachable!(),
            }
            second = second.max(x.max(y).max(z) - x.min(y).min(z));
        }
        x.max(y).max(z) - x.min(y).min(z)
    };
    Ok((first, second))
}
