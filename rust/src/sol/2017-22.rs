use std::collections::{HashMap, HashSet};

const ITERATIONS0: usize = 10_000;
const ITERATIONS1: usize = 100_000_00;

pub fn solve(input: &str) -> Result<(usize, usize), ()> {
    let first = {
        let mut res = 0usize;
        let mut infected = HashSet::new();
        let mut cursor = (0isize, 0isize);
        let mut dir = (-1isize, 0isize);
        input.lines().enumerate().for_each(|(i, line)| {
            cursor.0 += if i & 1 == 1 { 1 } else { 0 };
            line.bytes().enumerate().for_each(|(j, b)| {
                if i == 0 {
                    cursor.1 += if j & 1 == 1 { 1 } else { 0 };
                }
                if b == b'#' {
                    infected.insert((i as isize, j as isize));
                }
            })
        });
        for _ in 0..ITERATIONS0 {
            if infected.remove(&cursor) {
                dir = (dir.1, -dir.0);
            } else {
                infected.insert(cursor);
                dir = (-dir.1, dir.0);
                res += 1;
            }
            cursor.0 += dir.0;
            cursor.1 += dir.1;
        }
        res
    };
    let second = {
        let mut res = 0usize;
        let mut infected = HashMap::new();
        let mut cursor = (0isize, 0isize);
        let mut dir = (-1isize, 0isize);
        input.lines().enumerate().for_each(|(i, line)| {
            cursor.0 += if i & 1 == 1 { 1 } else { 0 };
            line.bytes().enumerate().for_each(|(j, b)| {
                if i == 0 {
                    cursor.1 += if j & 1 == 1 { 1 } else { 0 };
                }
                if b == b'#' {
                    infected.insert((i as isize, j as isize), 2usize);
                }
            })
        });
        for _ in 0..ITERATIONS1 {
            let new_state = (infected.remove(&cursor).unwrap_or(0) + 1) % 4;
            match new_state {
                0 => dir = (-dir.0, -dir.1),
                1 => dir = (-dir.1, dir.0),
                2 => res += 1,
                3 => dir = (dir.1, -dir.0),
                _ => unreachable!(),
            }
            infected.insert(cursor, new_state);
            cursor.0 += dir.0;
            cursor.1 += dir.1;
        }
        res
    };

    Ok((first, second))
}
