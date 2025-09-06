fn traverse_maze(
    maze: &Vec<&[u8]>,
    xy: (isize, isize),
    dir: (isize, isize),
    dims: (isize, isize),
    res0: &mut String,
    res1: &mut usize,
) {
    let (x, y) = xy;
    let (dx, dy) = dir;
    let (sx, sy) = (x as usize, y as usize);
    if x >= dims.0 || y >= dims.1 || x < 0 || y < 0 || maze[sy][sx] == b' ' {
        return;
    }
    *res1 += 1;
    let (new_xy, new_dir) = match maze[sy][sx] {
        b'-' | b'|' => ((x + dx, y + dy), dir),
        b'+' => {
            if dx == 0 {
                let ndy = if x + 1 >= dims.0 || maze[sy][sx + 1] == b' ' {
                    -1
                } else {
                    1
                };
                ((x + ndy, y), (ndy, 0))
            } else {
                let ndx = if y + 1 >= dims.1 || maze[sy + 1][sx] == b' ' {
                    -1
                } else {
                    1
                };
                ((x, y + ndx), (0, ndx))
            }
        }
        b'A'..=b'Z' => {
            res0.push(char::from(maze[sy][sx]));
            ((x + dx, y + dy), dir)
        }
        _ => unreachable!(),
    };
    traverse_maze(maze, new_xy, new_dir, dims, res0, res1);
}

pub fn solve(input: &str) -> Result<(String, usize), ()> {
    let maze: Vec<&[u8]> = input.lines().map(|line| line.as_bytes()).collect();
    let height = maze.len() as isize;
    let width = maze[0].len() as isize;
    let start_x = {
        let mut i = 0usize;
        loop {
            if maze[0][i] == b'|' {
                break i;
            }
            i += 1;
        }
    };
    let mut res0 = String::new();
    let mut res1 = 0usize;

    traverse_maze(
        &maze,
        (start_x as isize, 0),
        (0, 1),
        (width, height),
        &mut res0,
        &mut res1,
    );

    Ok((res0, res1))
}
