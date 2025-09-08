use std::collections::HashMap;

fn make_mat(mat: &str) -> Vec<Vec<bool>> {
    mat.split('/')
        .map(|row| row.bytes().map(|b| b == b'#').collect::<Vec<bool>>())
        .collect()
}

fn flip(mat: &mut Vec<Vec<bool>>) {
    let len = mat.len();
    for i in 0..len {
        for j in 0..(len / 2) {
            mat[i][j] ^= mat[i][len - j - 1];
            mat[i][len - j - 1] ^= mat[i][j];
            mat[i][j] ^= mat[i][len - j - 1];
        }
    }
}

fn transpose(mat: &mut Vec<Vec<bool>>) {
    let len = mat.len();
    for i in 1..len {
        for j in 0..i {
            mat[i][j] ^= mat[j][i];
            mat[j][i] ^= mat[i][j];
            mat[i][j] ^= mat[j][i];
        }
    }
}

fn all_variants(mat: Vec<Vec<bool>>) -> Vec<Vec<Vec<bool>>> {
    let mut res = vec![mat];
    for i in 0..7 {
        let mut nm = res[i].clone();
        if i & 1 == 1 {
            flip(&mut nm);
        } else {
            transpose(&mut nm);
        }
        res.push(nm);
    }
    res
}

fn parse_enhancer(line: &str) -> (Vec<Vec<Vec<bool>>>, Vec<Vec<bool>>) {
    let mut tokens = line.split_whitespace();
    let input_str = tokens.next().unwrap();
    tokens.next();
    let output_str = tokens.next().unwrap();

    (all_variants(make_mat(input_str)), make_mat(output_str))
}

const ITERATIONS0: usize = if cfg!(test) { 2 } else { 5 };
const ITERATIONS1: usize = if cfg!(test) { 0 } else { 13 };

fn enhance_mat(
    state: &mut Vec<Vec<bool>>,
    size: &mut usize,
    enhancers: &HashMap<Vec<Vec<bool>>, Vec<Vec<bool>>>,
    iterations: usize,
) {
    for _ in 0..iterations {
        let step = if *size % 2 == 0 { 2 } else { 3 };
        *state = state
            .chunks_exact(step)
            .flat_map(|rows| {
                let mut res = vec![vec![]; step + 1];

                {
                    let mut zzz: Vec<_> = rows.iter().map(|row| row.chunks_exact(step)).collect();
                    loop {
                        let mut mini_mat = vec![];
                        for zz in &mut zzz {
                            if let Some(x) = zz.next() {
                                mini_mat.push(Vec::from(x));
                            } else {
                                break;
                            }
                        }
                        if let Some(x) = enhancers.get(&mini_mat) {
                            let mut i = 0usize;
                            for xx in x {
                                res[i].append(&mut xx.clone());
                                i += 1;
                            }
                        } else {
                            break;
                        }
                    }
                }

                res
            })
            .collect::<Vec<_>>();
        *size = (step + 1) * *size / step;
    }
}

pub fn solve(input: &str) -> Result<(usize, usize), ()> {
    let enhancers: HashMap<_, _> = input
        .lines()
        .flat_map(|line| {
            let (a, b) = parse_enhancer(line);
            let mut v = vec![];
            for i in a {
                v.push((i, b.clone()));
            }
            v
        })
        .collect();
    let mut state: Vec<Vec<bool>> = vec![
        vec![false, true, false],
        vec![false, false, true],
        vec![true, true, true],
    ];
    let mut size = 3usize;

    enhance_mat(&mut state, &mut size, &enhancers, ITERATIONS0);
    let first = state.iter().fold(0, |acc, row| {
        acc + row
            .iter()
            .fold(0, |acc, cell| acc + if *cell { 1 } else { 0 })
    });
    enhance_mat(&mut state, &mut size, &enhancers, ITERATIONS1);
    let second = state.iter().fold(0, |acc, row| {
        acc + row
            .iter()
            .fold(0, |acc, cell| acc + if *cell { 1 } else { 0 })
    });
    Ok((first, second))
}
