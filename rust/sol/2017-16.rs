use std::collections::HashMap;

const INS: &[u8] = "psx".as_bytes();

const LEN: usize = 16;
const ITERATIONS: usize = 1_000_000_000;

enum Instruction {
    Spin(usize),
    Swp0(usize, usize),
    Swp1(u8, u8),
}

fn make_instruction(i_str: &str) -> Instruction {
    let bts = i_str.as_bytes();
    match bts[0] {
        val if val == INS[1] => Instruction::Spin(i_str[1..].parse().unwrap()),
        val if val == INS[2] => {
            let poss: Vec<_> = i_str[1..]
                .split('/')
                .map(|v| v.parse::<usize>().unwrap())
                .collect();
            Instruction::Swp0(poss[0], poss[1])
        }
        val if val == INS[0] => Instruction::Swp1(bts[1], bts[3]),
        _ => unreachable!(),
    }
}

fn rot_right(seq: &mut [u8; LEN], shift: usize) {
    let mut temp_seq = [0u8; LEN];
    for i in 0..LEN {
        temp_seq[(i + shift) % LEN] = seq[i];
    }
    *seq = temp_seq;
}

fn apply_dance_moves(seq: &mut [u8; LEN], instructions: &Vec<Instruction>) {
    for instruction in instructions {
        match instruction {
            Instruction::Spin(x) => {
                rot_right(seq, *x);
            }
            Instruction::Swp0(p0, p1) => {
                seq[*p0] ^= seq[*p1];
                seq[*p1] ^= seq[*p0];
                seq[*p0] ^= seq[*p1];
            }
            Instruction::Swp1(b0, b1) => {
                for i in 0..LEN {
                    if seq[i] == *b0 {
                        seq[i] = *b1;
                    } else if seq[i] == *b1 {
                        seq[i] = *b0;
                    }
                }
            }
        }
    }
}

pub fn solve(input: &str) -> Result<(String, String), ()> {
    let instructions: Vec<_> = input.split(',').map(make_instruction).collect();
    let mut seq: [u8; LEN] = std::array::from_fn(|i| 97 + i as u8);
    let first = {
        apply_dance_moves(&mut seq, &instructions);
        match String::from_utf8(seq.to_vec()) {
            Ok(res) => res,
            Err(_) => String::new(),
        }
    };
    let second = {
        let mut mp = HashMap::from([(seq, 0usize)]);
        let mut perms = vec![seq];
        let mut i = 1usize;
        let r = loop {
            apply_dance_moves(&mut seq, &instructions);
            if let Some(x) = mp.get(&seq) {
                break x;
            }
            mp.insert(seq, i);
            perms.push(seq);
            i += 1;
        };
        let j = r + (ITERATIONS - r) % (i - r) - 1;
        let res_seq = perms[j];

        match String::from_utf8(res_seq.to_vec()) {
            Ok(res) => res,
            Err(_) => String::new(),
        }
    };
    Ok((first, second))
}
