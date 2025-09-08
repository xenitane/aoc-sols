const KNOT_HASH_PADDING: [usize; 5] = [17, 31, 73, 47, 23];
const ITERATIONS: usize = 64;

fn knot_hash(str: String) -> [bool; 128] {
    let lengths: Vec<_> = {
        let mut lengths: Vec<_> = str.bytes().map(|v| v as usize).collect();
        for nl in KNOT_HASH_PADDING {
            lengths.push(nl);
        }
        lengths
    };
    let mut seq: [u8; 256] = std::array::from_fn(|i| i as u8);
    let mut idx: usize = 0;
    let mut skips: usize = 0;
    for _ in 0..ITERATIONS {
        for len in &lengths {
            if *len != 1 && *len <= 256 {
                let mut temp_seq = [0u8; 256];
                for i in idx..256 {
                    temp_seq[i - idx] = seq[i];
                }
                for i in 0..idx {
                    temp_seq[i + 256 - idx] = seq[i];
                }
                for i in 0..(len / 2) {
                    temp_seq[i] ^= temp_seq[len - i - 1];
                    temp_seq[len - i - 1] ^= temp_seq[i];
                    temp_seq[i] ^= temp_seq[len - i - 1];
                }
                for i in idx..256 {
                    seq[i] = temp_seq[i - idx];
                }
                for i in 0..idx {
                    seq[i] = temp_seq[i + 256 - idx];
                }
            }
            idx = (idx + *len + skips) % 256;
            skips += 1;
        }
    }
    let mut res = [false; 128];
    for i in 0..16 {
        let mut k = 0;
        for j in 0..16 {
            k ^= seq[i * 16 + j];
        }
        for j in (0..8).rev() {
            res[i * 8 + j] = 1 == (k & 1);
            k >>= 1;
        }
    }
    res
}

fn falsify(i: usize, j: usize, grid: &mut [[bool; 128]; 128]) {
    if !grid[i][j] {
        return;
    }
    grid[i][j] = false;
    if i > 0 {
        falsify(i - 1, j, grid);
    }
    if j > 0 {
        falsify(i, j - 1, grid);
    }
    if i < 127 {
        falsify(i + 1, j, grid);
    }
    if j < 127 {
        falsify(i, j + 1, grid);
    }
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let mut hash_grid = [[false; 128]; 128];
    for i in 0..128 {
        let mut key = String::new();
        key.push_str(&input);
        key.push_str(&"-");
        key.push_str(&format!("{}", i));
        hash_grid[i] = knot_hash(key);
    }
    let first = {
        let mut res = 0;
        for i in 0..128 {
            for j in 0..128 {
                res += if hash_grid[i][j] { 1 } else { 0 }
            }
        }
        res
    };
    let second = {
        let mut res = 0;
        for i in 0..128 {
            for j in 0..128 {
                if hash_grid[i][j] {
                    res += 1;
                    falsify(i, j, &mut hash_grid);
                }
            }
        }
        res
    };
    Ok((first, second))
}
