fn knot_hash(seq: &mut [u16; 256], lengths: &[usize], rounds: usize) {
    let mut idx: usize = 0;
    let mut skips: usize = 0;
    for _ in 0..rounds {
        for len in lengths {
            if *len != 1 && *len <= 256 {
                let mut temp_seq = [0u16; 256];
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
}

pub fn solve(input: &str) -> Result<(u16, String), ()> {
    let first = {
        let mut seq: [u16; 256] = std::array::from_fn(|i| i as u16);
        let lengths: Vec<_> = input
            .split(',')
            .map(|v| v.parse::<usize>().unwrap())
            .collect();
        knot_hash(&mut seq, &lengths, 1);
        seq[0] * seq[1]
    };
    let second = {
        let mut seq: [u16; 256] = std::array::from_fn(|i| i as u16);
        let mut lengths: Vec<_> = input.bytes().map(|v| v as usize).collect();

        for nl in [17, 31, 73, 47, 23] {
            lengths.push(nl);
        }
        knot_hash(&mut seq, &lengths, 64);
        let mut res = String::new();
        for i in 0..16 {
            let mut k = 0;
            for j in 0..16 {
                k ^= seq[i * 16 + j];
            }
            res.push_str(&format!("{:02x}", k));
        }
        res
    };
    Ok((first, second))
}
