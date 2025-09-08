use std::collections::HashMap;

const SIMULATION_TIME: usize = 10_000;

struct Particle {
    p: [isize; 3],
    v: [isize; 3],
    a: [isize; 3],
}

impl Particle {
    fn pos_at_t(&self, t: isize) -> [isize; 3] {
        let fv = t;
        let fa = (t * (t + 1)) / 2;
        let mut res = [0isize; 3];
        for i in 0..3 {
            res[i] = self.p[i] + self.v[i] * fv + self.a[i] * fa;
        }
        res
    }
}

fn parse_particle_data(line: &str) -> Particle {
    let mut res = [[0isize; 3]; 3];
    let tokens: Vec<_> = line
        .split(&[' ', 'p', 'v', 'a', '<', '>', '=', ','])
        .filter(|v| !v.is_empty())
        .map(|v| v.parse::<isize>().unwrap())
        .collect();
    for i in 0..3 {
        for j in 0..3 {
            res[i][j] = tokens[3 * i + j];
        }
    }
    Particle {
        p: res[0],
        v: res[1],
        a: res[2],
    }
}
pub fn solve(input: &str) -> Result<(usize, usize), ()> {
    let particles: Vec<_> = input.lines().map(parse_particle_data).collect();
    let len = particles.len();

    let first = {
        let mut res = 0usize;
        let mut min_acc = isize::MAX;

        for (i, particle) in particles.iter().enumerate() {
            let mut abs_acc = 0;
            for j in 0..3 {
                abs_acc += isize::abs(particle.a[j]);
            }
            if abs_acc < min_acc {
                min_acc = abs_acc;
                res = i;
            }
        }

        res
    };
    let second = {
        let mut alive: Vec<_> = (0..len).map(|i| (i, true)).collect();
        for t in 1..=SIMULATION_TIME {
            let mut pos_at_t = HashMap::new();
            for (idx, alive_status) in &alive {
                if !alive_status {
                    continue;
                }
                let pos = particles[*idx].pos_at_t(t as isize);
                if !pos_at_t.contains_key(&pos) {
                    pos_at_t.insert(pos, vec![]);
                }
                pos_at_t.get_mut(&pos).unwrap().push(*idx);
            }
            pos_at_t
                .into_iter()
                .filter(|(_, v)| v.len() > 1)
                .for_each(|(_, v)| {
                    for idx in v {
                        alive[idx] = (idx, false);
                    }
                });
        }
        let mut res = 0usize;
        for (_, alive_status) in alive {
            if alive_status {
                res += 1;
            }
        }
        res
    };
    Ok((first, second))
}
