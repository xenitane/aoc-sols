use std::collections::HashMap;
use std::collections::HashSet;

fn count_reachable(adj: &HashMap<u32, Vec<u32>>, i: u32, vis: &mut HashSet<u32>) -> u32 {
    if vis.contains(&i) {
        return 0;
    }
    vis.insert(i);
    let mut res = 1;
    let Some(x) = adj.get(&i) else { unreachable!() };
    for next in x {
        res += count_reachable(adj, *next, vis);
    }
    res
}

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let adj: HashMap<_, _> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ',']).filter(|s| !s.is_empty());
            let id: u32 = tokens.next().unwrap().parse().unwrap();
            tokens.next();
            let mut v = vec![];
            while let Some(t) = tokens.next() {
                v.push(t.parse::<u32>().unwrap());
            }
            (id, v)
        })
        .collect();
    let mut vis = HashSet::new();
    let first = count_reachable(&adj, 0, &mut vis);
    let second = {
        let mut groups = 1;
        for (i, _) in adj.iter() {
            if count_reachable(&adj, *i, &mut vis) > 0 {
                groups += 1;
            }
        }
        groups
    };
    Ok((first, second))
}
