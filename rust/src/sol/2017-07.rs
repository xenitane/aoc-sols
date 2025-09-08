use std::cmp::Ordering;
use std::collections::HashMap;

struct Tree {
    weight: u32,
    children: Option<Vec<Tree>>,
}

fn make_trees(
    key: &str,
    info: &mut HashMap<&str, (u32, Option<Vec<&str>>)>,
    trees: &mut HashMap<&str, Tree>,
) -> Tree {
    let Some((weight, ch_op)) = info.remove(key) else {
        unreachable!()
    };
    let sub_trees = match ch_op {
        Some(children) => {
            let mut res = vec![];
            for ch in children {
                let subtree = trees
                    .remove(ch)
                    .unwrap_or_else(|| make_trees(ch, info, trees));
                res.push(subtree);
            }
            Some(res)
        }
        None => None,
    };
    Tree {
        weight,
        children: sub_trees,
    }
}

fn rectify_tree(tree: &Tree) -> (Option<u32>, u32) {
    match &tree.children {
        None => (None, tree.weight),
        Some(chv) => {
            let mut w = tree.weight;
            let mut map = HashMap::new();
            for ch in chv {
                let (rectified_weight_op, sub_tree_weight) = rectify_tree(ch);
                if let Some(nw) = rectified_weight_op {
                    return (Some(nw), 0);
                }
                w += sub_tree_weight;
                let mut node_ids = map.remove(&sub_tree_weight).unwrap_or(vec![]);
                node_ids.push(ch.weight);
                map.insert(sub_tree_weight, node_ids);
            }
            let mut kk: Vec<_> = map.iter().collect();
            kk.sort_by(|(a, av), (b, bv)| {
                if av.len() < bv.len() || (av.len() == bv.len() && a > b) {
                    Ordering::Less
                } else if av.len() == bv.len() && a == b {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                }
            });
            if kk.len() == 1 {
                (None, w)
            } else {
                (Some(kk[0].1[0] + kk[1].0 - kk[0].0), 0)
            }
        }
    }
}

pub fn solve(input: &str) -> Result<(&str, u32), ()> {
    let mut tree_info: HashMap<_, _> = input
        .lines()
        .map(|line| {
            let tokens: Vec<_> = line
                .split(&[' ', ',', '(', ')'])
                .filter(|s| !s.is_empty())
                .collect();
            let key = tokens[0];
            let weight: u32 = tokens[1].parse().unwrap();
            (
                key,
                (
                    weight,
                    if tokens.len() == 2 {
                        None
                    } else {
                        Some(tokens[3..].to_vec())
                    },
                ),
            )
        })
        .collect();

    let mut trees = HashMap::new();
    while !tree_info.is_empty() {
        let Some((&key, _)) = tree_info.iter().next() else {
            unreachable!()
        };
        let tree = make_trees(key, &mut tree_info, &mut trees);
        trees.insert(key, tree);
    }
    let Some((&first, tree)) = trees.iter().next() else {
        unreachable!()
    };
    let (Some(second), _) = rectify_tree(&tree) else {
        unreachable!()
    };

    Ok((first, second))
}
