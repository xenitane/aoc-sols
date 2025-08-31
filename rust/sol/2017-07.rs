#![allow(special_module_name)]

use std::cmp::Ordering;

struct Tree {
    weight: u32,
    children: Option<Vec<Tree>>,
}

fn make_trees(
    key: &str,
    info: &mut std::collections::HashMap<&str, (u32, Option<Vec<&str>>)>,
    trees: &mut std::collections::HashMap<&str, Tree>,
) -> Tree {
    let (weight, ch_op) = info.remove(key).unwrap();
    let sub_trees = match ch_op {
        Some(children) => {
            let mut res = Vec::new();
            for ch in children {
                let subtree = if let Some(st) = trees.remove(ch) {
                    st
                } else {
                    make_trees(ch, info, trees)
                };
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
            let mut map = std::collections::HashMap::new();
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

fn solve(input: &str) -> Result<(&str, u32), ()> {
    let mut tree_info: std::collections::HashMap<_, _> = input
        .lines()
        .map(|line| {
            let mut tokens = line.split(&[' ', ',', '(', ')']).filter(|s| !s.is_empty());
            let key = tokens.next().unwrap();
            let weight: u32 = tokens.next().unwrap().parse().unwrap();
            if tokens.next() == None {
                (key, (weight, None))
            } else {
                let mut children_keys = Vec::new();
                while let Some(ch) = tokens.next() {
                    children_keys.push(ch);
                }
                (key, (weight, Some(children_keys)))
            }
        })
        .collect();

    let mut trees = std::collections::HashMap::new();
    while tree_info.len() > 0 {
        let (&key, _) = tree_info.iter().next().unwrap();
        let tree = make_trees(key, &mut tree_info, &mut trees);
        trees.insert(key, tree);
    }
    let (&first, tree) = trees.iter().next().unwrap();
    let (Some(second), _) = rectify_tree(&tree) else {
        unreachable!()
    };

    Ok((first, second))
}

pub mod lib;
pub mod opts;

use lib::PrintablePair;
use std::io;

fn main() -> Result<(), ()> {
    let input = lib::read_entire_file(opts::INPUT_FILE_PATH)?;
    let res = solve(&input)?;
    res.print_to(&mut io::stdout())?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn aa() -> Result<(), ()> {
        let input = lib::read_entire_file(opts::TEST_INPUT_FILE_PATH)?;
        let expected = lib::read_entire_file(opts::TEST_OUTPUT_FILE_PATH)?;
        let res = solve(&input)?;
        let mut buffer = String::new();
        res.print_to(&mut buffer)?;
        assert_eq!(
            expected.trim(),
            buffer.trim(),
            r#"Expected:
--------------------------------
{}
--------------------------------
Actual:
--------------------------------
{}
--------------------------------
"#,
            expected.trim(),
            buffer.trim()
        );
        Ok(())
    }
}
