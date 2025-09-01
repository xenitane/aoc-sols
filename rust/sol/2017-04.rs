use std::collections::HashSet;
use std::iter::FromIterator;

pub fn solve(input: &str) -> Result<(u32, u32), ()> {
    let is_valid_passphrase = |acc: (u32, u32), passphrase: &str| {
        let words: Vec<_> = passphrase.split_whitespace().collect();
        let len = words.len();
        let st: HashSet<_> = words.into_iter().collect();
        let f_acc = acc.0 + if len == st.len() { 1 } else { 0 };
        let nst: HashSet<_> = st
            .into_iter()
            .map(|ss| {
                let mut chars: Vec<_> = ss.chars().collect();
                chars.sort();
                String::from_iter(chars)
            })
            .collect();
        let s_acc = acc.1 + if len == nst.len() { 1 } else { 0 };
        (f_acc, s_acc)
    };
    let (first, second) = input.lines().fold((0, 0), is_valid_passphrase);
    Ok((first, second))
}
