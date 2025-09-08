use std::collections::{HashMap, HashSet};

struct TuringTransition<'a> {
    transitions: [(bool, bool, &'a str); 2],
}

struct TuringMachine<'a> {
    state: &'a str,
    tape: HashSet<isize>,
    cursor: isize,
}

fn parse_transition<'a>(transitions: &[&'a str]) -> (bool, bool, &'a str) {
    let tks: Vec<_> = transitions
        .iter()
        .map(|s| {
            s.split(&[' ', '.', ':'])
                .filter(|s| !s.is_empty())
                .last()
                .unwrap()
        })
        .collect();
    (tks[1] == "1", tks[2] == "right", tks[3])
}

fn make_turing_machine_transition<'a>(lines: Vec<&'a str>) -> (&'a str, TuringTransition<'a>) {
    let state_transitions: Vec<_> = lines
        .chunk_by(|_, b| *b.as_bytes().last().unwrap() != b':')
        .collect();

    (
        state_transitions[0][0]
            .split(&[' ', ':'])
            .filter(|s| !s.is_empty())
            .last()
            .unwrap(),
        TuringTransition {
            transitions: [
                parse_transition(state_transitions[1]),
                parse_transition(state_transitions[2]),
            ],
        },
    )
}

fn next_state<'a>(
    transitions: &HashMap<&'a str, TuringTransition<'a>>,
    turing_machine: &mut TuringMachine<'a>,
) {
    let Some(transitions) = transitions.get(turing_machine.state) else {
        unreachable!()
    };
    let sub_transition = if turing_machine.tape.remove(&turing_machine.cursor) {
        transitions.transitions[1]
    } else {
        transitions.transitions[0]
    };

    if sub_transition.0 {
        turing_machine.tape.insert(turing_machine.cursor);
    }
    turing_machine.cursor += if sub_transition.1 { 1 } else { -1 };
    turing_machine.state = sub_transition.2;
}

pub fn solve(input: &str) -> Result<(usize, ()), ()> {
    let mut segments = input.split("\n\n").map(|s| s.lines().collect::<Vec<_>>());
    let (init_state, iterations) = if let Some(segment) = segments.next() {
        let init_state = segment[0].split(&[' ', '.']).collect::<Vec<_>>()[3];
        let iters = segment[1].split_whitespace().collect::<Vec<_>>()[5]
            .parse::<u32>()
            .unwrap();
        (init_state, iters)
    } else {
        unreachable!()
    };

    let state_transitions: HashMap<_, _> = segments.map(make_turing_machine_transition).collect();

    let mut turing_machine = TuringMachine {
        state: init_state,
        cursor: 0,
        tape: HashSet::new(),
    };

    for _ in 0..iterations {
        next_state(&state_transitions, &mut turing_machine);
    }

    let first = turing_machine.tape.len();

    Ok((first, ()))
}
