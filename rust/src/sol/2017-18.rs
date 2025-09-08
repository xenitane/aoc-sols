use std::collections::HashMap;
use std::collections::VecDeque;

enum Val<'a> {
    Raw(i64),
    Reg(&'a str),
}

impl<'a> Val<'a> {
    fn new(raw: &'a str) -> Val<'a> {
        if raw.chars().next().unwrap().is_alphabetic() {
            Val::Reg(raw)
        } else {
            Val::Raw(raw.parse().unwrap())
        }
    }
    fn get_raw(&self) -> &'a str {
        match self {
            Val::Reg(r) => r,
            Val::Raw(_) => unreachable!(),
        }
    }
    fn unwrap(self: &Self, registers: &HashMap<&'a str, i64>) -> i64 {
        match self {
            Val::Raw(v) => *v,
            Val::Reg(r) => *registers.get(r).unwrap_or(&0),
        }
    }
}

enum Instruction<'a> {
    Snd(Val<'a>),
    Set(&'a str, Val<'a>),
    Add(&'a str, Val<'a>),
    Mul(&'a str, Val<'a>),
    Mod(&'a str, Val<'a>),
    Rcv(Val<'a>),
    Jgz(Val<'a>, Val<'a>),
}

impl<'a> Instruction<'a> {
    fn apply(
        self: &Self,
        system: &mut System<'a>,
        parallel_runner: bool,
    ) -> (Option<i64>, Option<i64>) {
        match &self {
            Instruction::Snd(val) => {
                if parallel_runner {
                    system.sent_count += 1;
                    (Some(1), Some(val.unwrap(&system.registers)))
                } else {
                    system.last_sent = Some(val.unwrap(&system.registers));
                    (Some(1), None)
                }
            }
            Instruction::Set(reg, val) => {
                system.registers.insert(reg, val.unwrap(&system.registers));
                (Some(1), None)
            }
            Instruction::Add(reg, val) => {
                let new_val =
                    val.unwrap(&system.registers) + system.registers.get(reg).unwrap_or(&0);
                system.registers.insert(reg, new_val);
                (Some(1), None)
            }
            Instruction::Mul(reg, val) => {
                let new_val =
                    val.unwrap(&system.registers) * system.registers.get(reg).unwrap_or(&0);
                system.registers.insert(reg, new_val);
                (Some(1), None)
            }
            Instruction::Mod(reg, val) => {
                let den = val.unwrap(&system.registers);
                let new_val = ((system.registers.get(reg).unwrap_or(&0) % den) + den) % den;
                system.registers.insert(reg, new_val);
                (Some(1), None)
            }
            Instruction::Rcv(val) => {
                if parallel_runner {
                    if let Some(v) = system.receive_queue.pop_front() {
                        let reg = val.get_raw();
                        system.registers.insert(reg, v);
                        (Some(1), None)
                    } else {
                        (None, None)
                    }
                } else {
                    if val.unwrap(&system.registers) > 0 {
                        system.first_recovered = system.last_sent;
                    }
                    (Some(1), None)
                }
            }
            Instruction::Jgz(val, offset) => {
                if val.unwrap(&system.registers) > 0 {
                    (Some(offset.unwrap(&system.registers)), None)
                } else {
                    (Some(1), None)
                }
            }
        }
    }
}

fn make_instruction(i_str: &str) -> Instruction<'_> {
    let tokens: Vec<_> = i_str.split_whitespace().collect();
    match tokens[0] {
        "snd" => Instruction::Snd(Val::new(tokens[1])),
        "set" => Instruction::Set(tokens[1], Val::new(tokens[2])),
        "add" => Instruction::Add(tokens[1], Val::new(tokens[2])),
        "mul" => Instruction::Mul(tokens[1], Val::new(tokens[2])),
        "mod" => Instruction::Mod(tokens[1], Val::new(tokens[2])),
        "rcv" => Instruction::Rcv(Val::new(tokens[1])),
        "jgz" => Instruction::Jgz(Val::new(tokens[1]), Val::new(tokens[2])),
        _ => unreachable!(),
    }
}

struct System<'a> {
    registers: HashMap<&'a str, i64>,
    last_sent: Option<i64>,
    first_recovered: Option<i64>,
    instruction_pointer: usize,
    sent_count: usize,
    receive_queue: VecDeque<i64>,
}

pub fn solve(input: &str) -> Result<(i64, usize), ()> {
    let asm_ins: Vec<_> = input.lines().map(make_instruction).collect();
    let first = {
        let mut system = System {
            registers: HashMap::new(),
            instruction_pointer: 0,
            last_sent: None,
            first_recovered: None,
            sent_count: 0,
            receive_queue: VecDeque::new(),
        };
        loop {
            if system.instruction_pointer >= asm_ins.len() {
                break 0;
            }
            let (Some(jump), _) = asm_ins[system.instruction_pointer].apply(&mut system, false)
            else {
                unreachable!()
            };

            if let Some(v) = system.first_recovered {
                break v;
            }
            system.instruction_pointer += jump as usize;
        }
    };
    let second = {
        let mut sys0 = System {
            registers: HashMap::from([("p", 0)]),
            instruction_pointer: 0,
            last_sent: None,
            first_recovered: None,
            sent_count: 0,
            receive_queue: VecDeque::new(),
        };
        let mut sys1 = System {
            registers: HashMap::from([("p", 1)]),
            instruction_pointer: 0,
            last_sent: None,
            first_recovered: None,
            sent_count: 0,
            receive_queue: VecDeque::new(),
        };
        loop {
            loop {
                let (jump, sent) = asm_ins[sys1.instruction_pointer].apply(&mut sys1, true);
                if let Some(v) = sent {
                    sys0.receive_queue.push_back(v);
                }
                if let Some(j) = jump {
                    sys1.instruction_pointer += j as usize;
                } else {
                    break;
                }
            }
            loop {
                let (jump, sent) = asm_ins[sys0.instruction_pointer].apply(&mut sys0, true);
                if let Some(v) = sent {
                    sys1.receive_queue.push_back(v);
                }
                if let Some(j) = jump {
                    sys0.instruction_pointer += j as usize;
                } else {
                    break;
                }
            }
            if sys0.receive_queue.is_empty()
                && sys1.receive_queue.is_empty()
                && matches!(asm_ins[sys0.instruction_pointer], Instruction::Rcv(..))
                && matches!(asm_ins[sys1.instruction_pointer], Instruction::Rcv(..))
            {
                break sys1.sent_count;
            }
        }
    };
    Ok((first, second))
}
