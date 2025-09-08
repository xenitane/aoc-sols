enum Val {
    Raw(isize),
    Reg(u8),
}

impl Val {
    fn new(raw: &str) -> Val {
        if raw.chars().next().unwrap().is_alphabetic() {
            Val::Reg(raw.as_bytes()[0] - b'a')
        } else {
            Val::Raw(raw.parse().unwrap())
        }
    }
    fn unwrap(self: &Self, registers: &[isize; 8]) -> isize {
        match self {
            Val::Raw(v) => *v,
            Val::Reg(r) => registers[*r as usize],
        }
    }
}

enum Instruction {
    Set(u8, Val),
    Sub(u8, Val),
    Mul(u8, Val),
    Jgz(Val, Val),
}

impl Instruction {
    fn apply(self: &Self, system: &mut System) {
        let new_ip = (system.instruction_pointer as isize)
            + match &self {
                Instruction::Set(reg, val) => {
                    system.registers[*reg as usize] = val.unwrap(&system.registers);
                    1
                }
                Instruction::Sub(reg, val) => {
                    system.registers[*reg as usize] -= val.unwrap(&system.registers);
                    1
                }
                Instruction::Mul(reg, val) => {
                    system.registers[*reg as usize] *= val.unwrap(&system.registers);
                    system.mul_count += 1;
                    1
                }
                Instruction::Jgz(val, offset) => {
                    if val.unwrap(&system.registers) != 0 {
                        offset.unwrap(&system.registers)
                    } else {
                        1
                    }
                }
            };
        system.instruction_pointer = new_ip as usize;
    }
}

fn make_instruction(i_str: &str) -> Instruction {
    let tokens: Vec<_> = i_str.split_whitespace().collect();
    match tokens[0] {
        "set" => Instruction::Set(tokens[1].as_bytes()[0] - b'a', Val::new(tokens[2])),
        "sub" => Instruction::Sub(tokens[1].as_bytes()[0] - b'a', Val::new(tokens[2])),
        "mul" => Instruction::Mul(tokens[1].as_bytes()[0] - b'a', Val::new(tokens[2])),
        "jnz" => Instruction::Jgz(Val::new(tokens[1]), Val::new(tokens[2])),
        _ => unreachable!(),
    }
}

struct System {
    registers: [isize; 8],
    instruction_pointer: usize,
    mul_count: usize,
}

fn run_program(asm_ins: &Vec<Instruction>, init_reg: [isize; 8]) -> System {
    let mut system = System {
        registers: init_reg,
        instruction_pointer: 0,
        mul_count: 0,
    };
    loop {
        if system.instruction_pointer >= asm_ins.len() {
            break;
        }
        asm_ins[system.instruction_pointer].apply(&mut system);
    }
    system
}

fn is_prime(num: usize) -> bool {
    if num < 2 || num & 1 == 0 {
        return false;
    }
    let mut f = 3;
    while f * f <= num {
        if num % f == 0 {
            return false;
        }
        f += 2;
    }
    return true;
}

pub fn solve(input: &str) -> Result<(usize, usize), ()> {
    let asm_ins: Vec<_> = input.lines().map(make_instruction).collect();
    let first = {
        let system = run_program(&asm_ins, [0; 8]);
        system.mul_count
    };
    let second = {
        // analyze the instructions, my input is asking me to count
        // composite numbers of the form `106500+17k` for k in range [0,1000]
        let mut res = 1001usize;
        for i in 0..=1000 {
            if is_prime(106500 + 17 * i) {
                res -= 1;
            }
        }
        res
    };
    Ok((first, second))
}
