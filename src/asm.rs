//! A typed assembly language.
#![allow(dead_code)]

use std::collections::HashMap;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Register(usize);

#[derive(PartialEq, Clone, Debug)]
enum Value {
    Word(u32),
}

#[derive(PartialEq, Clone, Debug)]
enum Operand {
    Register(Register),
    Value(Value),
}

#[derive(PartialEq, Clone, Debug)]
enum Instruction {
    Mov(Register, Operand),
    Add(Register, Operand, Operand),
    Sub(Register, Operand, Operand),
    And(Register, Operand, Operand),
    Or(Register, Operand, Operand),
    Not(Register, Operand),
    Shl(Register, Operand, Operand),
    Shr(Register, Operand, Operand),
}

#[derive(PartialEq, Clone, Debug)]
struct Sequence(Vec<Instruction>);

#[derive(PartialEq, Debug)]
struct File(HashMap<Register, Value>);

#[derive(PartialEq, Debug)]
struct Machine {
    seq: Sequence,
    file: File,
}

impl Machine {
    fn eval(&mut self) {
        use self::Instruction::*;
        for i in self.seq.0.iter() {
            match *i {
                Mov(ref r, ref o) => {
                    let v = self.right_value(o).clone();
                    self.file.insert(r.clone(), v);
                }
                _ => unimplemented!(),
            }
        }
    }

    fn right_value<'a>(&'a self, o: &'a Operand) -> &'a Value {
        use self::Operand::*;
        match *o {
            Register(ref r) => self.file.get(&r),
            Value(ref v) => v,
        }
    }
}

impl File {
    fn insert(&mut self, r: Register, v: Value) {
        self.0.insert(r, v);
    }

    fn get(&self, r: &Register) -> &Value {
        self.0
            .get(r)
            .expect(&format!("missing content in register {:?}", r))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mov() {
        use self::Instruction::*;
        use self::Value::*;
        let mut f = HashMap::new();
        let v = Word(0b0);
        let s = Sequence(vec![Mov(Register(0), Operand::Value(v.clone()))]);
        let mut m = Machine {
            seq: s.clone(),
            file: File(f.clone()),
        };
        m.eval();
        f.insert(Register(0), v);
        assert_eq!(
            m,
            Machine {
                seq: s,
                file: File(f),
            }
        );
    }
}
