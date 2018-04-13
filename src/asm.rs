//! A typed assembly language.
#![allow(dead_code)]

use std::collections::HashMap;
use std::ops::{BitAnd, BitOr, Not, Shl, Shr};

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
                Add(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let (v, _) = v1.overflowing_add(&v2);
                    self.file.insert(r.clone(), v);
                }
                Sub(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let (v, _) = v1.overflowing_sub(&v2);
                    self.file.insert(r.clone(), v);
                }
                And(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let v = v1 & v2;
                    self.file.insert(r.clone(), v);
                }
                Or(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let v = v1 | v2;
                    self.file.insert(r.clone(), v);
                }
                Not(ref r, ref o) => {
                    let v = self.right_value(o).clone();
                    let v = !v;
                    self.file.insert(r.clone(), v);
                }
                Shl(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let v = v1 << v2;
                    self.file.insert(r.clone(), v);
                }
                Shr(ref r, ref o1, ref o2) => {
                    let v1 = self.right_value(o1).clone();
                    let v2 = self.right_value(o2).clone();
                    let v = v1 >> v2;
                    self.file.insert(r.clone(), v);
                }
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

impl BitAnd for Value {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        let Value::Word(w1) = self;
        let Value::Word(w2) = rhs;
        Value::Word(w1 & w2)
    }
}

impl BitOr for Value {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        let Value::Word(w1) = self;
        let Value::Word(w2) = rhs;
        Value::Word(w1 | w2)
    }
}

impl Not for Value {
    type Output = Self;

    fn not(self) -> Self {
        let Value::Word(w1) = self;
        Value::Word(!w1)
    }
}

impl Shl<Value> for Value {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self {
        let Value::Word(w1) = self;
        let Value::Word(w2) = rhs;
        Value::Word(w1 << w2)
    }
}

impl Shr<Value> for Value {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self {
        let Value::Word(w1) = self;
        let Value::Word(w2) = rhs;
        Value::Word(w1 >> w2)
    }
}

impl Value {
    fn overflowing_add(&self, rhs: &Value) -> (Value, bool) {
        let Value::Word(w1) = *self;
        let Value::Word(w2) = *rhs;
        let (w, overflow) = w1.overflowing_add(w2);
        (Value::Word(w), overflow)
    }

    fn overflowing_sub(&self, rhs: &Value) -> (Value, bool) {
        let Value::Word(w1) = *self;
        let Value::Word(w2) = *rhs;
        let (w, overflow) = w1.overflowing_sub(w2);
        (Value::Word(w), overflow)
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
