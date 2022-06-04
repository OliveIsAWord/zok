use crate::lexer::{Operator, Scalar};
use crate::parser::Ast;
use std::fmt;

#[derive(Debug)]
pub enum Value {
    Num(Scalar),
    Array(Vec<Self>),
}

impl Value {
    fn print(&self, f: &mut fmt::Formatter<'_>, mut use_parens: bool) -> fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{:?}", x),
            Self::Array(xs) => {
                if xs.len() < 2 {
                    use_parens = true;
                }
                if use_parens {
                    write!(f, "(")?;
                }
                for (i, x) in xs.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    x.print(f, true)?;
                }
                if use_parens {
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print(f, false)
    }
}

fn to_float(s: Scalar) -> f64 {
    match s {
        // The precision loss is an intended feature. The ramifications are passed on to the end user.
        #[allow(clippy::cast_precision_loss)]
        Scalar::Int(x) => x as f64,
        Scalar::Float(f) => f,
    }
}

use std::ops;
impl ops::Add<Self> for Scalar {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Scalar::Int(x), Scalar::Int(y)) => Scalar::Int(x.wrapping_add(y)),
            (a, b) => Scalar::Float(to_float(a) + to_float(b)),
        }
    }
}

#[derive(Debug)]
pub struct EvalError;
pub type EvalResult = Result<Value, EvalError>;

pub fn evaluate(ast: &Ast) -> EvalResult {
    match ast {
        &Ast::Scalar(x) => Ok(Value::Num(x)),
        Ast::Array(xs) => xs
            .iter()
            .map(evaluate)
            .collect::<Result<_, _>>()
            .map(Value::Array),
        Ast::Monad(op, box rhs) => {
            let rhs = evaluate(rhs)?;
            evaluate_monad(*op, rhs)
        }
        Ast::Dyad(op, box (lhs, rhs)) => {
            let lhs = evaluate(lhs)?;
            let rhs = evaluate(rhs)?;
            evaluate_dyad(*op, lhs, rhs)
        }
    }
}

fn evaluate_monad(op: Operator, val: Value) -> EvalResult {
    match op {
        Operator::Plus => todo!("transpose operation"),
        Operator::Minus => atomic_monad(
            |x| match x {
                Scalar::Int(i) => Scalar::Int(i.wrapping_neg()),
                Scalar::Float(f) => Scalar::Float(-f),
            },
            val,
        ),
    }
}

fn evaluate_dyad(op: Operator, val1: Value, val2: Value) -> EvalResult {
    match op {
        Operator::Plus => atomic_dyad(|x, y| x + y, val1, val2),
        Operator::Minus => todo!("subtraction"),
    }
}

fn atomic_monad<F>(f: F, val: Value) -> EvalResult
where
    F: Fn(Scalar) -> Scalar + Copy,
{
    use Value::{Array, Num};
    match val {
        Num(a) => Ok(Num(f(a))),
        Array(xs) => xs
            .into_iter()
            .map(|a| atomic_monad(f, a))
            .collect::<Result<_, _>>()
            .map(Array),
    }
}

fn atomic_dyad<F>(f: F, val1: Value, val2: Value) -> EvalResult
where
    F: Fn(Scalar, Scalar) -> Scalar + Copy,
{
    use Value::{Array, Num};
    match (val1, val2) {
        (Num(a), Num(b)) => Ok(Num(f(a, b))),
        (Array(xs), Array(ys)) => {
            if xs.len() != ys.len() {
                return Err(EvalError);
            }
            xs.into_iter()
                .zip(ys.into_iter())
                .map(|(a, b)| atomic_dyad(f, a, b))
                .collect::<Result<_, _>>()
                .map(Array)
        }
        (Num(a), Array(ys)) => ys
            .into_iter()
            .map(|b| atomic_dyad(f, Num(a), b))
            .collect::<Result<_, _>>()
            .map(Array),
        (Array(xs), Num(b)) => xs
            .into_iter()
            .map(|a| atomic_dyad(f, a, Num(b)))
            .collect::<Result<_, _>>()
            .map(Array),
    }
}

// (Array(xs), Num(b)) => Array(xs.iter().map(|a| Num(func(a, b))).collect()),
// (Num(a), Array(ys)) => Array(ys.iter().map(|b| Num(func(a, b))).collect()),
// (Array(xs), Array(ys)) => {
//     if xs.len() != ys.len() {
//         return Err(EvalError);
//     }
//     Array(xs.iter().zip(&ys).map(|(a, b)| Num(func(a, b))).collect())
// }
