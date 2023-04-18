use crate::lexer::{Operator, Scalar};
use crate::parser::Ast;
use std::fmt;

#[derive(Debug)]
pub enum Value {
    Num(Scalar),
    Array(Vec<Self>),
}

impl Value {
    // TODO: rewrite this without recursion
    fn print(&self, f: &mut fmt::Formatter<'_>, mut use_parens: bool) -> fmt::Result {
        match self {
            Self::Num(x) => write!(f, "{x:?}"),
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

const fn to_float(s: Scalar) -> f64 {
    match s {
        Scalar::Int(i) => int_to_float(i),
        Scalar::Float(f) => f,
    }
}

/// A type safe way of converting the Zok int type to the Zok float type
/// The precision loss is an intended feature; the ramifications are passed on to the end user.
#[allow(clippy::cast_precision_loss)]
const fn int_to_float(x: i64) -> f64 {
    x as f64
}

fn scalar_op<F, G>(f_int: F, f_float: G) -> impl Fn(Scalar, Scalar) -> Scalar
where
    F: Fn(i64, i64) -> i64,
    G: Fn(f64, f64) -> f64,
{
    use Scalar::{Float, Int};
    move |x, y| match (x, y) {
        (Int(a), Int(b)) => Int(f_int(a, b)),
        (Int(a), Float(b)) => Float(f_float(int_to_float(a), b)),
        (Float(a), Int(b)) => Float(f_float(a, int_to_float(b))),
        (Float(a), Float(b)) => Float(f_float(a, b)),
    }
}

use std::ops;
impl ops::Add<Self> for Scalar {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        scalar_op(i64::wrapping_add, f64::add)(self, rhs)
    }
}
impl ops::Sub<Self> for Scalar {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        scalar_op(i64::wrapping_sub, f64::sub)(self, rhs)
    }
}
impl ops::Mul<Self> for Scalar {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        scalar_op(i64::wrapping_mul, f64::mul)(self, rhs)
    }
}
impl ops::Div<Self> for Scalar {
    type Output = Self;
    fn div(self, mut rhs: Self) -> Self {
        use Scalar::{Float, Int};
        if matches!(rhs, Int(0)) {
            // use floating point rules for zero division
            rhs = Float(0.0);
        }
        scalar_op(i64::wrapping_div, f64::div)(self, rhs)
    }
}
impl ops::Neg for Scalar {
    type Output = Self;
    fn neg(self) -> Self {
        use Scalar::{Float, Int};
        match self {
            Int(i) => Int(i.wrapping_neg()),
            Float(f) => Float(f.neg()),
        }
    }
}

#[derive(Debug)]
pub struct EvalError;
impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "eval error lol")
    }
}
impl std::error::Error for EvalError {}
pub type EvalResult = Result<Value, EvalError>;

// TODO: rewrite this without recursion
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
        Operator::Minus => atomic_monad(|x| -x)(val),
        Operator::Times => Ok(match val {
            // Get the first value of the array without cloning, or the empty array if there is none
            Value::Array(a) => a.into_iter().next().unwrap_or(Value::Array(vec![])),
            x @ Value::Num(_) => x,
        }),
        Operator::ForwardSlash => atomic_monad(|x| Scalar::Float(to_float(x).sqrt()))(val),
        Operator::Bang => match val {
            Value::Num(Scalar::Int(i)) => {
                let range = if i >= 0 { 0..i } else { i..0 };
                Ok(Value::Array(
                    range.map(Scalar::Int).map(Value::Num).collect(),
                ))
            }
            Value::Num(Scalar::Float(_)) => => Err(EvalError),
            _ => todo!("range on list"),
        },
    }
}

fn evaluate_dyad(op: Operator, val1: Value, val2: Value) -> EvalResult {
    match op {
        Operator::Plus => atomic_dyad(|x, y| x + y)(val1, val2),
        Operator::Minus => atomic_dyad(|x, y| x - y)(val1, val2),
        Operator::Times => atomic_dyad(|x, y| x * y)(val1, val2),
        Operator::ForwardSlash => atomic_dyad(|x, y| x / y)(val1, val2),
        Operator::Bang => todo!("bang dyad"),
    }
}

// TODO: rewrite this without recursion
fn atomic_monad<F>(f: F) -> impl Fn(Value) -> EvalResult
where
    F: Fn(Scalar) -> Scalar + Copy,
{
    use Value::{Array, Num};
    move |val| match val {
        Num(a) => Ok(Num(f(a))),
        Array(xs) => xs
            .into_iter()
            .map(atomic_monad(f))
            .collect::<Result<_, _>>()
            .map(Array),
    }
}

// TODO: rewrite this without recursion
fn atomic_dyad<F>(f: F) -> impl Fn(Value, Value) -> EvalResult
where
    F: Fn(Scalar, Scalar) -> Scalar + Copy,
{
    use Value::{Array, Num};
    move |val1, val2| match (val1, val2) {
        (Num(a), Num(b)) => Ok(Num(f(a, b))),
        (Array(xs), Array(ys)) => {
            // the length of the output will be the length of the shorter input, thanks to `Iterator::zip`
            xs.into_iter()
                .zip(ys.into_iter())
                .map(|(a, b)| atomic_dyad(f)(a, b))
                .collect::<Result<_, _>>()
                .map(Array)
        }
        (Num(a), Array(ys)) => ys
            .into_iter()
            .map(|b| atomic_dyad(f)(Num(a), b))
            .collect::<Result<_, _>>()
            .map(Array),
        (Array(xs), Num(b)) => xs
            .into_iter()
            .map(|a| atomic_dyad(f)(a, Num(b)))
            .collect::<Result<_, _>>()
            .map(Array),
    }
}
