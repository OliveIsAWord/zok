use crate::lexer::{Operator, Scalar, Token};

#[derive(PartialEq)]
pub enum Ast {
    Scalar(Scalar),
    Array(Vec<Self>),
    Monad(Operator, Box<Self>),
    Dyad(Operator, Box<(Self, Self)>),
}

use std::fmt;
impl fmt::Debug for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Scalar(v) => write!(f, "{v:?}"),
            Self::Array(vals) => write!(f, "{vals:?}"),
            Self::Monad(op, rhs) => write!(f, "M({op:?} {rhs:?})"),
            Self::Dyad(op, box (lhs, rhs)) => write!(f, "D({op:?} {lhs:?} {rhs:?})"),
        }
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum ParseError {
    ExpectedToken,
    ExtraClosingBrace,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::ExpectedToken => "unclosed parenthesis",
            Self::ExtraClosingBrace => "extra closed parenthesis",
        };
        write!(f, "{s}")
    }
}
impl std::error::Error for ParseError {}

pub fn parse_expr(tokens: &[Token], unwrap_single: bool) -> Result<Ast, ParseError> {
    let mut array: Vec<Ast> = vec![];
    let mut iter = iter_with_rest(tokens);
    while let Some((token, rest)) = iter.next() {
        match token {
            Token::Num(i) => array.push(Ast::Scalar(*i)),
            &Token::Op(op) => {
                // check if next token is right paren or none, then error `ExpectedTokens`
                let rhs = parse_expr(rest, true)?;
                let lhs = match array.len() {
                    0 => return Ok(Ast::Monad(op, box rhs)),
                    1 => array.pop().unwrap(),
                    _ => Ast::Array(array),
                };
                return Ok(Ast::Dyad(op, box (lhs, rhs)));
            }
            Token::OpenParen => {
                let mut depth: usize = 1;
                for (i, token) in rest.iter().enumerate() {
                    match token {
                        Token::OpenParen => depth += 1,
                        Token::CloseParen => {
                            depth -= 1;
                            if depth == 0 {
                                array.push(parse_expr(&rest[..i], false)?);
                                iter = iter_with_rest(&rest[i + 1..]);
                                break;
                            }
                        }
                        _ => (),
                    }
                }
                if depth > 0 {
                    return Err(ParseError::ExpectedToken);
                }
            }
            Token::CloseParen => return Err(ParseError::ExtraClosingBrace),
            Token::OpenSquare | Token::CloseSquare => todo!(),
        }
    }
    match array.len() {
        //0 => Err(ParseError::ExpectedToken),
        1 if unwrap_single => Ok(array.pop().unwrap()),
        _ => Ok(Ast::Array(array)),
    }
}

fn iter_with_rest(tokens: &[Token]) -> impl Iterator<Item = (&Token, &[Token])> {
    tokens
        .iter()
        .enumerate()
        .map(|(i, t)| (t, &tokens[i + 1..]))
}
