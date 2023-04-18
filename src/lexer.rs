use std::fmt;

#[derive(Debug, PartialEq)]
pub enum Token {
    //Eof,
    Num(Scalar),
    Op(Operator),
    OpenParen,
    CloseParen,
}

#[derive(Clone, Copy, PartialEq)]
pub enum Scalar {
    Int(i64),
    Float(f64),
}

impl fmt::Debug for Scalar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Float(float) => write!(f, "{float:?}"), // Debug printing forces a trailing ".0" for non-fractional float values
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    ForwardSlash,
}

const OP_CHARS: &[(char, Operator)] = &[
    ('+', Operator::Plus),
    ('-', Operator::Minus),
    ('*', Operator::Times),
    ('/', Operator::ForwardSlash),
];

#[derive(Debug)]
pub struct ParseOperatorError;

impl TryFrom<char> for Operator {
    type Error = ParseOperatorError;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        OP_CHARS
            .iter()
            .find_map(|&(op_c, op)| if c == op_c { Some(op) } else { None })
            .ok_or(ParseOperatorError)
    }
}

impl fmt::Debug for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (c, op) in OP_CHARS {
            if self == op {
                return write!(f, "{c}");
            }
        }
        unreachable!()
    }
}

// enum ParseResult {
//     Success(Token),
//     Fail,
//     Error,
// }
#[derive(Debug)]
pub struct LexError;

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error while lexing")
    }
}
impl std::error::Error for LexError {}

pub fn lex(mut src: &str) -> Result<Vec<Token>, LexError> {
    let lexers = [lex_open_paren, lex_close_paren, lex_operator, lex_num];
    let mut tokens = vec![]; // vec![Token::Eof]
    src = src.trim_start();
    while !src.is_empty() {
        //dbg!(src);
        let mut lex_fail = true;
        for (i, lexer) in lexers.iter().enumerate() {
            //dbg!(i);
            if let Some((tok, rest)) = lexer(src) {
                if src.as_ptr() == rest.as_ptr() {
                    eprintln!("Lexer {i:?} succeeded but did not consume any characters from the front of the source str");
                    eprintln!("token: {tok:?}");
                    eprintln!("source str: {src:?}");
                    if src != rest {
                        eprintln!("rest str: {rest:?}");
                    }
                    unreachable!();
                }
                tokens.push(tok);
                src = rest;
                lex_fail = false;
                break;
            }
        }
        //dbg!(lex_fail);
        if lex_fail {
            return Err(LexError);
        }
        src = src.trim_start();
    }
    //tokens.push(Token::CloseParen);
    Ok(tokens)
}

fn lex_num(src: &str) -> Option<(Token, &str)> {
    compete_lexers(lex_int, lex_float, src)
}

fn lex_operator(src: &str) -> Option<(Token, &str)> {
    let (c, rest) = split_first_char(src)?;
    c.try_into().ok().map(|o| (Token::Op(o), rest))
}

fn lex_open_paren(src: &str) -> Option<(Token, &str)> {
    try_lex_pattern('(', Token::OpenParen, src)
}
fn lex_close_paren(src: &str) -> Option<(Token, &str)> {
    try_lex_pattern(')', Token::CloseParen, src)
}

fn lex_int(src: &str) -> Option<(Token, &str)> {
    try_lex(
        |s| s.parse::<i64>().ok().map(|x| Token::Num(Scalar::Int(x))),
        src,
    )
}

fn lex_float(src: &str) -> Option<(Token, &str)> {
    try_lex(
        |s| s.parse::<f64>().ok().map(|x| Token::Num(Scalar::Float(x))),
        src,
    )
}

fn try_lex<F>(f: F, src: &str) -> Option<(Token, &str)>
where
    F: Fn(&str) -> Option<Token>,
{
    let mut val = None;
    for (slice, rest) in iter_slices(src) {
        match f(slice) {
            None => break,
            Some(v) => val = Some((v, rest)),
        }
    }
    val
}

use std::str::pattern::Pattern;
fn try_lex_pattern<'a>(c: impl Pattern<'a>, token: Token, src: &'a str) -> Option<(Token, &str)> {
    c.strip_prefix_of(src).map(|rest| (token, rest))
}

// Please tell me someone can refactor this
fn compete_lexers<F1, F2>(f1: F1, f2: F2, src: &str) -> Option<(Token, &str)>
where
    F1: Fn(&str) -> Option<(Token, &str)>,
    F2: Fn(&str) -> Option<(Token, &str)>,
{
    match (f1(src), f2(src)) {
        (Some((f1_token, f1_rest)), Some((f2_token, f2_rest))) => {
            if f1_rest.len() > f2_rest.len() {
                Some((f2_token, f2_rest))
            } else {
                Some((f1_token, f1_rest))
            }
        }
        (x, y) => x.or(y),
    }
}

fn iter_slices(s: &str) -> impl Iterator<Item = (&str, &str)> {
    s.char_indices().map(|(pos, char)| {
        let cut = pos + char.len_utf8();
        (&s[..cut], &s[cut..])
    })
}

fn split_first_char(s: &str) -> Option<(char, &str)> {
    s.chars().next().map(|c| (c, &s[c.len_utf8()..]))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn iter_test() {
        assert_eq!(
            iter_slices("hello").collect::<Vec<_>>(),
            vec![
                ("h", "ello"),
                ("he", "llo"),
                ("hel", "lo"),
                ("hell", "o"),
                ("hello", "")
            ]
        );
        assert_eq!(
            iter_slices("bár").collect::<Vec<_>>(),
            vec![("b", "ár"), ("bá", "r"), ("bár", "")]
        );
    }
    #[test]
    fn ints() {
        assert_eq!(
            lex_int("123abc"),
            Some((Token::Num(Scalar::Int(123)), "abc"))
        );
        assert_eq!(
            lex_int("3.14159"),
            Some((Token::Num(Scalar::Int(3)), ".14159"))
        );
        assert_eq!(lex_int("-10"), None);
        assert_eq!(lex_int("andre3000"), None);
    }
    #[test]
    fn floats() {
        assert_eq!(
            lex_float("123abc"),
            Some((Token::Num(Scalar::Float(123.0)), "abc"))
        );
        assert_eq!(
            lex_float("3.14159"),
            Some((Token::Num(Scalar::Float(3.14159)), ""))
        );
        assert_eq!(lex_float("-10"), None);
        assert_eq!(lex_float("andre3000"), None);
    }
    #[test]
    fn ops() {
        assert_eq!(
            lex_operator("+ 69420"),
            Some((Token::Op(Operator::Plus), " 69420"))
        );
        assert_eq!(lex_operator("- 69420"), None);
        assert_eq!(lex_operator("4 8 15 16 23 42"), None);
    }
}
