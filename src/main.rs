use crate::Expr::*;
use std::collections::HashMap;
use std::fmt::Display;
use std::io::{stdin, stdout, Write};
use std::iter::Peekable;

#[allow(dead_code)]
#[allow(unused)]
#[derive(Debug, Clone, PartialEq)]
enum Expr {
    Sym(String),
    Fun(String, Vec<Expr>),
}

impl Expr {
    fn parse_peekable(lexer: &mut Peekable<impl Iterator<Item = Token>>) -> Self {
        if let Some(name) = lexer.next() {
            match name.kind {
                TokenKind::Sym => {
                    if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::OpenParen) {
                        let mut args = Vec::new();
                        if let Some(_) = lexer.next_if(|t| t.kind == TokenKind::ClosedParen) {
                            return Expr::Fun(name.text, args);
                        }
                        args.push(Self::parse_peekable(lexer));
                        while let Some(_) = lexer.next_if(|t| t.kind == TokenKind::Comma) {
                            args.push(Self::parse_peekable(lexer));
                        }
                        if lexer
                            .next_if(|t| t.kind == TokenKind::ClosedParen)
                            .is_none()
                        {
                            todo!("Expected Closed paren")
                        }
                        Expr::Fun(name.text, args)
                        // todo!("Parse functor args")
                    } else {
                        Expr::Sym(name.text)
                    }
                }
                _ => todo!("Report expected Symbol"),
            }
        } else {
            todo!("Report end of file Error")
        }
    }
    fn parse(lexer: impl Iterator<Item = Token>) -> Self {
        Self::parse_peekable(&mut lexer.peekable())
        // None
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Sym(name) => write!(f, "{name}"),
            Expr::Fun(name, args) => {
                write!(f, "{name}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
                // Ok(())
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Rule {
    head: Expr,
    body: Expr,
}

fn substitute_bindings(bindings: &Bindings, expr: &Expr) -> Expr {
    match expr {
        Sym(name) => {
            if let Some(value) = bindings.get(name) {
                value.clone()
            } else {
                expr.clone()
            }
        }
        Fun(name, args) => {
            let new_name = match bindings.get(name) {
                Some(Sym(new_name)) => new_name,
                None => name,
                Some(_) => panic!("Invalid value. Burn in fire"),
            };
            let mut new_args = Vec::new();
            for arg in args {
                new_args.push(substitute_bindings(bindings, &arg));
            }
            Fun(new_name.clone(), new_args)
        }
    }
}

impl Rule {
    #[allow(unused)]
    fn apply_all(&self, expr: &Expr) -> Expr {
        if let Some(bindings) = pattern_match(&self.head, &expr) {
            substitute_bindings(&bindings, &self.body)
        } else {
            match expr {
                Sym(_) => expr.clone(),
                Fun(name, args) => {
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(self.apply_all(arg))
                    }
                    Fun(name.clone(), new_args)
                }
                _ => unreachable!(),
            }
        }
    }
}

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.head, self.body)
    }
}

type Bindings = HashMap<String, Expr>;

fn pattern_match(pattern: &Expr, value: &Expr) -> Option<Bindings> {
    fn pattern_match_impl(pattern: &Expr, value: &Expr, bindings: &mut Bindings) -> bool {
        match (pattern, value) {
            (Sym(name), _) => {
                if let Some(bound_value) = bindings.get(name) {
                    if bound_value == value {
                        true
                    } else {
                        false
                    }
                } else {
                    bindings.insert(name.clone(), value.clone());
                    true
                }
            }
            (Fun(name1, args1), Fun(name2, args2)) => {
                if name1 == name2 && args1.len() == args2.len() {
                    for i in 0..args1.len() {
                        if !pattern_match_impl(&args1[i], &args2[i], bindings) {
                            return false;
                        }
                    }
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
    let mut bindings = HashMap::new();
    if pattern_match_impl(pattern, value, &mut bindings) {
        Some(bindings)
    } else {
        None
    }
}

#[derive(Debug, PartialEq)]
enum TokenKind {
    Sym,
    OpenParen,
    ClosedParen,
    Comma,
    Equals,
}

#[allow(dead_code)]
#[derive(Debug)]
struct Token {
    kind: TokenKind,
    text: String,
}
#[derive(Clone)]
struct Lexer<Char: Iterator<Item = char>> {
    chars: Peekable<Char>,
}

impl<Char: Iterator<Item = char>> Lexer<Char> {
    fn from_iter(chars: Char) -> Self {
        Self {
            chars: chars.peekable(),
        }
    }
}
impl<Char: Iterator<Item = char>> Iterator for Lexer<Char> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        //ignore all spaces
        while let Some(_) = self.chars.next_if(|x| x.is_whitespace()) {}

        if let Some(x) = self.chars.next() {
            let mut text = String::new();
            text.push(x);
            match x {
                '(' => Some(Token {
                    kind: TokenKind::OpenParen,
                    text,
                }),
                ')' => Some(Token {
                    kind: TokenKind::ClosedParen,
                    text,
                }),
                ',' => Some(Token {
                    kind: TokenKind::Comma,
                    text,
                }),
                '=' => Some(Token {
                    kind: TokenKind::Equals,
                    text,
                }),

                _ => {
                    if !x.is_alphanumeric() {
                        panic!("Unexpected Token starts with '{}'", x);
                    }
                    while let Some(x) = self.chars.next_if(|x| x.is_alphanumeric()) {
                        text.push(x);
                    }
                    Some(Token {
                        kind: TokenKind::Sym,
                        text,
                    })
                }
            }
        } else {
            None
        }
    }
}

#[allow(unused_macros)]
macro_rules! fun_args {
    () => {
        vec![]
    };
    ($name: ident) => {
        vec![expr!($name)]
    };
    ($name: ident, $($rest: tt)*) => {
        {
            let mut t = vec![expr!($name)];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    };
    ($name: ident($($args: tt)*)) => {
        vec![expr!($name($($args)*))]
    };
    ($name: ident($($args: tt)*), $($rest: tt)*) => {
        {
            let mut t = vec![expr!($name($($args)*))];
            t.append(&mut fun_args!($($rest)*));
            t
        }
    };
}
#[allow(unused_macros)]
macro_rules! expr {
    ($name: ident) => {
        Expr::Sym(stringify!($name).to_string())
    };
    ($name: ident($($args: tt)*)) => {
        Expr::Fun(stringify!($name).to_string(), fun_args!($($args)*))
    };

}

#[cfg(test)]
#[macro_use]
mod tests {
    use super::*;
    #[test]
    pub fn rule_apply_all() {
        let swap = Rule {
            head: expr!(swap(pair(a, b))),
            body: expr!(pair(b, a)),
        };

        let expr = expr!(foo(swap(pair(f(a), g(b))), swap(pair(q(z), k(y)))));
        let expected = expr!(foo(pair(g(b), f(a)), pair(k(y), q(z))));
        println!("Rule:         {swap}");
        println!("Expression:   {expr}");
        println!("Expression':  {}", swap.apply_all(&expr));

        assert_eq!(swap.apply_all(&expr), expected);
    }
}

fn main() {
    // let lexer = Lexer::from_iter("swap( pair(a, b))".chars());

    // let expr = Expr::parse(lexer);
    let swap = Rule {
        head: expr!(swap(pair(a, b))),
        body: expr!(pair(b, a)),
    };

    let mut command = String::new();
    let mut quit = false;

    while !quit {
        command.clear();
        print!("> ");
        stdout().flush().unwrap();
        stdin().read_line(&mut command).unwrap();
        println!("{}",swap.apply_all(&Expr::parse(Lexer::from_iter(command.chars()))));
    }

    // println!("{}", swap.apply_all(&expr));
}
