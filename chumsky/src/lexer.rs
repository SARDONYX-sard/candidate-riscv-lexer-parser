use chumsky::{
    prelude::Simple,
    primitive::{filter, just, one_of, take_until},
    recovery::skip_then_retry_until,
    text::{self, TextParser},
    Parser,
};
use chumsky::{prelude::*, stream::Stream};
use core::fmt;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, env, fs};

use crate::utils::{Error, Span};

#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Null,
    /// e.g. -1
    Num(String),
    /// "string"
    Str(String),
    /// Operator
    ///
    /// # Example
    /// - expect `*`
    /// ```riscv
    /// _entry:
    ///   li a0, 1024*4
    /// ```
    Op(String),
    /// Control characters (delimiters, semicolons, etc.)
    ///
    /// # Example
    /// - expect `,`, `(`, `)`
    /// ```riscv
    /// _entry:
    ///   sd a1, 0(a0)
    /// ```
    Ctrl(char),
    /// `label_name` of `label_name:`
    ///
    /// # Example
    /// - expect `section`, `global`, `_entry`, `csrr`, `a1`, `mhartid`
    /// ```riscv
    /// .section .text
    /// .global _entry
    /// _entry:
    ///   csrr a1, mhartid
    /// ```
    Symbol(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Null => write!(f, "null"),
            Token::Num(n) => write!(f, "{}", n),
            Token::Str(s) => write!(f, "{}", s),
            Token::Op(s) => write!(f, "{}", s),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Symbol(s) => write!(f, "{}", s),
        }
    }
}

fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    // A parser for numbers
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+-*/!=")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};,").map(|c| Token::Ctrl(c));
    // A parser for identifiers and keywords
    let label = text::ident().map(|ident: String| match ident.as_str() {
        "null" => Token::Null,
        _ => Token::Symbol(ident),
    });

    // A single token can be one of the above
    let token = num
        .or(str_)
        .or(op)
        .or(ctrl)
        .or(label)
        .recover_with(skip_then_retry_until([]));

    let comment = just("#").then(take_until(just('\n'))).padded();

    token
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated()
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Value {
    Null,
    Num(f64),
    Str(String),
    Func(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
            Self::Num(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", x),
            Self::Func(name) => write!(f, "<function: {}>", name),
        }
    }
}

impl Value {
    fn num(self, span: Span) -> Result<f64, Error> {
        if let Value::Num(x) = self {
            Ok(x)
        } else {
            Err(Error {
                span,
                msg: format!("'{}' is not a number", self),
            })
        }
    }
}

/// Binary Operators
///
/// `+`, `-`, `*`, `/`, `==`, `!=`
#[derive(Clone, Debug)]
pub enum BinaryOp {
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `==`
    Eq,
    /// `!=`
    NotEq,
}

pub type Spanned<T> = (T, Span);

/// An expression node in the AST. Children are spanned so we can generate useful runtime errors.
#[derive(Debug)]
pub enum Expr {
    Error,
    Value(Value),
    List(Vec<Spanned<Self>>),
    Local(Spanned<String>),
    /// expect `addi a1, a1, 1`
    Inst(String, Box<Spanned<Self>>, Box<Spanned<Self>>, Span),
    Then(Box<Spanned<Self>>, Box<Spanned<Self>>),
    /// expect `1`, `+`, `1`
    Binary(Box<Spanned<Self>>, BinaryOp, Box<Spanned<Self>>),
    Call(Box<Spanned<Self>>, Spanned<Vec<Spanned<Self>>>),
}

impl Expr {
    /// Returns `true` if the expr is [`Error`].
    ///
    /// [`Error`]: Expr::Error
    fn is_error(&self) -> bool {
        matches!(self, Self::Error)
    }

    /// Returns `true` if the expr is [`Inst`].
    ///
    /// [`Inst`]: Expr::Inst
    fn is_let(&self) -> bool {
        matches!(self, Self::Inst(..))
    }

    /// Returns `true` if the expr is [`Value`].
    ///
    /// [`Value`]: Expr::Value
    fn is_value(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    /// Extract Value in Value. as Result
    fn try_into_value(self) -> Result<Value, Self> {
        if let Self::Value(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    /// Extract Value in Value. as Option
    fn as_value(&self) -> Option<&Value> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// A function node in the AST.
#[derive(Debug)]
pub struct Func {
    pub body: Spanned<Expr>,
    pub name: Spanned<String>,
    pub span: Span,
}

fn expr_parser() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let raw_expr = recursive(|raw_expr| {
            let val = filter_map(|span, tok| match tok {
                Token::Null => Ok(Expr::Value(Value::Null)),
                Token::Num(n) => Ok(Expr::Value(Value::Num(n.parse().unwrap()))),
                Token::Str(s) => Ok(Expr::Value(Value::Str(s))),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("value");

            // ([a-zA-Z0-9]('.'?[a-zA-Z0-9])*) | (\"(\\[^\n]|[^"\n])*\")
            let symbol = filter_map(|span, tok| match tok {
                Token::Symbol(ident) => Ok((ident.clone(), span)),
                _ => Err(Simple::expected_input_found(span, Vec::new(), Some(tok))),
            })
            .labelled("symbol");

            // A list of expressions
            // Null | Num | Str | Symbol {',' symbol}
            // - Example
            //   a1, a1, 1
            //   a1, a1, a1
            let items = expr
                .clone()
                .chain(just(Token::Ctrl(',')).ignore_then(expr.clone()).repeated())
                .then_ignore(just(Token::Ctrl(',')).or_not())
                .or_not()
                .map(|item| item.unwrap_or_else(Vec::new));

            // instruction ::= symbol [symbol {',' symbol}] [';']
            // instruction ::= symbol [operand {',' operand}]
            // - Example
            //   addi a1, a1, 1
            //   addi a1, a1, 1;
            let instruction = symbol
                .ignore_then(symbol)
                .then_ignore(just(Token::Ctrl(' ')).or_not())
                .then(raw_expr)
                .then_ignore(just(Token::Ctrl(';')).or_not())
                .then(expr.clone())
                .map(|((name, val), body)| {
                    Expr::Inst(name.0, Box::new(val), Box::new(body), name.1)
                });

            let list = items
                .clone()
                .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
                .map(Expr::List);

            // 'Atoms' are expressions that contain no ambiguity
            let atom = val
                .or(symbol.map(Expr::Local))
                .or(instruction)
                .or(list)
                .map_with_span(|expr, span| (expr, span))
                // Atoms can also just be normal expressions, but surrounded with parentheses
                .or(expr
                    .clone()
                    .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
                // Attempt to recover anything that looks like a parenthesized expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ))
                // Attempt to recover anything that looks like a list but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('['),
                    Token::Ctrl(']'),
                    [
                        (Token::Ctrl('('), Token::Ctrl(')')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ));

            // Function calls have very high precedence so we prioritise them
            let call = atom
                .then(
                    items
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                        .map_with_span(|args, span| (args, span))
                        .repeated(),
                )
                .foldl(|f, args| {
                    let span = f.1.start..args.1.end;
                    (Expr::Call(Box::new(f), args), span)
                });

            // Product ops (multiply and divide) have equal precedence
            let op = just(Token::Op("*".to_string()))
                .to(BinaryOp::Mul)
                .or(just(Token::Op("/".to_string())).to(BinaryOp::Div));
            let product = call
                .clone()
                .then(op.then(call).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Sum ops (add and subtract) have equal precedence
            let op = just(Token::Op("+".to_string()))
                .to(BinaryOp::Add)
                .or(just(Token::Op("-".to_string())).to(BinaryOp::Sub));
            let sum = product
                .clone()
                .then(op.then(product).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            // Comparison ops (equal, not-equal) have equal precedence
            let op = just(Token::Op("==".to_string()))
                .to(BinaryOp::Eq)
                .or(just(Token::Op("!=".to_string())).to(BinaryOp::NotEq));
            let compare = sum
                .clone()
                .then(op.then(sum).repeated())
                .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;
                    (Expr::Binary(Box::new(a), op, Box::new(b)), span)
                });

            compare
        });

        // Blocks are expressions but delimited with braces
        let block = expr
            .clone()
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| (Expr::Error, span),
            ));

        // Both blocks and `if` are 'block expressions' and can appear in the place of statements
        let block_expr = block.labelled("block");

        let block_chain = block_expr
            .clone()
            .then(block_expr.clone().repeated())
            .foldl(|a, b| {
                let span = a.1.start..b.1.end;
                (Expr::Then(Box::new(a), Box::new(b)), span)
            });

        block_chain
            // Expressions, chained by semicolons, are statements
            .or(raw_expr.clone())
            .then(just(Token::Ctrl(';')).ignore_then(expr.or_not()).repeated())
            .foldl(|a, b| {
                let span = a.1.clone(); // TODO: Not correct
                (
                    Expr::Then(
                        Box::new(a),
                        Box::new(match b {
                            Some(b) => b,
                            None => (Expr::Value(Value::Null), span.clone()),
                        }),
                    ),
                    span,
                )
            })
    })
}
