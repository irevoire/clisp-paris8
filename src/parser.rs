use std::{fmt, iter::Peekable};

use miette::{NamedSource, SourceSpan};

use crate::{
    errors::{Error, ErrorKind, ParseError},
    tokenizer::{Lexeme, Span, Token, tokenize},
};

#[derive(Debug)]
pub enum Expression {
    Ref {
        span: Span,
        expr: Box<Expression>,
    },
    List {
        opening_span: Span,
        list: Vec<Expression>,
        closing_span: Span,
    },
    Literal(Value),
}

impl Expression {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ref { span, expr } => {
                write!(f, "{}", &source[span.start..span.end])?;
                expr.display(source, f)
            }
            Expression::List {
                opening_span,
                list,
                closing_span,
            } => {
                write!(f, "{}", &source[opening_span.start..opening_span.end])?;
                for expr in list {
                    expr.display(source, f)?;
                }
                write!(f, "{}", &source[closing_span.start..closing_span.end])
            }
            Expression::Literal(value) => value.display(source, f),
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Ident(Span),
    String(Span),
    Number { span: Span, value: f64 },
    Dot(Span),
}

impl Value {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Dot(span) | Value::Number { span, value: _ } | Value::Ident(span) => {
                write!(f, "{}", &source[span.start..span.end])
            }
            Value::String(span) => write!(f, "\"{}\"", &source[span.start..span.end]),
        }
    }
}

impl From<Span> for SourceSpan {
    fn from(span: Span) -> Self {
        (span.start, span.len()).into()
    }
}

type TokenStream = Peekable<std::vec::IntoIter<Token>>;

struct Parser<'a> {
    content: &'a str,
    tokens: TokenStream,
}

#[derive(Debug)]
pub struct ParsedCode {
    src: NamedSource<String>,
    top_level: Vec<Expression>,
}

impl fmt::Display for ParsedCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for expr in self.top_level.iter() {
            expr.display(self.src.inner(), f)?;
        }
        Ok(())
    }
}

pub fn parse(filename: &str, content: &str) -> Result<ParsedCode, Error> {
    let src = NamedSource::new(filename, content.to_string());
    if content.trim().is_empty() {
        return Ok(ParsedCode {
            src,
            top_level: Vec::new(),
        });
    }

    let tokens = tokenize(content);
    let tokens = tokens.into_iter().peekable();
    let mut this = Parser { content, tokens };
    let mut top_level = Vec::new();

    while this.tokens.peek().is_some() {
        let expr = this.parse_expr();
        top_level.push(expr.map_err(|err| Error {
            src: src.clone(),
            error: ErrorKind::ParseError(err),
        })?);
    }

    Ok(ParsedCode { src, top_level })
}

impl Parser<'_> {
    fn parse_expr(&mut self) -> Result<Expression, ParseError> {
        match self.tokens.peek() {
            Some(Token {
                lex: Lexeme::ParensOpen,
                span: _,
            }) => self.parse_list(),
            Some(Token {
                lex: Lexeme::WhiteSpace | Lexeme::NewLine(_),
                span: _,
            }) => {
                self.tokens.next();
                self.parse_expr()
            }
            Some(Token {
                lex: Lexeme::Identifier,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Literal(Value::Ident(span)))
            }
            Some(Token {
                lex: Lexeme::String,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Literal(Value::String(span)))
            }
            Some(Token {
                lex: Lexeme::Quote,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Ref {
                    span,
                    expr: Box::new(self.parse_expr()?),
                })
            }
            Some(rcv) => todo!("{:?}", rcv),
            None => Err(ParseError::ExpectedExpressionButFoundNothing {
                // point to the end of the file
                at: (self.content.len().saturating_sub(1), 0).into(),
            }),
        }
    }

    fn skip_whitespace(&mut self) -> Option<Span> {
        let tok = self
            .tokens
            .peek()
            .filter(|tok| matches!(tok.lex, Lexeme::WhiteSpace | Lexeme::NewLine(_)))
            .map(|peek| peek.span);
        if tok.is_some() {
            self.tokens.next().unwrap();
        }
        tok
    }

    fn parse_list(&mut self) -> Result<Expression, ParseError> {
        let opening = self.tokens.next().unwrap();
        assert_eq!(opening.lex, Lexeme::ParensOpen);
        self.skip_whitespace();

        let mut list = Vec::new();

        while let Some(current) = self.tokens.peek()
            && current.lex != Lexeme::ParensClose
        {
            dbg!(current.lex);
            match self.parse_expr() {
                Ok(expr) => list.push(expr),
                Err(ParseError::ExpectedExpressionButFoundNothing { at }) => {
                    return Err(ParseError::MissingClosingParen {
                        matching: opening.span.into(),
                        here: at,
                    });
                }
                Err(e) => return Err(e),
            };
            self.skip_whitespace();
            println!("parsed {:?}", list.last().unwrap());
        }

        if self.tokens.peek().is_none() {
            return Err(ParseError::MissingClosingParen {
                matching: opening.span.into(),
                here: (self.content.len().saturating_sub(1), 0).into(),
                // point to the end of the file
            });
        }

        let closing = self.tokens.next().unwrap();
        assert_eq!(closing.lex, Lexeme::ParensClose);

        Ok(Expression::List {
            opening_span: opening.span,
            list,
            closing_span: closing.span,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn empty() {
        assert_snapshot!(parse("kefir", "").unwrap(), @"");
    }

    #[test]
    fn expected_expression() {
        let diag = parse("kefir", "(").unwrap_err().to_test_report();
        assert_snapshot!(diag, @r"
         × Expecting closing parenthesis or expression
          ╭─[kefir:1:1]
        1 │ (
          · ┬▲
          · │╰── Was expecting closing parens or expression but reached end of file
          · ╰── Opening parens here
          ╰────
        ");

        let diag = parse("kefir", "     (  ").unwrap_err().to_test_report();
        assert_snapshot!(diag, @r"
         × Expecting closing parenthesis or expression
          ╭─[kefir:1:6]
        1 │      (  
          ·      ┬ ▲
          ·      │ ╰── Was expecting closing parens or expression but reached end of file
          ·      ╰── Opening parens here
          ╰────
        ");
    }

    #[test]
    fn missing_closing_parens() {
        let diag = parse("kefir", "(kouf").unwrap_err().to_test_report();
        assert_snapshot!(diag, @r"
         × Expecting closing parenthesis or expression
          ╭─[kefir:1:1]
        1 │ (kouf
          · ┬   ▲
          · │   ╰── Was expecting closing parens or expression but reached end of file
          · ╰── Opening parens here
          ╰────
        ");
    }

    #[test]
    fn empty_list() {
        assert_snapshot!(parse("kefir", "()").unwrap(), @"()");
    }

    #[test]
    fn normal_list() {
        assert_snapshot!(parse("kefir", "(hello 'world )").unwrap(), @"(hello'world)");
    }
}
