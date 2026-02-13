use std::{fmt, iter::Peekable};

use miette::{NamedSource, SourceSpan};

use crate::{
    errors::{Error, ErrorKind, ParseError},
    tokenizer::{Lexeme, Span, Token, tokenize},
};

#[derive(Debug, Clone)]
pub struct Comments {
    comments: Vec<Comment>,
}

impl Comments {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for comment in self.comments.iter() {
            comment.display(source, f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Comment {
    comment: Option<Span>,
    space: Option<Token>,
}

impl Comment {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(comment) = self.comment {
            write!(f, "{}", &source[comment.start..comment.end])?;
        }
        if let Some(Token { lex: _, span }) = self.space {
            write!(f, "{}", &source[span.start..span.end])?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum Expression {
    Ref {
        comments: Comments,
        span: Span,
        expr: Box<Expression>,
        ws: Vec<Token>,
    },
    List {
        comments_1: Comments,
        opening_span: Span,
        comments_2: Comments,
        list: Vec<Expression>,
        comments_3: Comments,
        closing_span: Span,
        ws: Vec<Token>,
    },
    Literal {
        comments: Comments,
        lit: Value,
        ws: Vec<Token>,
    },
}

impl Expression {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ref {
                comments,
                span,
                expr,
                ws,
            } => {
                comments.display(source, f)?;
                write!(f, "{}", &source[span.start..span.end])?;
                expr.display(source, f)?;
                for ws in ws {
                    write!(f, "{}", &source[ws.span.start..ws.span.end])?;
                }
                Ok(())
            }
            Expression::List {
                comments_1,
                opening_span,
                comments_2,
                list,
                comments_3,
                closing_span,
                ws,
            } => {
                comments_1.display(source, f)?;
                write!(f, "{}", &source[opening_span.start..opening_span.end])?;
                comments_2.display(source, f)?;
                for expr in list {
                    expr.display(source, f)?;
                }
                comments_3.display(source, f)?;
                write!(f, "{}", &source[closing_span.start..closing_span.end])?;
                for ws in ws {
                    write!(f, "{}", &source[ws.span.start..ws.span.end])?;
                }
                Ok(())
            }
            Expression::Literal { comments, lit, ws } => {
                comments.display(source, f)?;

                lit.display(source, f)?;

                for ws in ws {
                    write!(f, "{}", &source[ws.span.start..ws.span.end])?;
                }
                Ok(())
            }
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
        let comments = self.get_comments_and_whitespaces();

        match self.tokens.peek() {
            Some(Token {
                lex: Lexeme::ParensOpen,
                span: _,
            }) => self.parse_list(comments),
            Some(Token {
                lex: Lexeme::WhiteSpace | Lexeme::NewLine(_),
                span: _,
            }) => {
                unreachable!();
                // self.tokens.next();
                // self.parse_expr()
            }
            Some(Token {
                lex: Lexeme::Identifier,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Literal {
                    comments,
                    lit: Value::Ident(span),
                    ws: self.get_whitespaces(),
                })
            }
            Some(Token {
                lex: Lexeme::String,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Literal {
                    comments,
                    lit: Value::String(span),
                    ws: self.get_whitespaces(),
                })
            }
            Some(Token {
                lex: Lexeme::Quote,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Ref {
                    comments,
                    span,
                    expr: Box::new(self.parse_expr()?),
                    ws: self.get_whitespaces(),
                })
            }
            Some(rcv) => todo!("{:?}", rcv),
            None => Err(ParseError::ExpectedExpressionButFoundNothing {
                // point to the end of the file
                at: (self.content.len().saturating_sub(1), 0).into(),
            }),
        }
    }

    fn get_whitespaces(&mut self) -> Vec<Token> {
        let mut comments = Vec::new();

        while let Some(Token {
            lex: Lexeme::WhiteSpace | Lexeme::NewLine(_),
            span: _,
        }) = self.tokens.peek()
        {
            comments.push(self.tokens.next().unwrap());
        }

        comments
    }

    fn get_comments_and_whitespaces(&mut self) -> Comments {
        let mut comments = Vec::new();

        loop {
            let mut should_break = true;
            let mut comment = Comment {
                comment: None,
                space: None,
            };

            match self.tokens.peek() {
                // TODO: Handle comments here
                // Some(Lexeme::()) => break,
                Some(Token {
                    lex: Lexeme::WhiteSpace | Lexeme::NewLine(_),
                    span: _,
                }) => {
                    should_break = false;
                    comment.space = Some(self.tokens.next().unwrap());
                }
                Some(_) => (),
                None => break,
            }

            if should_break {
                break;
            }

            comments.push(comment);
        }

        Comments { comments }
    }

    fn parse_list(&mut self, comments_1: Comments) -> Result<Expression, ParseError> {
        let opening = self.tokens.next().unwrap();
        assert_eq!(opening.lex, Lexeme::ParensOpen);
        let comments_2 = self.get_comments_and_whitespaces();

        let mut list = Vec::new();

        while let Some(current) = self.tokens.peek()
            && current.lex != Lexeme::ParensClose
        {
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
            self.get_comments_and_whitespaces();
        }

        let comments_3 = self.get_comments_and_whitespaces();

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
            comments_1,
            opening_span: opening.span,
            comments_2,
            list,
            comments_3,
            closing_span: closing.span,
            ws: self.get_whitespaces(),
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
        assert_snapshot!(dbg!(parse("kefir", "(hello 'world )").unwrap()), @"(hello 'world )");
    }

    #[test]
    fn normal_list_with_newlines() {
        assert_snapshot!(dbg!(parse("kefir", "(    hello
            'world    
            kefir  )").unwrap()), @r"
        (    hello
                    'world    
                    kefir  )
        ");
    }
}
