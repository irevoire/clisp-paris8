use std::{fmt, iter::Peekable};

use miette::{NamedSource, SourceSpan};

use crate::{
    errors::{Error, ErrorKind, ParseError},
    tokenizer::{Lexeme, Span, Token, tokenize},
};

#[derive(Debug, Clone, Default)]
pub struct Comments {
    pub comments: Vec<Comment>,
}

impl Comments {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for comment in self.comments.iter() {
            comment.display(source, f)?;
        }
        Ok(())
    }

    pub fn contains_comments(&self) -> bool {
        self.comments
            .iter()
            .any(|comment| comment.comment.is_some())
    }
}

#[derive(Debug, Clone)]
pub struct Comment {
    pub comment: Option<Span>,
    pub space: Option<Token>,
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

#[derive(Debug, Clone)]
pub struct Whitespaces {
    pub ws: Vec<Token>,
}

impl Whitespaces {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for ws in self.ws.iter() {
            write!(f, "{}", &source[ws.span.start..ws.span.end])?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Ref {
        comments: Comments,
        span: Span,
        expr: Box<Expression>,
    },
    List {
        comments_1: Comments,
        opening_span: Span,
        list: Vec<Expression>,
        comments_2: Comments,
        closing_span: Span,
    },
    Literal {
        comments: Comments,
        lit: Value,
    },
    // The very last comments of a file. Cannot be followed by anything and can only be returned when reaching EoF
    FinalComments {
        comments: Comments,
    },
}

impl Expression {
    pub fn display(&self, source: &str, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Ref {
                comments,
                span,
                expr,
            } => {
                comments.display(source, f)?;
                write!(f, "{}", &source[span.start..span.end])?;
                expr.display(source, f)
            }
            Expression::List {
                comments_1,
                opening_span,
                list,
                comments_2: comments_3,
                closing_span,
            } => {
                comments_1.display(source, f)?;
                write!(f, "{}", &source[opening_span.start..opening_span.end])?;
                for expr in list {
                    expr.display(source, f)?;
                }
                comments_3.display(source, f)?;
                write!(f, "{}", &source[closing_span.start..closing_span.end])
            }
            Expression::Literal { comments, lit } => {
                comments.display(source, f)?;
                lit.display(source, f)
            }
            Expression::FinalComments { comments } => comments.display(source, f),
        }
    }
}

#[derive(Debug, Clone)]
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

impl From<SourceSpan> for Span {
    fn from(span: SourceSpan) -> Self {
        Span {
            start: span.offset(),
            end: span.offset() + span.len(),
        }
    }
}

type TokenStream = Peekable<std::vec::IntoIter<Token>>;

struct Parser<'a> {
    content: &'a str,
    tokens: TokenStream,
}

#[derive(Debug)]
pub struct ParsedCode {
    pub src: NamedSource<String>,
    pub top_level: Vec<Expression>,
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
        match this.parse_expr() {
            Ok(expr) => top_level.push(expr),
            Err(ParseError::ExpectedExpressionButFoundNothing { at: _, comments }) => {
                top_level.push(Expression::FinalComments { comments });
            }
            Err(err) => {
                return Err(Error {
                    src: src.clone(),
                    error: ErrorKind::ParseError(err),
                });
            }
        }
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
                })
            }
            Some(Token {
                lex: Lexeme::Dot,
                span,
            }) => {
                let span = *span;
                self.tokens.next().unwrap();
                Ok(Expression::Literal {
                    comments,
                    lit: Value::Dot(span),
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
                })
            }
            Some(rcv) => Err(ParseError::UnexpectedToken {
                expecting: ["(", "'", "\""].iter().map(|s| s.to_string()).collect(),
                got: rcv.lex,
                here: rcv.span.into(),
                comments,
            }),
            None => Err(ParseError::ExpectedExpressionButFoundNothing {
                // point to the end of the file
                at: (self.content.len().saturating_sub(1), 0).into(),
                comments,
            }),
        }
    }

    fn get_comments_and_whitespaces(&mut self) -> Comments {
        let mut comments = Vec::new();

        loop {
            let mut should_break = true;
            let mut comment = Comment {
                comment: None,
                space: None,
            };

            if let Some(Token {
                lex: Lexeme::Comment,
                span: _,
            }) = self.tokens.peek()
            {
                should_break = false;
                comment.comment = Some(self.tokens.next().unwrap().span);
            }

            if let Some(Token {
                lex: Lexeme::WhiteSpace | Lexeme::NewLine(_),
                span: _,
            }) = self.tokens.peek()
            {
                should_break = false;
                comment.space = Some(self.tokens.next().unwrap());
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

        let mut list = Vec::new();

        let mut comments_3 = Comments::default();
        let mut closing_span = None;

        while self.tokens.peek().is_some() {
            match self.parse_expr() {
                Ok(expr) => list.push(expr),
                Err(ParseError::ExpectedExpressionButFoundNothing { at, comments: _ }) => {
                    return Err(ParseError::MissingClosingParen {
                        matching: opening.span.into(),
                        here: at,
                    });
                }
                Err(ParseError::UnexpectedToken {
                    expecting: _,
                    got: Lexeme::ParensClose,
                    here,
                    comments,
                }) => {
                    comments_3 = comments;
                    closing_span = Some(here);
                    self.tokens.next().unwrap();
                    break;
                }
                Err(e) => return Err(e),
            };
        }

        if closing_span.is_none() {
            return Err(ParseError::MissingClosingParen {
                matching: opening.span.into(),
                here: (self.content.len().saturating_sub(1), 0).into(),
                // point to the end of the file
            });
        }

        Ok(Expression::List {
            comments_1,
            opening_span: opening.span,
            list,
            comments_2: comments_3,
            closing_span: closing_span.unwrap().into(),
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
    fn toplevel_final_comment() {
        assert_snapshot!(parse("kefir", "; hello").unwrap(), @"; hello");
    }

    #[test]
    fn simple_comment() {
        assert_snapshot!(parse("kefir", "(hello;comment\n world )").unwrap(), @r"
        (hello;comment
         world )
        ");
    }

    #[test]
    fn normal_list() {
        assert_snapshot!(parse("kefir", "(hello 'world )").unwrap(), @"(hello 'world )");
    }

    #[test]
    fn normal_list_with_newlines_and_comments() {
        assert_snapshot!(parse("kefir", "; I'm a toplevel comment
            (; smol comment before the next expression
                hello
            'world    
            ; my world
            kefir  ; why not comment here
            )").unwrap(), @r"
        ; I'm a toplevel comment
                    (; smol comment before the next expression
                        hello
                    'world    
                    ; my world
                    kefir  ; why not comment here
                    )
        ");
    }

    #[test]
    fn normal_list_with_newlines() {
        assert_snapshot!(parse("kefir", "(    hello
            'world    
            kefir  )").unwrap(), @r"
        (    hello
                    'world    
                    kefir  )
        ");
    }
}
