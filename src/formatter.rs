use miette::NamedSource;

use crate::{
    parser::{Comments, Expression, ParsedCode, Value, Whitespaces},
    tokenizer::{Lexeme, Token},
};
use std::{
    any::Any,
    borrow::Cow,
    fmt::{self, Write},
};

pub fn format(parsed: &ParsedCode) -> String {
    let mut output = String::new();
    let source = parsed.src.inner();
    for expr in parsed.top_level.iter() {
        expr.format(source, 0, false, &mut output).unwrap();
        match expr.last_ws().is_multiline() {
            Multiline::No => (),
            Multiline::Yes => writeln!(output).unwrap(),
            Multiline::Multi => writeln!(output, "\n").unwrap(),
        }
    }

    output
}

impl Comments {
    fn format(
        &self,
        src: &str,
        level: usize,
        mut skip_next_ws: bool,
        w: &mut impl Write,
    ) -> std::fmt::Result {
        for comment in self.comments.iter() {
            let mut already_inserted_newlines = skip_next_ws;
            skip_next_ws = false;

            if let Some(comment) = comment.comment {
                let comment = src[comment.start..comment.end].trim();
                writeln!(w, "{}{comment}", "  ".repeat(level))?;
                already_inserted_newlines = true;
            }
            if let Some(ws) = comment.space {
                match ws.is_multiline() {
                    Multiline::No => (),
                    _ if already_inserted_newlines => writeln!(w)?,
                    Multiline::Yes => writeln!(w)?,
                    Multiline::Multi => writeln!(w, "\n")?,
                };
            }
        }
        Ok(())
    }

    fn is_multiline(&self) -> Multiline {
        let mut ret = Multiline::No;
        for comment in self.comments.iter() {
            if let Some(_comment) = comment.comment {
                // A comment inevitably ends with a newlines
                ret = Multiline::Yes;
            }
            if let Some(ws) = comment.space {
                ret = ret.or(ws.is_multiline());
            }
        }
        ret
    }
}

impl Token {
    fn is_multiline(&self) -> Multiline {
        match self.lex {
            Lexeme::NewLine(false) => Multiline::Yes,
            Lexeme::NewLine(true) => Multiline::Multi,
            _ => Multiline::No,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Multiline {
    No,
    Yes,
    Multi,
}

impl Multiline {
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Multiline::No, Multiline::No) => Multiline::No,
            (Multiline::Multi, _) | (_, Multiline::Multi) => Multiline::Multi,
            (Multiline::No, Multiline::Yes) => Multiline::Yes,
            (Multiline::Yes, Multiline::No) => Multiline::Yes,
            (Multiline::Yes, Multiline::Yes) => Multiline::Multi,
        }
    }
}

impl Whitespaces {
    fn is_multiline(&self) -> Multiline {
        let mut ret = Multiline::No;

        for ws in self.ws.iter() {
            match ws.lex {
                Lexeme::WhiteSpace => (),
                Lexeme::NewLine(false) => ret = Multiline::Yes,
                Lexeme::NewLine(true) => ret = Multiline::Multi,
                _ => unreachable!(),
            }
        }

        ret
    }
}

impl Expression {
    fn is_multiline(&self) -> Multiline {
        match self {
            Expression::Ref {
                comments,
                span: _,
                expr,
                ws: _,
            } => comments.is_multiline().or(expr.is_multiline()),
            Expression::List {
                comments_1,
                opening_span: _,
                list,
                comments_2: comments_3,
                closing_span: _,
                ws: _,
            } => comments_1
                .is_multiline()
                .or(list
                    .iter()
                    .fold(Multiline::No, |acc, expr| acc.or(expr.is_multiline())))
                .or(comments_3.is_multiline()),
            Expression::Literal {
                comments,
                lit: _,
                ws: _,
            } => comments.is_multiline(),
            Expression::FinalComments { comments } => comments.is_multiline(),
        }
    }

    fn format(
        &self,
        src: &str,
        level: usize,
        skip_next_ws: bool,
        w: &mut impl Write,
    ) -> std::fmt::Result {
        match self {
            Expression::Ref {
                comments,
                span: _,
                expr,
                ws: _,
            } => {
                comments.format(src, level, skip_next_ws, w)?;
                write!(w, "{}'", "  ".repeat(level))?;
                expr.format(src, 0, skip_next_ws, w)?;
                Ok(())
            }
            Expression::List {
                comments_1,
                opening_span: _,
                list,
                comments_2,
                closing_span: _,
                ws: _,
            } => {
                let s = ParsedCode {
                    src: NamedSource::new("kefir", src.to_string()),
                    top_level: vec![list[0].clone()],
                }
                .to_string();
                println!("{s} is {:?}", self.is_multiline());
                if self.is_multiline() == Multiline::No {
                    // if everything fits on a single line it means there is no comments
                    write!(w, "{}(", "  ".repeat(level))?;
                    for (i, expr) in list.iter().enumerate() {
                        // we set the level to 0 to be sure no spaces are inserted
                        expr.format(src, 0, skip_next_ws, w)?;
                        if i != list.len() - 1 {
                            write!(w, " ")?;
                        }
                    }
                    write!(w, ")")?;
                } else {
                    comments_1.format(src, level + 1, skip_next_ws, w)?;
                    write!(w, "{}(", "  ".repeat(level))?;
                    for (i, expr) in list.iter().enumerate() {
                        expr.format(src, level + 1, true, w)?;
                        if i != list.len() - 1 {
                            writeln!(w)?;
                        }
                    }
                    comments_2.format(src, level + 1, true, w)?;
                    write!(w, " )")?;
                }

                Ok(())
            }
            Expression::Literal {
                comments,
                lit,
                ws: _,
            } => {
                comments.format(src, level, skip_next_ws, w)?;
                lit.format(src, level, w)?;

                Ok(())
            }
            Expression::FinalComments { comments } => comments.format(src, level, skip_next_ws, w),
        }
    }

    fn last_ws(&self) -> Cow<'_, Whitespaces> {
        match self {
            Expression::Ref { ws, .. } => Cow::Borrowed(ws),
            Expression::List { ws, .. } => Cow::Borrowed(ws),
            Expression::Literal { ws, .. } => Cow::Borrowed(ws),
            Expression::FinalComments { comments } => Cow::Owned(Whitespaces {
                ws: comments
                    .comments
                    .iter()
                    .filter_map(|comment| comment.space)
                    .collect(),
            }),
        }
    }
}

impl Value {
    fn format(&self, src: &str, level: usize, w: &mut impl Write) -> std::fmt::Result {
        match self {
            Value::Ident(span) => write!(w, "{}{}", "  ".repeat(level), &src[span.start..span.end]),
            Value::String(span) => {
                write!(w, "{}{}", "  ".repeat(level), &src[span.start..span.end])
            }
            Value::Number { span, value: _ } => {
                write!(w, "{}{}", "  ".repeat(level), &src[span.start..span.end])
            }
            Value::Dot(span) => write!(w, "{}{}", "  ".repeat(level), &src[span.start..span.end]),
        }
    }
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;

    use crate::parser::parse;

    fn fmt(s: &str) -> String {
        let parsed = parse("kefir", s).unwrap();
        super::format(&parsed)
    }

    #[test]
    fn test_format_literal() {
        assert_snapshot!(fmt("kefir"), @"kefir");
        assert_snapshot!(fmt("42"), @"42");
        assert_snapshot!(fmt("+42"), @"+42");
        assert_snapshot!(fmt("42+"), @"42+");
        assert_snapshot!(fmt("\"55\""), @r#""55""#);
    }

    #[test]
    fn test_format_literal_with_comment() {
        assert_snapshot!(fmt(";hello\nkefir"), @r"
        ;hello
        kefir
        ");
        assert_snapshot!(fmt(";hello    \nkefir"), @r"
        ;hello
        kefir
        ");
        assert_snapshot!(fmt(";hello \n\n\n\n\n\nkefir"), @r"
        ;hello

        kefir
        ");
        assert_snapshot!(fmt(";hello \n\n\n\n;world\n;!\n\nkefir"), @r"
        ;hello

        ;world
        ;!

        kefir
        ");
    }

    #[test]
    fn test_format_single_line_list() {
        assert_snapshot!(fmt("(hello world)"), @"(hello world)");
        assert_snapshot!(fmt("  (  hello    world  )  "), @"(hello world)");
        assert_snapshot!(fmt("(   ( hello) (   ) (world !) )"), @"((hello) () (world !))");
        assert_snapshot!(fmt("(set 'seau '(o o o ))"), @"(set 'seau '(o o o))");
    }

    #[test]
    fn test_format_two_single_line_list() {
        assert_snapshot!(fmt("(hello world)"), @"(hello world)");
        assert_snapshot!(fmt("  (  hello    world  )  "), @"(hello world)");
        assert_snapshot!(fmt("(   ( hello) (   ) (world !) )"), @"((hello) () (world !))");
        assert_snapshot!(fmt("(set 'seau '(o o o ))
            (print seau)"), @r"
        (set 'seau '(o o o))
        (print seau)
        ");
    }

    #[test]
    fn test_format_multi_line_list() {
        assert_snapshot!(fmt("(\nhello world)"), @r"
        (
          hello
          world )
        ");
        assert_snapshot!(fmt("  (  hello  \n  world  )  "), @r"
        (
          hello
          world
        )
        ");
        assert_snapshot!(fmt("(   ( hello) ( \n  ) (world !) )"), @r"
        (
          (hello)
          (

          )
          (world !)
        )
        ");
    }
}
