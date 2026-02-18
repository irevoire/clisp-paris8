use crate::{
    parser::{Comments, Expression, ParsedCode, Value, Whitespaces},
    tokenizer::{Lexeme, Token},
};
use std::fmt::Write;

pub fn format(parsed: &ParsedCode) -> String {
    let mut output = String::new();
    let source = parsed.src.inner();
    for expr in parsed.top_level.iter() {
        match expr.starts_by_ws() {
            Multiline::No => (),
            Multiline::Yes => writeln!(output).unwrap(),
            Multiline::Multi => writeln!(output, "\n").unwrap(),
        }
        expr.format(source, 0, true, &mut output).unwrap();
        match expr.is_multiline(true) {
            Multiline::No => (),
            Multiline::Yes | Multiline::Multi => writeln!(output).unwrap(),
            // Multiline::Multi => writeln!(output, "\n").unwrap(),
        }
    }

    output
}

impl Comments {
    fn starts_by_ws(&self) -> Multiline {
        if let Some(comment) = self.comments.first() {
            if comment.comment.is_some() {
                Multiline::No
            } else if let Some(ws) = comment.space {
                ws.is_multiline(false)
            } else {
                // this makes no sense and should never happens
                Multiline::No
            }
        } else {
            Multiline::No
        }
    }

    // Return `true` if we were supposed to skip a whitespace but didn't.
    // This means the caller should be skipping the next whitespace.
    fn format(
        &self,
        src: &str,
        level: usize,
        mut skip_next_ws: bool,
        w: &mut impl Write,
    ) -> Result<bool, std::fmt::Error> {
        for comment in self.comments.iter() {
            let mut already_inserted_newlines = skip_next_ws;

            if let Some(comment) = comment.comment {
                let comment = src[comment.start..comment.end].trim();
                writeln!(w, "{}{comment}", "  ".repeat(level))?;
                already_inserted_newlines = true;
                skip_next_ws = false;
            }
            if let Some(ws) = comment.space
                && !skip_next_ws
            {
                match ws.is_multiline(skip_next_ws) {
                    Multiline::No => (),
                    _ if already_inserted_newlines => writeln!(w)?,
                    Multiline::Yes => writeln!(w)?,
                    Multiline::Multi => writeln!(w, "\n")?,
                };
            }
        }
        Ok(skip_next_ws)
    }

    fn is_multiline(&self, mut skip_next_ws: bool) -> Multiline {
        let mut ret = Multiline::No;
        for comment in self.comments.iter() {
            if let Some(_comment) = comment.comment {
                // A comment inevitably ends with a newlines
                ret = Multiline::Yes;
                skip_next_ws = false;
            }
            if let Some(ws) = comment.space {
                if skip_next_ws {
                    skip_next_ws = false;
                } else {
                    ret = ret.or(ws.is_multiline(skip_next_ws));
                }
            }
        }
        ret
    }
}

impl Token {
    fn is_multiline(&self, skip_next_ws: bool) -> Multiline {
        match self.lex {
            _ if skip_next_ws => Multiline::No,
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
    fn starts_by_ws(&self) -> Multiline {
        match self {
            Expression::Ref {
                comments,
                span: _,
                expr: _,
            } => comments.starts_by_ws(),
            Expression::List {
                comments_1,
                opening_span: _,
                list: _,
                comments_2: _,
                closing_span: _,
            } => comments_1.starts_by_ws(),
            Expression::Literal { comments, lit: _ } => comments.starts_by_ws(),
            Expression::FinalComments { comments } => comments.starts_by_ws(),
        }
    }

    fn is_multiline(&self, skip_next_ws: bool) -> Multiline {
        match self {
            Expression::Ref {
                comments,
                span: _,
                expr,
            } => comments
                .is_multiline(skip_next_ws)
                .or(expr.is_multiline(false)),
            Expression::List {
                comments_1,
                opening_span: _,
                list,
                comments_2: comments_3,
                closing_span: _,
            } => comments_1
                .is_multiline(skip_next_ws)
                .or(list
                    .iter()
                    .fold(Multiline::No, |acc, expr| acc.or(expr.is_multiline(false))))
                .or(comments_3.is_multiline(false)),
            Expression::Literal { comments, lit: _ } => comments.is_multiline(skip_next_ws),
            Expression::FinalComments { comments } => comments.is_multiline(skip_next_ws),
        }
    }

    fn contains_comments(&self) -> bool {
        match self {
            Expression::Ref {
                comments,
                span: _,
                expr,
            } => comments.contains_comments() | expr.contains_comments(),
            Expression::List {
                comments_1,
                opening_span: _,
                list,
                comments_2: comments_3,
                closing_span: _,
            } => {
                comments_1.contains_comments()
                    | list
                        .iter()
                        .fold(false, |acc, expr| acc | expr.contains_comments())
                    | comments_3.contains_comments()
            }
            Expression::Literal { comments, lit: _ } => comments.contains_comments(),
            Expression::FinalComments { comments } => comments.contains_comments(),
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
            } => {
                let skip_next_ws = comments.format(src, level, skip_next_ws, w)?;
                if !skip_next_ws {
                    write!(w, "{}", "  ".repeat(level))?;
                }
                write!(w, "'")?;
                expr.format(src, 0, false, w)?;
                Ok(())
            }
            Expression::List {
                comments_1,
                opening_span: _,
                list,
                comments_2,
                closing_span: _,
            } => {
                if self.is_multiline(skip_next_ws) == Multiline::No {
                    // if everything fits on a single line it means there is no comments
                    if !skip_next_ws {
                        write!(w, "{}", "  ".repeat(level))?;
                    }
                    write!(w, "(")?;
                    for (i, expr) in list.iter().enumerate() {
                        // we set the level to 0 to be sure no spaces are inserted
                        expr.format(src, 0, true, w)?;
                        if i != list.len() - 1 {
                            write!(w, " ")?;
                        }
                    }
                    write!(w, ")")?;
                } else {
                    let skip_ident = comments_1.format(src, level + 1, skip_next_ws, w)?;
                    if !skip_ident {
                        write!(w, "{}", "  ".repeat(level))?;
                    }
                    write!(w, "(")?;
                    for (i, expr) in list.iter().enumerate() {
                        if i > 0 {
                            write!(w, "{}", "  ".repeat(level + 1))?;
                        }
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
            Expression::Literal { comments, lit } => {
                comments.format(src, level, skip_next_ws, w)?;
                lit.format(src, level, skip_next_ws, w)?;

                Ok(())
            }
            Expression::FinalComments { comments } => {
                comments.format(src, level, skip_next_ws, w)?;
                Ok(())
            }
        }
    }
}

impl Value {
    fn format(
        &self,
        src: &str,
        level: usize,
        skip_next_ws: bool,
        w: &mut impl Write,
    ) -> std::fmt::Result {
        if !skip_next_ws {
            write!(w, "{}", "  ".repeat(level))?;
        }
        match self {
            Value::Ident(span) => write!(w, "{}", &src[span.start..span.end]),
            Value::String(span) => {
                write!(w, "{}", &src[span.start..span.end])
            }
            Value::Number { span, value: _ } => {
                write!(w, "{}", &src[span.start..span.end])
            }
            Value::Dot(span) => write!(w, "{}", &src[span.start..span.end]),
        }
    }
}

#[cfg(test)]
mod test {
    use insta::assert_snapshot;

    use crate::parser::parse;

    fn fmt(s: &str) -> String {
        let parsed = parse("kefir", s).unwrap();
        dbg!(&parsed);
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
    fn test_format_multi_line_list() {
        assert_snapshot!(fmt("(\nhello world)"), @r"
        (hello
          world )
        ");
        assert_snapshot!(fmt("  (  hello  \n  world  )  "), @r"
        (hello
          world )
        ");
        assert_snapshot!(fmt("(hello world\n) "), @r"
        (hello
          world )
        ");
        assert_snapshot!(fmt("(   ( hello) ( \n  ) (world !) )"), @r"
        ((hello)
          ( )
          (world !) )
        ");
        assert_snapshot!(fmt("
(setq noooombre '(1 15 3 5 24 2 35 5))
(setq noooombre (nombres 12 noooombre))
(print noooombre)
"), @r"
        (setq noooombre '(1 15 3 5 24 2 35 5))
        (setq noooombre (nombres 12 noooombre))
        (print noooombre)
        ");
        assert_snapshot!(fmt("
(defun nombres (element liste)
  (cond
    ((not liste) nil)
    ((<= element (car liste)) (rplacd liste (nombres element (cdr liste))))
    ((nombres element (cdr liste))) ) )

"), @r"
        (defun
          nombres
          (element liste)
          (cond
            ((not liste) nil)
            ((<= element (car liste)) (rplacd liste (nombres element (cdr liste))))
            ((nombres element (cdr liste))) ) )
        ");
    }

    #[test]
    fn test_format_multi_line_list_with_spaces() {
        assert_snapshot!(fmt("
(setq atooome (atomes  '(a (b (c) (d)) (e f) (g (h)) nil)))
(print atooome)

(setq atoooome (atomes '(a . d)))
(print atoooome)
"), @r"
        (setq atooome (atomes '(a (b (c) (d)) (e f) (g (h)) nil)))
        (print atooome)

        (setq atoooome (atomes '(a . d)))
        (print atoooome)
        ");
    }
}
