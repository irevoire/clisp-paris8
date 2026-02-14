#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Lexeme {
    ParensOpen,
    ParensClose,
    Dot,
    Quote,
    String,
    Identifier,
    Comment,
    WhiteSpace,
    // Set to true if there is multiple consecutive newlines
    NewLine(bool),
}

impl Lexeme {
    pub fn to_string_expected(&self) -> String {
        match self {
            Lexeme::ParensOpen => String::from("a `(`"),
            Lexeme::ParensClose => String::from("a `)`"),
            Lexeme::Dot => String::from("a `.`"),
            Lexeme::Quote => String::from("a `'`"),
            Lexeme::String => String::from("a string"),
            Lexeme::Identifier => String::from("an identifier"),
            Lexeme::Comment => String::from("a comment"),
            Lexeme::WhiteSpace => String::from("spaces"),
            Lexeme::NewLine(_) => String::from("newlines"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub lex: Lexeme,
    pub span: Span,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    if input.is_empty() {
        return Vec::new();
    }
    let mut output = Vec::new();

    let mut next = 0;

    while next < input.len() {
        let Some(c) = input[next..].chars().next() else {
            return output;
        };
        let current = next;
        next += c.len_utf8();
        match c {
            ';' => {
                while let Some(c) = input[next..].chars().next()
                    && c != '\n'
                {
                    next += c.len_utf8();
                }
                // we might overshoot here if we reached EoF
                next += '\n'.len_utf8();

                output.push(Token {
                    lex: Lexeme::Comment,
                    span: Span {
                        start: current,
                        end: next.min(input.len()),
                    },
                })
            }
            '.' | '(' | ')' | '\'' => output.push(Token {
                lex: match c {
                    '.' => Lexeme::Dot,
                    '(' => Lexeme::ParensOpen,
                    ')' => Lexeme::ParensClose,
                    '\'' => Lexeme::Quote,
                    _ => unreachable!(),
                },
                span: Span {
                    start: current,
                    end: next,
                },
            }),
            '"' => {
                while let Some(c) = input[next..].chars().next()
                    && c != '"'
                {
                    next += c.len_utf8();
                }
                next += c.len_utf8();

                output.push(Token {
                    lex: Lexeme::String,
                    span: Span {
                        start: current,
                        end: next,
                    },
                })
            }
            c if c.is_whitespace() => {
                let mut new_line = (c == '\n') as usize;

                while let Some(c) = input[next..].chars().next() {
                    if c.is_whitespace() {
                        new_line += (c == '\n') as usize;
                        next += c.len_utf8();
                    } else {
                        break;
                    }
                }
                output.push(Token {
                    lex: match new_line {
                        0 => Lexeme::WhiteSpace,
                        1 => Lexeme::NewLine(false),
                        _ => Lexeme::NewLine(true),
                    },
                    span: Span {
                        start: current,
                        end: next,
                    },
                })
            }
            _c => {
                while let Some(c) = input[next..].chars().next()
                    && (!['"', ' ', '(', ')'].contains(&c))
                {
                    next += c.len_utf8();
                }
                output.push(Token {
                    lex: Lexeme::Identifier,
                    span: Span {
                        start: current,
                        end: next,
                    },
                })
            }
        }
    }

    output
}

#[cfg(test)]
mod test {
    use insta::assert_debug_snapshot;

    use super::*;

    #[test]
    fn test_tokenize() {
        assert_debug_snapshot!(tokenize(" "), @r"
        [
            Token {
                lex: WhiteSpace,
                span: Span {
                    start: 0,
                    end: 1,
                },
            },
        ]
        ");
        assert_debug_snapshot!(tokenize("\n"), @r"
        [
            Token {
                lex: NewLine(
                    false,
                ),
                span: Span {
                    start: 0,
                    end: 1,
                },
            },
        ]
        ");
        assert_debug_snapshot!(tokenize("\n  \n \n"), @r"
        [
            Token {
                lex: NewLine(
                    true,
                ),
                span: Span {
                    start: 0,
                    end: 6,
                },
            },
        ]
        ");
        assert_debug_snapshot!(tokenize("hello"), @r"
        [
            Token {
                lex: Identifier,
                span: Span {
                    start: 0,
                    end: 5,
                },
            },
        ]
        ");
        assert_debug_snapshot!(tokenize("'hello"), @r"
        [
            Token {
                lex: Quote,
                span: Span {
                    start: 0,
                    end: 1,
                },
            },
            Token {
                lex: Identifier,
                span: Span {
                    start: 1,
                    end: 6,
                },
            },
        ]
        ");
        assert_debug_snapshot!(tokenize("(hello world)"), @r"
        [
            Token {
                lex: ParensOpen,
                span: Span {
                    start: 0,
                    end: 1,
                },
            },
            Token {
                lex: Identifier,
                span: Span {
                    start: 1,
                    end: 6,
                },
            },
            Token {
                lex: WhiteSpace,
                span: Span {
                    start: 6,
                    end: 7,
                },
            },
            Token {
                lex: Identifier,
                span: Span {
                    start: 7,
                    end: 12,
                },
            },
            Token {
                lex: ParensClose,
                span: Span {
                    start: 12,
                    end: 13,
                },
            },
        ]
        ");
    }

    #[test]
    fn test_tokenize_string() {
        assert_debug_snapshot!(tokenize("\" hello\""), @r"
        [
            Token {
                lex: String,
                span: Span {
                    start: 0,
                    end: 8,
                },
            },
        ]
        ");

        // TODO: If we're missing the closing quote we should throw an error but that's not the case currently
        assert_debug_snapshot!(tokenize("\" hello"), @r"
        [
            Token {
                lex: String,
                span: Span {
                    start: 0,
                    end: 8,
                },
            },
        ]
        ");
    }

    #[test]
    fn test_tokenize_comment() {
        assert_debug_snapshot!(tokenize("; hello world (I'm an expression)"), @r"
        [
            Token {
                lex: Comment,
                span: Span {
                    start: 0,
                    end: 33,
                },
            },
        ]
        ");

        assert_debug_snapshot!(tokenize(";\nhello"), @r"
        [
            Token {
                lex: Comment,
                span: Span {
                    start: 0,
                    end: 2,
                },
            },
            Token {
                lex: Identifier,
                span: Span {
                    start: 2,
                    end: 7,
                },
            },
        ]
        ");

        assert_debug_snapshot!(tokenize(";\n\nhello"), @r"
        [
            Token {
                lex: Comment,
                span: Span {
                    start: 0,
                    end: 2,
                },
            },
            Token {
                lex: NewLine(
                    false,
                ),
                span: Span {
                    start: 2,
                    end: 3,
                },
            },
            Token {
                lex: Identifier,
                span: Span {
                    start: 3,
                    end: 8,
                },
            },
        ]
        ");
    }
}
