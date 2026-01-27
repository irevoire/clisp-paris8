#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        self.end - self.start
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
    WhiteSpace,
    // Set to true if there is multiple consecutive newlines
    NewLine(bool),
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
                while let Some(c) = input[next..].chars().next() {
                    if c == '"' {
                        next += c.len_utf8();
                        break;
                    }
                }

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
                while let Some(c) = input[next..].chars().next() {
                    if c.is_whitespace() {
                        break;
                    } else {
                        next += c.len_utf8();
                    }
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
                    end: 13,
                },
            },
        ]
        ");
    }
}
