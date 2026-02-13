use miette::{Diagnostic, NamedSource, SourceSpan};
use thiserror::Error;

use crate::{parser::Comments, tokenizer::Lexeme};

#[derive(Debug, Error)]
#[error("{error}")]
pub struct Error {
    pub src: NamedSource<String>,
    pub error: ErrorKind,
}

impl miette::Diagnostic for Error {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.error.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.error.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.error.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.error.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.src)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.error.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.error.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.error.diagnostic_source()
    }
}

impl Error {
    #[cfg(test)]
    pub fn to_test_report(&self) -> String {
        let mut output = String::new();
        miette::GraphicalReportHandler::new()
            .with_theme(miette::GraphicalTheme::unicode_nocolor())
            .render_report(&mut output, self)
            .unwrap();
        output
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ErrorKind {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ParseError(#[from] ParseError),
}

#[derive(Error, Debug, Diagnostic)]
#[diagnostic()]
pub enum ParseError {
    #[error("Expected expression but found nothing")]
    ExpectedExpressionButFoundNothing {
        #[label("Expected expression here")]
        at: SourceSpan,
        comments: Comments,
    },
    #[error("Expecting closing parenthesis or expression")]
    MissingClosingParen {
        #[label("Opening parens here")]
        matching: SourceSpan,
        #[label("Was expecting closing parens or expression but reached end of file")]
        here: SourceSpan,
    },
    #[error("Was expecting {} {:?} but instead got {}",
        if expecting.len() == 1 { "a" } else { "one of" },
        expecting,
        got.to_string_expected(),
    )]
    UnexpectedToken {
        expecting: Vec<String>,
        got: Lexeme,
        #[label("Was expecting {} {:?}",
            if expecting.len() == 1 { "a" } else { "one of" },
            expecting,
        )]
        here: SourceSpan,
        comments: Comments,
    },
}
