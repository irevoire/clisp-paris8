use clisp_formatter_paris8::errors::ParseError;
use miette::{
    Diagnostic, GraphicalReportHandler, GraphicalTheme, IntoDiagnostic, NamedSource, SourceSpan,
};

#[derive(Debug, thiserror::Error)]
#[error("{diag}")]
struct MainError {
    src: NamedSource<String>,
    diag: Diag,
}

impl miette::Diagnostic for MainError {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.diag.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        self.diag.severity()
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.diag.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.diag.url()
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.src)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.diag.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn Diagnostic> + 'a>> {
        self.diag.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.diag.diagnostic_source()
    }
}

#[derive(Debug, thiserror::Error, Diagnostic)]
enum Diag {
    #[error("error")]
    Variant {
        #[label("here")]
        label: SourceSpan,
    },
}

fn main() -> miette::Result<()> {
    let src = NamedSource::new("truc", "hello world!".to_string());
    // let err = ParseError::ExpectedExpressionButFoundNothing {
    //     src,
    //     at: ("hello world!".len() - 1, 0).into(),
    // };
    // let err = Result::<(), ParseError>::Err(ParseError::ExpectedExpressionButFoundNothing {
    //     src: NamedSource::new("kefir", "(".to_string()),
    //     at: (0, 0).into(),
    // })
    // .unwrap_err();
    let err = MainError {
        src,
        diag: Diag::Variant {
            label: (0, 0).into(),
        },
    };

    let mut output = String::new();
    GraphicalReportHandler::new()
        .with_theme(GraphicalTheme::unicode_nocolor())
        .render_report(&mut output, &err)
        .unwrap();
    println!("{output}");

    Err(err.into())
}
