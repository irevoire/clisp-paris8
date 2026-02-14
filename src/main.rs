use clisp_formatter_paris8::parser::parse;

fn main() -> miette::Result<()> {
    for filename in std::env::args().skip(1) {
        let content = std::fs::read_to_string(&filename).unwrap();
        parse(&filename, &content)?;
    }

    Ok(())
}
