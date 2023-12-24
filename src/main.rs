use std::io::Read;

mod lexer;
mod ast;
mod parser;
mod types;
mod compiler;

fn get_line_col(source: &str, index: usize) -> (usize, usize) {
    let mut start_of_line = 0;
    let mut line = 1;
    let mut column = 1;
    for (i, c) in source.chars().enumerate() {
        if i >= index { 
            column = i - start_of_line + 1;
            break;
        }
        if c == '\n' {
            line += 1;
            start_of_line = i + 1;
        } else if c == '\r' {
            start_of_line = i + 1;
        }
    }
    (line, column)
}

fn show_error<T>(err: T, source: &str, span: Option<lexer::Span>) -> !
    where T: std::fmt::Debug {
    if let Some(span) = span {
        let (line, column) = get_line_col(source, span.lo);
        eprintln!("Error at {}:{} - {:?}", line, column, err);
    } else {
        eprintln!("Error - {:?}", err);
    }
    std::process::exit(1);
}

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("usage: {} <filename>", args[0]);
        return;
    }

    let mut file = std::fs::File::open(&args[1]).expect("Could not open file");
    let mut buf = String::new();
    file.read_to_string(&mut buf).expect("Could not read file");

    let mut parsed_ast = match parser::parse(lexer::Lexer::new(&buf)) {
        Ok(result) => result, 
        Err(err) => show_error(&err, &buf, err.0.as_ref().map(|x| x.1)),
    };

    println!("{}", parsed_ast);
    let mut elab = types::Elaborator::new();
    let ty = match elab.elaborate_expr(&mut parsed_ast) {
        Err(err) => show_error(&err.tag, &buf, Some(err.span)),
        Ok(r) => r
    };

    println!("{:?}", ty);

    let file = std::fs::File::create(format!("{}.lua", args[1])).expect("Could not create file");

    let buffered_writer = std::io::BufWriter::new(file);
    let mut compiler = compiler::Compiler::new(Box::new(buffered_writer));
    compiler.compile_expr(&parsed_ast);

}
