use clap::{App, Arg};
use fsmcreator::interpreter::Interpreter;
use fsmcreator::{parse_text, parse_tokens, peg_error_to_showed, type_check_objects};
use std::fs::File;
use std::io;
use std::io::{stdin, Read, Write};

fn main() {
    let app = App::new("Tekstkvest")
        .version("Какая блять версия")
        .author("Anonymous student from college Server")
        .about("Engine for text kvests")
        .arg(
            Arg::with_name("file")
                .short("f")
                .long("file")
                .takes_value(true)
                .help("path to file with script"),
        );
    let matches = app.get_matches();
    let path_to_file = match matches.value_of("file") {
        Some(s) => s,
        None => {
            println!("Write file name");
            return;
        }
    };
    let mut file = match File::open(path_to_file) {
        Ok(f) => f,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let mut data = String::new();
    file.read_to_string(&mut data).unwrap();
    let ast = match parse_text(&data) {
        Ok(d) => d,
        Err(e) => {
            println!("{}", peg_error_to_showed(e, &data));
            return;
        }
    };
    let (ctx, mut ir_ctx) = match parse_tokens(ast) {
        Ok(t) => t,
        Err(errs) => {
            errs.into_iter()
                .for_each(|e| println!("{}\n", e.display(&data)));
            return;
        }
    };
    match type_check_objects(Some(&ctx), &mut ir_ctx) {
        Err(e) => {
            println!("{}", e.display(&data));
            return;
        }
        Ok(()) => {}
    }
    println!("Types: ");
    ctx.objects.iter().for_each(|o| {
        println!("{}\n", o);
    });

    let mut interpreter = Interpreter::from_ir_context(ctx, ir_ctx);

    repl(">>", |d| match interpreter.execute_code(&d) {
        Ok(b) => b.to_string(),
        Err(e) => e,
    })
}

fn repl(s: &str, mut f: impl FnMut(String) -> String) {
    loop {
        print!("{}", s);
        io::stdout().flush();
        let mut data = String::new();
        stdin().read_line(&mut data).expect("Error when read line");
        data.pop();
        match data.as_str() {
            "exit" => return,
            _ => println!("{}", f(data)),
        }
    }
}
