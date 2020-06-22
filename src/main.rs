use clap::{App, Arg};
use fsmcreator::compile_file;
use fsmcreator::interpreter::Interpreter;
use std::io;
use std::io::{stdin, Write};

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
        )
        .arg(
            Arg::with_name("std")
                .short("s")
                .long("std")
                .takes_value(true)
                .help("path to std"),
        );
    let matches = app.get_matches();
    let path_to_file = match matches.value_of("file") {
        Some(s) => s,
        None => {
            println!("Write file name");
            return;
        }
    };
    let path_to_std = match matches.value_of("std") {
        Some(s) => s,
        None => {
            println!("Write path to std");
            return;
        }
    };
    let (ctx, ir_ctx1) = match compile_file(path_to_std, None) {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    let (ctx, ir_ctx) = match compile_file(path_to_file, Some(&ctx)) {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
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
        io::stdout().flush().unwrap();
        let mut data = String::new();
        stdin().read_line(&mut data).expect("Error when read line");
        data.pop();
        match data.as_str() {
            "exit" => return,
            _ => println!("{}", f(data)),
        }
    }
}
