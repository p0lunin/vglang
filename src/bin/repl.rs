use clap::{App, Arg};
use fsmcreator::{compile_file, eval, load_core, DisplayScope, GlobalCtx, Interpreter, ScopeCtx};
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
    let path_to_std = matches.value_of("std");

    let mut global = GlobalCtx::new();

    let _core_ctx = load_core(&mut global);

    let _std_ctx = {
        path_to_std.and_then(|path_to_std| {
            let ctx = match compile_file(path_to_std, &mut global) {
                Ok(ctx) => ctx,
                Err(e) => {
                    println!("{}", e);
                    return None;
                }
            };
            Some(ctx)
        })
    };
    let _file_ctx = match compile_file(path_to_file, &mut global) {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    println!("Objects: ");
    global.iter_funcs().for_each(|(f, ctx)| {
        println!("{}\n", f.display_value_string(&global));
    });

    repl(">> ", |d| {
        let mut ctx = ScopeCtx::new(&global);
        let mut interpreter = Interpreter::new(&mut ctx);
        match eval(&mut interpreter, &d) {
            Ok(b) => b.display_value_string(&ctx),
            Err(e) => e,
        }
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
