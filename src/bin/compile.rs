use clap::{App, Arg};
use fsmcreator::{compile_file, eval, Interpreter, load_core, GlobalCtx, ScopeCtx, DisplayScope};
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
    global.iter_funcs().for_each(|(x, _)| {
        println!("{}", x.display_value_string(&global))
    });
}
