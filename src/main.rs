use clap::{App, Arg};
use fsmcreator::{compile_file, eval, Interpreter, load_core};
use std::io;
use std::io::{stdin, Write};
/*use z3::ast::Ast;

fn some() {
    let config = z3::Config::new();
    let ctx = z3::Context::new(&config);
    let solver = z3::Solver::new(&ctx);
    solver.push();
    solver.assert(&z3::ast::Int::from_u64(&ctx, 12).gt(&z3::ast::Int::from_u64(&ctx, 11)));
    let res = solver.check();
    println!("{:?}", res)
}*/

fn main() {
    //some();
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

    let (core_ctx, core_impls) = load_core();

    let ctx = {
        path_to_std.and_then(|path_to_std| {
            let ctx = match compile_file(path_to_std, Some(&core_ctx)) {
                Ok(ctx) => ctx,
                Err(e) => {
                    println!("{}", e);
                    return None;
                }
            };
            Some(ctx)
        })
    };
    let (ctx, mut impls) = match ctx {
        Some((c, mut impls)) => {
            impls.extend(core_impls);
            (c, impls)
        },
        None => (core_ctx, core_impls),
    };
    let (ctx, impls2) = match compile_file(path_to_file, Some(&ctx)) {
        Ok(ctx) => ctx,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };
    println!("Objects: ");
    ctx.objects.iter().for_each(|o| {
        println!("{}\n", o);
    });

    let mut f2 = impls2.functions;
    f2.append(&mut impls.functions);

    let mut interpreter = Interpreter::new(&f2, &ctx);

    repl(">>", |d| match eval(&mut interpreter, &d) {
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
