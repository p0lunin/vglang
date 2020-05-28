use clap::{App, Arg};
use fsmcreator::{parse_tokens, peg_error_to_showed, type_check_objects, parse_text};
use std::fs::File;
use std::io::Read;

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
    let ctx = match parse_tokens(ast) {
        Ok(t) => t,
        Err(errs) => {
            errs.into_iter()
                .for_each(|e| println!("{}\n", e.display(&data)));
            return;
        }
    };
    match type_check_objects(&ctx.objects, Some(&ctx)) {
        Err(e) => {
            println!("{}", e.display(&data));
            return;
        }
        Ok(()) => {}
    }
    println!("Types: ");
    ctx.objects.into_iter().for_each(|o| {
        println!("{}\n", o);
    })
}
