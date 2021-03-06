use crate::common::error_builder::ErrorMsgBuilder;
use crate::common::Span;

pub trait SpannedError<T> {
    fn spanned_err(self, span: Span) -> Result<T, Error>;
}

impl<T> SpannedError<T> for Result<T, String> {
    fn spanned_err(self, span: Span) -> Result<T, Error> {
        self.map_err(|e| Error::Custom(span, e, "-here".to_owned()))
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    NotDefined(Span, &'static str),
    Convert(Span, &'static str),
    DifferentTypes(Span, Span),
    NotHaveType(Span),
    VoidType(Span),
    Custom(Span, String, String),
}

impl Error {
    pub fn custom<T: Into<String>>(span: Span, desc: T) -> Self {
        Self::Custom(span, desc.into(), "-here".to_string())
    }
    pub fn span(&self) -> Span {
        match self {
            Error::NotDefined(s, _) => *s,
            Error::Convert(s, _) => *s,
            Error::DifferentTypes(s, _) => *s,
            Error::NotHaveType(s) => *s,
            Error::VoidType(s) => *s,
            Error::Custom(s, _, _) => *s,
        }
    }
}

impl Error {
    pub fn cannot_infer_type(span: Span) -> Self {
        Error::Custom(span, format!("Cannot infer type!"), "-here".to_string())
    }
    pub fn display(&self, source: &str) -> String {
        match self {
            Error::NotDefined(span, t) => ErrorMsgBuilder::new(*span, source)
                .error_kind("Error")
                .description(&format!("Not defined {}", t))
                .new_line()
                .error_line()
                .new_line()
                .offset_before_error()
                .fill_str_error_size_times("^")
                .add(&format!("- here"))
                .build(),
            Error::Convert(span, t) => ErrorMsgBuilder::new(*span, source)
                .error_kind("Error")
                .description(&format!("Convert error {}", t))
                .new_line()
                .error_line()
                .new_line()
                .offset_before_error()
                .fill_str_error_size_times("^")
                .add(&format!("- here"))
                .build(),
            Error::DifferentTypes(span, _) => ErrorMsgBuilder::new(*span, source)
                .error_kind("Error")
                .description(&format!("Different types"))
                .new_line()
                .error_line()
                .new_line()
                .offset_before_error()
                .fill_str_error_size_times("^")
                .add(&format!("- here"))
                .build(),
            Error::NotHaveType(span) => ErrorMsgBuilder::new(*span, source)
                .error_kind("Error")
                .description(&format!("Not have type"))
                .new_line()
                .error_line()
                .new_line()
                .offset_before_error()
                .fill_str_error_size_times("^")
                .add(&format!("- here"))
                .build(),
            Error::VoidType(span) => ErrorMsgBuilder::new(*span, source)
                .error_kind("Error")
                .description(&format!("Void type"))
                .new_line()
                .error_line()
                .new_line()
                .offset_before_error()
                .fill_str_error_size_times("^")
                .add(&format!("- here"))
                .build(),
            Error::Custom(span, desc, ann) => {
                ErrorMsgBuilder::default_one_span(*span, source, "Error", &desc, &ann).build()
            }
        }
    }
}

pub fn peg_error_to_showed(err: peg::error::ParseError<peg::str::LineCol>, source: &str) -> String {
    let span = Span::new(err.location.offset, err.location.offset + 1);
    let found = match err.location.offset + 1 > source.len() {
        true => "EOF".to_string(),
        false => source[err.location.offset..]
            .chars()
            .next()
            .unwrap()
            .to_string(),
    };
    let description = format!("Expected one of {}, but found {:?}", err.expected, found);
    ErrorMsgBuilder::default_one_span(span, source, "ParseError", &description, "- here").build()
}
