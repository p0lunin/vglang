use crate::error_builder::ErrorMsgBuilder;
use crate::spanned::Span;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Error {
    Span(Span),
    NotDefined(Span, &'static str),
    Convert(Span, &'static str),
    DifferentTypes(Span, Span),
    NotHaveType(Span),
    VoidType(Span),
    Custom(Span, String, String),
}

impl Error {
    pub fn span(&self) -> Span {
        match self {
            Error::Span(s) => *s,
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
    pub fn display(&self, source: &str) -> String {
        match self {
            Error::Span(span) => ErrorMsgBuilder::default_one_span(
                *span,
                source,
                "Error",
                "Something wrong",
                "- here",
            )
            .build(),
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
            Error::DifferentTypes(span, t) => ErrorMsgBuilder::new(*span, source)
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
    dbg!(source.len());
    let span = Span::new(err.location.offset, err.location.offset + 1);
    let description = format!(
        "Expected one of {}, but found {:?}",
        err.expected,
        &source[err.location.offset..err.location.offset + 1]
    );
    ErrorMsgBuilder::default_one_span(span, source, "ParseError", &description, "- here").build()
}
