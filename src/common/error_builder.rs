use crate::common::Span;

fn multiply_str(str: &str, count: usize) -> String {
    let mut string = String::with_capacity(count * str.len());
    for _ in 0..count {
        string.push_str(str);
    }
    string
}
fn line_and_offset(data: &str, line_num: usize) -> (&str, usize) {
    let mut offset = 0;
    for (i, line) in data.split("\n").enumerate() {
        if i == line_num {
            return (line, offset);
        } else {
            offset += line.len() + 1;
        }
    }
    unreachable!();
}

#[derive(Debug)]
pub struct ErrorMsgBuilder<'a> {
    span: Span,
    source: &'a str,
    data: String,
    err_line_num: usize,
    err_line_num_length: usize,
    offset_before_error: usize,
    error_line: &'a str,
    last_added_size: usize,
    error_size: usize,
}
impl<'a> ErrorMsgBuilder<'a> {
    pub fn default_one_span(
        span: Span,
        source: &'a str,
        kind: &str,
        description: &str,
        annotate: &str,
    ) -> Self {
        Self::new(span, source)
            .error_kind(kind)
            .description(description)
            .new_line()
            .offset_before_error()
            .new_line()
            .error_line()
            .new_line()
            .offset_before_error()
            .fill_str_error_size_times("^")
            .add(annotate)
    }
    pub fn new(span: Span, source: &'a str) -> Self {
        let err_line_num = source
            .char_indices()
            .filter(|(i, c)| *c == '\n' && *i < span.start)
            .count()
            + 1;
        let err_line_num_length = err_line_num.to_string().len();
        let (error_line, offset_before_line) = line_and_offset(source, err_line_num - 1);
        let offset_before_error = span.start - offset_before_line;

        Self {
            span,
            source,
            data: String::new(),
            err_line_num,
            err_line_num_length,
            offset_before_error,
            error_line,
            last_added_size: 0,
            error_size: span.end - span.start,
        }
    }
    pub fn error_kind(self, msg: &str) -> Self {
        self.add(format!("[{}]: ", msg).as_str())
    }
    pub fn description(self, desc: &str) -> Self {
        self.add(desc)
    }
    pub fn error_line(self) -> Self {
        let data = format!("{} | {}", self.err_line_num, self.error_line);
        self.add(data.as_str())
    }
    pub fn new_line(self) -> Self {
        self.add("\n")
    }
    pub fn fill_str(self, str: &str, count: usize) -> Self {
        self.add(multiply_str(str, count).as_str())
    }
    pub fn fill_str_error_size_times(self, str: &str) -> Self {
        let data = multiply_str(str, self.error_size);
        self.add(data.as_str())
    }
    pub fn offset_before_error(self) -> Self {
        let offset_before_error = self.offset_before_error;
        let err_line_num_length = self.err_line_num_length;
        self.fill_str(" ", err_line_num_length)
            .add(" | ")
            .fill_str(" ", offset_before_error)
    }

    pub fn build(self) -> String {
        self.data
    }

    pub fn add(mut self, str: &str) -> Self {
        self.data += str;
        self.last_added_size = str.len();
        self
    }
}
