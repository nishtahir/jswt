use std::collections::HashMap;

use colored::Colorize;

use crate::{
    codeframe::{code_frame, location_from_offset, LineCol, Location},
    highlighter::highlight,
    DiagnosticMessage, Level,
};

pub struct ErrorEmitter<'a> {
    source_map: &'a HashMap<String, &'static str>,
}

impl<'a> ErrorEmitter<'a> {
    ///
    pub fn new(source_map: &'a HashMap<String, &'static str>) -> Self {
        Self { source_map }
    }

    pub fn emit(&self, diagnostics: &[DiagnosticMessage]) {
        for diagnostic in diagnostics {
            let DiagnosticMessage {
                level,
                message,
                span,
                hint,
            } = diagnostic;

            let file = &span.file.to_string();
            let source = self.source_map[file];
            let location = Location {
                end: location_from_offset(source, span.end),
                start: location_from_offset(source, span.start),
            };
            let code_frame = create_codeframe(source, &location, message);
            let header = create_header(file, &location.start, level);
            println!("{}\n{}", header, code_frame);
            if let Some(hint) = hint {
                let text = format!("Hint: {}", &hint).bold();
                println!("{}", text);
            }
            // Blank line for better output
            println!("\n");
        }
    }
}

fn create_header(file: &str, location: &LineCol, level: &Level) -> String {
    let err = match level {
        Level::Error => "error".bright_red().bold(),
        Level::Warning => "warning".yellow().bold(),
    };

    format!("{}: {}:{}:{}", err, file, location.line, location.col)
}

fn create_codeframe(source: &str, location: &Location, message: &str) -> String {
    code_frame(&highlight(source, false), location, message)
}
