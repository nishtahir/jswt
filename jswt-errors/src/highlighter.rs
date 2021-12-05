use colored::{Color, Colorize};
use regex::Regex;

/// Highlighter that functions as a mini tokenizer.
/// Rather than return a token stream just apply the highlighting
/// in place.
struct HighlightRule {
    matcher: Regex,
    color: Color,
}

pub fn highlight(source: &str) -> String {
    let rules = [
        HighlightRule {
            // Keywords
            matcher: Regex::new(r"^(\bfunction|let|const)\b").unwrap(),
            color: Color::Cyan,
        },
        HighlightRule {
            matcher: Regex::new(r"^\d+").unwrap(),
            color: Color::Magenta,
        },
    ];

    let mut offset = 0;

    let mut highlighted_source = String::from("");
    'outer: while offset < source.len() {
        let rest = &source[offset..];

        for rule in rules.iter() {
            if let Some(res) = rule.matcher.find(rest) {
                let match_text = res.as_str();
                // Advance cursor based on match
                offset += match_text.len();
                highlighted_source += &match_text.color(rule.color).to_string();
                continue 'outer;
            }
        }

        highlighted_source += &source[offset..offset + 1].clear().to_string();
        offset += 1;
    }

    highlighted_source.to_owned()
    // Attempt to match the next token with a defined lexer rule
}
