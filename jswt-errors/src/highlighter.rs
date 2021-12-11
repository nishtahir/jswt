use colored::{Color, Colorize};
use regex::Regex;

/// Highlighter that functions as a mini tokenizer.
/// Rather than return a token stream just apply the highlighting
/// in place.
struct HighlightRule {
    matcher: Regex,
    color: Color,
}

pub fn highlight(source: &str, force: bool) -> String {
    if force {
        colored::control::set_override(true)
    }
    let rules = [
        HighlightRule {
            // Comments
            matcher: Regex::new(r"^((//[^\n]*)|((?s)^/\*.*?\*/))").unwrap(),
            // Gray
            color: Color::BrightBlack,
        },
        HighlightRule {
            // Keywords
            matcher: Regex::new(r"^(\bexport|import|function|let|const|return)\b").unwrap(),
            color: Color::Cyan,
        },
        HighlightRule {
            // built-ins
            matcher: Regex::new(r"^\b(i32|u32|f32|string|boolean)\b").unwrap(),
            color: Color::Yellow,
        },
        HighlightRule {
            matcher: Regex::new(r"^\d+").unwrap(),
            color: Color::Magenta,
        },
        HighlightRule {
            // Ident
            matcher: Regex::new(r"^\b[a-z_][A-Za-z0-9]*\b").unwrap(),
            color: Color::White,
        },
        HighlightRule {
            // Class?
            matcher: Regex::new(r"^\b[A-Za-z_][A-Za-z0-9]*\b").unwrap(),
            color: Color::Yellow,
        },
        HighlightRule {
            matcher: Regex::new(r"^\d+").unwrap(),
            color: Color::Magenta,
        },
        HighlightRule {
            matcher: Regex::new(r#"^"[^"]*""#).unwrap(),
            color: Color::Green,
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
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_snapshot;

    #[test]
    fn test_highlight_source() {
        let raw_source = r#"
        // @ts-nocheck

        // Single max utility function
        function max(a: i32, b: i32): i32 {
            if (a > b) {
                return a;
            }
            return b;
        }
        
        function min(a: i32, b: i32): i32 {
            if (a < b) {
                return a;
            }
            return b;
        }
        
        @inline
        @wast("(i32.lt_u (local.get $a) (local.get $b))")
        function lessUnsigned(a: i32, b: i32): bool { }
        
        @inline
        @wast("(i32.ge_u (local.get $a) (local.get $b))")
        function greaterEqUnsigned(a: i32, b: i32): bool { }
        
        @inline
        @wast("(i32.gt_u (local.get $a) (local.get $b))")
        function greaterUnsigned(a: i32, b: i32): bool { }
        
        @inline
        @wast("(i32.div_u (local.get $a) (local.get $b))")
        function divUnsigned(a: i32, b: i32): bool { }
        "#;

        let actual = highlight(raw_source, true);
        assert_snapshot!(actual);
    }
}
