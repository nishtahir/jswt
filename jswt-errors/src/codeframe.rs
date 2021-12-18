use regex::Regex;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]

pub struct LineCol {
    pub line: i32,
    pub col: i32,
}

pub struct Location {
    pub start: LineCol,
    pub end: LineCol,
}

struct MarkerLines {
    start: usize,
    end: usize,
    markers: HashMap<usize, (i32, i32)>,
}

fn marker_lines(source: &[&str], location: &Location) -> MarkerLines {
    let lines_above = 2;
    let lines_below = 3;

    let start_line = location.start.line;
    let start_col = location.start.col;
    let end_line = location.end.line;
    let end_col = location.end.col;

    let start = (start_line - (lines_above + 1)).max(0);
    let end = ((source.len() as i32).min(end_line + lines_below)).max(0);
    let line_diff = end_line - start_line;

    let mut markers: HashMap<usize, (i32, i32)> = HashMap::new();
    if line_diff > 0 {
        for i in 0..=line_diff {
            let line_number = i + start_line;
            if start_col < 1 {
                // markerLines[lineNumber] = true;
                todo!()
            } else if i == 0 {
                let source_len = source[(line_number - 1) as usize].len();
                markers.insert(
                    line_number as usize,
                    (start_col, (source_len as i32) - start_col + 1),
                );
            } else if i == line_diff {
                markers.insert(line_number as usize, (0, end_col));
            } else {
                let source_len = source[(line_number - i) as usize].len();
                markers.insert(line_number as usize, (0, source_len as i32));
            }
        }
    } else if start_col == end_col {
        if start_col > 0 {
            markers.insert(start_line as usize, (start_col, 0));
        } else {
            // markers[start_line] = true;
            todo!()
        }
    } else {
        markers.insert(start_line as usize, (start_col, end_col - start_col));
    }

    MarkerLines {
        start: start as usize,
        end: end as usize,
        markers,
    }
}

/// Incomplete port of babel/code
/// https://github.com/babel/babel/blob/main/packages/babel
//
/// Generates error messages with code appropriate context
///   1 | function a(b, c) {
/// > 2 |   return b + c;
///     |   ^^^^^^^^^^^^^
///   3 | }
///
/// TODO: Port over floating message support

pub fn code_frame(source: &str, location: &Location, message: &str) -> String {
    let lines = source.split('\n').collect::<Vec<&str>>();
    let MarkerLines {
        start,
        end,
        markers,
    } = marker_lines(&lines, location);

    let number_max_width = ((end as f32).log10()) as usize + 1;
    let context = &lines[start..end];

    context
        .iter()
        .enumerate()
        .map(|(index, line)| {
            let line_number = start + index + 1;
            let gutter = format!(" {: >width$} |", line_number, width = number_max_width);
            let is_last_line = markers.get(&(line_number + 1)).is_none();

            if let Some((start_col, end_col)) = markers.get(&line_number) {
                let marker_spacing = " ".repeat(0_i32.max(start_col - 1) as usize);
                let number_of_markers = *end_col as usize;

                let rg = Regex::new(r"\d").unwrap();
                let mut marker_line = format!(
                    "\n {} {}{}",
                    rg.replace_all(&gutter, " "),
                    marker_spacing,
                    "^".repeat(number_of_markers)
                );

                if is_last_line {
                    marker_line += " ";
                    marker_line += message;
                }
                format!(">{} {}{}", gutter, line, marker_line)
            } else {
                format!(" {} {}", gutter, line)
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}

/// Derive a Location from a given offset
pub fn location_from_offset(source: &str, offset: usize) -> LineCol {
    let mut location = LineCol { line: 1, col: 1 };
    if offset > 0 {
        let mut remaining = offset;
        for ch in source.chars() {
            if ch == '\n' {
                location.line += 1;
                location.col = 1;
            } else {
                location.col += 1;
            }

            remaining -= 1;
            if remaining == 0 {
                break;
            }
        }
    }

    location
}

#[cfg(test)]
mod test {
    use jswt_assert::assert_debug_snapshot;

    use super::*;

    #[test]
    fn test_marker_lines() {
        let raw_source = "function a(b, c) {\n  return b + c;\n}";
        let lines: Vec<&str> = raw_source.split("\n").collect();

        let err_location = Location {
            start: LineCol { line: 1, col: 1 },
            end: LineCol { line: 3, col: 1 },
        };

        let marker_lines = marker_lines(&lines, &err_location);
        assert_eq!(marker_lines.start, 0);
        assert_eq!(marker_lines.end, 3);

        let mut expected_markers = HashMap::new();
        expected_markers.insert(1, (1, 18));
        expected_markers.insert(2, (0, 15));
        expected_markers.insert(3, (0, 1));

        assert_eq!(expected_markers, marker_lines.markers);
    }

    #[test]
    fn test_find_offset_row_col() {
        let raw_source = "function a(b, c) {\n  return b + c;\n}";

        let actual = location_from_offset(raw_source, 10);

        let expected = LineCol { line: 1, col: 11 };
        assert_eq!(expected, actual);

        let actual = location_from_offset(raw_source, 21);

        let expected = LineCol { line: 2, col: 3 };
        assert_eq!(expected, actual);

        let actual = location_from_offset(raw_source, 36);

        let expected = LineCol { line: 3, col: 2 };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_find_offset_row_col_with_offset_exceeds_len_returns_last_location() {
        let raw_source = "function a(b, c) {\n  return b + c;\n}";

        let actual = location_from_offset(raw_source, 100);

        let expected = LineCol { line: 3, col: 2 };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_find_offset_row_col_with_offset_0_returns_first_location() {
        let raw_source = "function a(b, c) {\n  return b + c;\n}";

        let actual = location_from_offset(raw_source, 0);

        let expected = LineCol { line: 1, col: 1 };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_code_frame() {
        let err_location = Location {
            start: LineCol { line: 2, col: 3 },
            end: LineCol { line: 2, col: 16 },
        };

        let raw_source = "function a(b, c) {\n  return b + c;\n}";

        let actual = code_frame(raw_source, &err_location, "test message");
        assert_debug_snapshot!(actual);
    }
}
