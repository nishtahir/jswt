use jswt_common::fs;
use std::{borrow::Cow, cell::Cell};

/// Representation of a tokenizable consumable source
pub struct Source {
    pub path: Cow<'static, str>,
    pub module: Cow<'static, str>,
    cursor: Cell<usize>,
}

impl Source {
    /// Creates a new source with a cursor at the begining of the file
    pub fn new(path: Cow<'static, str>, module: Cow<'static, str>) -> Self {
        Self {
            path,
            module,
            cursor: Cell::new(0),
        }
    }

    /// Checks if the cursor has reached the
    /// end of the source
    pub fn has_more_content(&self) -> bool {
        let content = fs::read_to_string(&self.path);
        self.cursor.get() < content.len()
    }

    /// Returns the rest of the source file starting
    /// from the cursors offset
    pub fn content_from_cursor(&self) -> &str {
        let content = fs::read_to_string(&self.path);
        &content[self.cursor()..]
    }

    /// Advances the cursor by the given amount
    pub fn advance_cursor(&self, amount: usize) {
        self.cursor.set(self.cursor() + amount);
    }

    /// Current cursor offset from the start
    /// of the source
    pub fn cursor(&self) -> usize {
        self.cursor.get()
    }
}
