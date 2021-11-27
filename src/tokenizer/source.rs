use std::cell::Cell;

/// Representation of a tokenizable consumable source
pub struct Source {
    pub name: String,
    pub content: &'static str,
    cursor: Cell<usize>,
}

impl Source {
    /// Creates a new source with a cursor at the begining of the file
    pub fn new(name: String, content: &'static str) -> Self {
        Self {
            name,
            content,
            cursor: Cell::new(0),
        }
    }

    /// Checks if the cursor has reached the
    /// end of the source
    pub fn has_more_content(&self) -> bool {
        self.cursor.get() < self.content.len()
    }

    /// Returns the rest of the source file starting
    /// from the cursors offset
    pub fn content_from_cursor(&self) -> &'static str {
        &self.content[self.cursor()..]
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
