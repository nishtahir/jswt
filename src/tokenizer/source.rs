use std::cell::Cell;

pub struct Source {
    pub name: String,
    pub content: &'static str,
    cursor: Cell<usize>,
}

impl Source {
    pub fn new(name: String, content: &'static str) -> Self {
        Self {
            name,
            content,
            cursor: Cell::new(0),
        }
    }

    pub fn has_more_content(&self) -> bool {
        self.cursor.get() < self.content.len()
    }

    pub fn content_from_cursor(&self) -> &'static str {
        &self.content[self.cursor.get()..]
    }

    pub fn advance_cursor(&self, amount: usize) {
        self.cursor.set(self.cursor() + amount);
    }

    pub fn cursor(&self) -> usize {
        self.cursor.get()
    }
}
