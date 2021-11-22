use std::{cell::Cell, rc::Rc};

pub struct Source {
    pub name: Rc<String>,
    pub content: Rc<String>,
    pub cursor: Cell<usize>,
}

impl<'a> Source {
    pub fn has_more(&self) -> bool {
        self.cursor.get() < self.content.len()
    }

    pub fn next_chunk(&self) -> &str {
        &self.content[self.cursor.get()..]
    }

    pub fn advance_by(&self, amount: usize) {
        let current = self.cursor.get();
        self.cursor.set(current + amount);
    }

    pub fn cursor(&self) -> usize {
        self.cursor.get()
    }
}
