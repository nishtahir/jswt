use std::iter::Peekable;

/// Iterator which keeps track of line and column position of its content
pub struct LineColumnIterator<I>
where
    I: Iterator,
{
    iter: Peekable<I>,
    line: usize,
    col: usize,
    offset: usize,
}

impl<I> LineColumnIterator<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(iter: I) -> LineColumnIterator<I> {
        LineColumnIterator {
            iter: iter.peekable(),
            line: 1,
            col: 0,
            offset: 0,
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn col(&self) -> usize {
        self.col
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

impl<I> Iterator for LineColumnIterator<I>
where
    I: Iterator<Item = char>,
{
    type Item = char;

    fn next(&mut self) -> Option<char> {
        match self.iter.next() {
            None => None,
            Some('\n') => {
                self.offset += 1;
                self.line += 1;
                self.col = 0;
                Some('\n')
            }
            Some(c) => {
                self.offset += 1;
                self.col += 1;
                Some(c)
            }
        }
    }
}
