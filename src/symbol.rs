use crate::node::Node;

#[derive(Debug, PartialEq)]
pub struct Symbol<'a> {
    pub name: Node<'a>,
}
