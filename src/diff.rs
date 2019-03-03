use crate::{Model, Html};
use web_sys::{Document, Element};
use std::ops::Deref;

struct Differ<'a, M: Model> {
    path: Vec<Element>,
    buffered_pop: Vec<u32>,
    doc: &'a Document,
    marker: std::marker::PhantomData<M>,
}

impl<'a, M: Model> Differ<'a, M> {
    fn current(&self) -> &Element {
        self.path.last().unwrap()
    }
}


impl<'a, M: Model> treediff::Delegate<'a, u32, Html<M>> for Differ<'a, M> {
    fn push(&mut self, key: &u32) {
        let next = self.current().children().get_with_index(*key).unwrap();
        self.path.push(next);
    }

    fn pop(&mut self) {
        let elem = self.path.pop().unwrap();
        for ix in self.buffered_pop.drain(0..).rev() {
            elem.children().get_with_index(ix).unwrap().remove()
        }
    }

    fn removed(&mut self, k: &u32, _: &Html<M>) {
        self.buffered_pop.push(*k);
    }

    fn added(&mut self, k: &u32, new_node: &Html<M>) {
        let new_elem = new_node.render_to_dom(self.doc).expect("Failed to render");
        let c = self.current();
        let child = c.children().get_with_index(*k);
        c.insert_before(&new_elem, child.as_ref().map(|ch| ch.deref())).expect("failed to insert");
    }
}
