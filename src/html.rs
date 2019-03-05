use crate::{Html, Model, Str};
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum Attribute {
    Value(Str),
    Href(Str),
    Placeholder(Str),
    Class(Vec<Str>),
    Id(Str),
    Style(Style),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ClosureId(u64);

pub enum Event<M: Model> {
    OnClick {
        id: ClosureId,
        cb: Rc<Fn() -> M::Msg>,
    },
    OnInput {
        id: ClosureId,
        cb: Rc<Fn(String) -> M::Msg>,
    },
}

impl<M: Model> Event<M> {
    fn id(&self) -> ClosureId {
        match self {
            Event::OnClick { id, .. } => *id,
            Event::OnInput { id, .. } => *id,
        }
    }
}

impl<M: Model> fmt::Debug for Event<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Event::OnClick { id, .. } => write!(f, "OnClickEvent({:?})", id),
            Event::OnInput { id, .. } => write!(f, "OnInputEvent({:?})", id),
        }
    }
}

impl<M: Model> PartialEq for Event<M> {
    fn eq(&self, other: &Event<M>) -> bool {
        self.id() == other.id()
    }
}

fn hash_closure<S: Hash, F: Hash>(s: S, f: F) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    f.hash(&mut hasher);
    hasher.finish()
}

pub fn on_click<M: Model, S: Hash + 'static>(s: S, f: fn(s: &S) -> M::Msg) -> Event<M> {
    // Can't hash fn ptr - compiler bug! Do very unsafe workaround
    // https://github.com/rust-lang/rust/issues/46989
    let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
    let hash = hash_closure(&s, ptr);
    Event::OnClick {
        id: ClosureId(hash),
        cb: Rc::new(move || f(&s)),
    }
}

pub fn on_input<M: Model, S: Hash + 'static>(s: S, f: fn(&S, String) -> M::Msg) -> Event<M> {
    // Can't hash fn ptr - compiler bug! Do very unsafe workaround
    // https://github.com/rust-lang/rust/issues/46989
    let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
    let hash = hash_closure(&s, ptr);
    Event::OnInput {
        id: ClosureId(hash),
        cb: Rc::new(move |val| f(&s, val)),
    }
}

macro_rules! attr_key_value {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name(val: impl Into<Str>) -> Attribute {
            Attribute::$tag(val.into())
        }
    };
}

attr_key_value!(id, Id);
attr_key_value!(value, Value);
attr_key_value!(placeholder, Placeholder);
attr_key_value!(href, Href);

pub fn class(c: impl Classify) -> Attribute {
    Attribute::Class(c.classify())
}

#[derive(Debug, Clone, PartialEq)]
pub struct Style;

pub trait Classify {
    fn classify(self) -> Vec<Str>;
}

impl<S: Into<Str>> Classify for S {
    fn classify(self) -> Vec<Str> {
        vec![self.into()]
    }
}

pub trait ElemMod<M: Model> {
    fn modify_element(self, elem: &mut Html<M>);
}

impl<M: Model> ElemMod<M> for &'static str {
    fn modify_element(self, elem: &mut Html<M>) {
        if elem.text.is_some() {
            let mut s: String = elem.text.take().unwrap().into_owned();
            s.push_str(&self);
            elem.text = Some(s.into())
        } else {
            elem.text = Some(self.into())
        }
    }
}

// TODO exact duplicate of above :/
impl<M: Model> ElemMod<M> for String {
    fn modify_element(self, elem: &mut Html<M>) {
        if elem.text.is_some() {
            let mut s: String = elem.text.take().unwrap().into_owned();
            s.push_str(&self);
            elem.text = Some(s.into())
        } else {
            elem.text = Some(self.into())
        }
    }
}

impl<M: Model, E: ElemMod<M>> ElemMod<M> for Vec<E> {
    fn modify_element(self, elem: &mut Html<M>) {
        for modifier in self {
            modifier.modify_element(elem)
        }
    }
}

impl<M: Model> ElemMod<M> for Attribute {
    fn modify_element(self, elem: &mut Html<M>) {
        elem.attrs.push(self)
    }
}

impl<M: Model> ElemMod<M> for Event<M> {
    fn modify_element(self, elem: &mut Html<M>) {
        elem.events.push(self)
    }
}

impl<M: Model> ElemMod<M> for Html<M> {
    fn modify_element(self, elem: &mut Html<M>) {
        elem.children.push(self)
    }
}
