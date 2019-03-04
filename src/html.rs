use std::fmt;
use std::rc::Rc;

use crate::{Html, Model, Str, Listener};

#[derive(Debug, PartialEq)]
pub enum Attribute {
    Value(Str),
    Placeholder(Str),
    Class(Vec<Str>),
    Id(Str),
    Style(Style),
}

#[derive(Clone, Hash)]
pub enum Event<M: Model> {
    OnClick(fn() -> M::Msg),
    OnInput(fn(String) -> M::Msg),
}

impl<M: Model> PartialEq for Event<M> {
    fn eq(&self, other: &Self) -> bool {
        use Event::*;
        match (self, other) {
            (OnClick(f1), OnClick(f2)) => f1 == f2,
            (OnInput(f1), OnInput(f2)) => f1 == f2,
            _ => false
        }
    }
}

impl<M: Model> fmt::Debug for Event<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Event::OnClick(ptr) => write!(f, "Event(OnClick({:p})", ptr),
            Event::OnInput(ptr) => write!(f, "Event(OnInput({:p})", ptr),
        }
    }
}


// impl PartialEq for Attribute {
//     fn eq(&self, other: &Self) -> bool {
//         use Attribute::*;
//         match (self, other) {
//             (Value(l), Value(r)) => { crate::log!("{} == {}?", l, r);l == r}
//             (Placeholder(l), Placeholder(r)) => l == r,
//             (Class(l), Class(r)) => l == r,
//             (Id(l), Id(r)) => l == r,
//             (Style(l), Style(r)) => l == r,
//             _different => false
//         }
//     }
// }

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

pub fn on_click<M: Model>(f: fn() -> M::Msg) -> Event<M> {
    Event::OnClick(f)
}

pub fn on_input<M: Model>(f: fn(String) -> M::Msg) -> Event<M> {
    Event::OnInput(f)
}

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
