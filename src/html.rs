use closures::Closure;
use std::any::{Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::{log, Html, Listener, Model, Str};

#[derive(Debug, PartialEq)]
pub enum Attribute {
    Value(Str),
    Placeholder(Str),
    Class(Vec<Str>),
    Id(Str),
    Style(Style),
}

pub enum Event<M: Model> {
    OnClick(Rc<dyn Fn() -> M::Msg>),
    OnInput(Rc<dyn Fn(String) -> M::Msg>),
    Unchanged,
}

thread_local! {
    static EVENT_MAP: RefCell<HashMap<u32, Box<dyn Any>>> = RefCell::new(HashMap::new());
}

impl<M: Model> fmt::Debug for Event<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Event::OnClick(ptr) => write!(f, "Event(OnClick({:p})", ptr),
            Event::OnInput(ptr) => write!(f, "Event(OnInput({:p})", ptr),
            Event::Unchanged => write!(f, "Event(Unchanged)"),
        }
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

#[macro_export]
macro_rules! on_click {
    ($state:expr, $closure:expr) => {
        $crate::html::__on_click($state, $closure, line!())
    };
    ($closure:expr) => {
        $crate::html::__on_click((), $closure, line!())
    };
}

#[doc(hidden)]
pub fn __on_click<S: PartialEq + Clone + 'static, M: Model>(
    state: S,
    f: fn(&S) -> M::Msg,
    callsite_id: u32,
) -> Event<M> {
    log!("On-Click closure");
    let cl = Closure::new(state, f);
    EVENT_MAP.with(|map| {
        log!("In map");
        let mut map = map.borrow_mut();
        let previous: Option<&Box<Any>> = map.get(&callsite_id);
        if previous.is_some() {
            log!("previous found")
        } else {
            log!("previous not found")
        };
        if let Some(prev) = previous.and_then(|any| any.downcast_ref::<Rc<Closure<_, _>>>()) {
            if &cl == prev.as_ref() {
                // Short-circuit the happy path
                log!("On-Click closure unchanged");
                return Event::Unchanged;
            }
        };
        let cl = Rc::new(cl);
        map.insert(callsite_id, Box::new(cl.clone()) as Box<dyn Any>);
        Event::OnClick(cl)
    })
}

pub fn on_input<M: Model>(f: impl Fn(String) -> M::Msg + 'static) -> Event<M> {
    unimplemented!()
    // Event::OnInput(Box::new(f))
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
