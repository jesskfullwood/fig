use std::any::Any;
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

macro_rules! closure_handler {
    ($func_name:ident, $macro_name:ident, $functy:ty, $closurety:ty, $event:ident) => {
        #[doc(hidden)]
        pub fn $func_name<M: Model, S: PartialEq + Clone + 'static>(
            state: S,
            f: $functy,
            callsite_id: u32,
        ) -> Event<M> {
            let cl = <$closurety>::new(state, f);
            EVENT_MAP.with(|map| {
                let mut map = map.borrow_mut();
                let previous: Option<&Box<Any>> = map.get(&callsite_id);
                if let Some(prev) = previous.and_then(|any| any.downcast_ref::<Rc<$closurety>>()) {
                    if &cl == prev.as_ref() {
                        // Short-circuit the happy path
                        return Event::Unchanged;
                    }
                };
                log!("Closure @{} **changed**", callsite_id);
                let cl = Rc::new(cl);
                map.insert(callsite_id, Box::new(cl.clone()) as Box<dyn Any>);
                Event::$event(cl)
            })
        }

        #[macro_export]
        macro_rules! $macro_name {
            ($state:expr, $closure2:expr) => {
                // XXX using line! is a total hack and will break in mysterious ways
                // e.g. if two invocations in different files are on the same line count.
                // How else to get unique callsite id?
                $crate::html::$func_name($state, $closure2, line!())
            };
            ($closure2:expr) => {
                $crate::html::$func_name((), $closure2, line!())
            };
        }
    };
}

closure_handler!(
    __on_click,
    on_click,
    fn(&S) -> M::Msg,
    closures::Closure<_, _>,
    OnClick
);
closure_handler!(
    __on_input,
    on_input,
    fn(&S, String) -> M::Msg,
    closures::Closure1<_, _, _>,
    OnInput
);

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
