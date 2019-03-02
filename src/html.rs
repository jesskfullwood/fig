use std::rc::Rc;
use std::fmt;

use crate::{Model, Str, Html};

pub enum Attribute<M: Model> {
    Value(Str),
    Placeholder(Str),
    Class(Vec<Str>),
    Id(Str),
    Style(Style),
    OnClick(Rc<dyn Fn() -> M::Msg>),
    OnInput(Rc<Fn(String) -> M::Msg>),
}

impl<M: Model> fmt::Debug for Attribute<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attribute")
    }
}

macro_rules! attr_key_value {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model>(val: impl Into<Str>) -> Attribute<M> {
            Attribute::$tag(val.into())
        }
    };
}

attr_key_value!(id, Id);
attr_key_value!(value, Value);
attr_key_value!(placeholder, Placeholder);

pub fn on_click<M: Model>(f: impl Fn() -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnClick(Rc::new(f))
}

pub fn on_input<M: Model>(f: impl Fn(String) -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnInput(Rc::new(f))
}


pub fn class<M: Model>(c: impl Classify) -> Attribute<M> {
    Attribute::Class(c.classify())
}

pub struct Style;

pub trait ElemMod<M: Model> {
    fn modify_element(self, elem: &mut Html<M>);
}

pub trait Classify {
    fn classify(self) -> Vec<Str>;
}

impl<S: Into<Str>> Classify for S {
    fn classify(self) -> Vec<Str> {
        vec![self.into()]
    }
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

impl<M: Model> ElemMod<M> for Attribute<M> {
    fn modify_element(self, elem: &mut Html<M>) {
        elem.attrs.push(self)
    }
}

impl<M: Model> ElemMod<M> for Html<M> {
    fn modify_element(self, elem: &mut Html<M>) {
        elem.children.push(self)
    }
}

macro_rules! make_dsl_macros {
    ($d:tt $($name:ident : $tag:ident),*) => {
        $(
            #[macro_export]
            macro_rules! $name {
                ($d($modifier:expr),* $d(,)?) => {
                    {
                        use $crate::{Html, Tag, html::ElemMod};
                        let mut html = Html::tag(Tag::$tag);
                        $d($modifier.modify_element(&mut html);)*;
                        html
                    }
                }
            }
        )*
    }
}

// Pass in the '$' symbol - workaround for macro_rules bug - see
// https://github.com/rust-lang/rust/issues/35853#issuecomment-415993963
make_dsl_macros!($ div: Div,
                 p: P,
                 button: Button,
                 option: Option,
                 select: Select
);
