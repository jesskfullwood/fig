use crate::{Html, Model, Str};
use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

macro_rules! make_html_tags {
    ($d:tt, $($typ:ident => $text:ident),* $(,)?) => {
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum Tag {
            $($typ,)*
        }

        impl std::fmt::Display for Tag {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                use Tag::*;
                let tag = match self {
                    $($typ => stringify!($text),)*
                };
                write!(f, "{}", tag)
            }
        }

        $(
            #[macro_export]
            macro_rules! $text {
                ($d($modifier:expr),* $d(,)?) => {
                    {
                        use $crate::{Html, html::{Tag, ElemMod}};
                        let mut html = Html::tag(Tag::$typ);
                        $d($modifier.modify_element(&mut html);)*;
                        html
                    }
                }
            }
        )*
    }
}

make_html_tags! {
    // Why do we pass in the weird '$' symbol? Workaround for macro_rules bug - see
    // https://github.com/rust-lang/rust/issues/35853#issuecomment-415993963
    $,
    A => a,
    B => b,
    Button => button,
    Div => div,
    Em => em,
    H1 => h1,
    H2 => h2,
    H3 => h3,
    H4 => h4,
    I => i,
    Input => input,
    Li => li,
    Option => option,
    P => p,
    Select => select,
    Span => span,
    Ul => ul
}

pub trait Stringify {
    fn stringify(self) -> Option<Str>;
}

impl Stringify for () {
    fn stringify(self) -> Option<Str> {
        None
    }
}

impl Stringify for &'static str {
    fn stringify(self) -> Option<Str> {
        Some(self.into())
    }
}

impl Stringify for String {
    fn stringify(self) -> Option<Str> {
        Some(self.into())
    }
}

pub trait ToAttr: Sized {
    fn into_attrs(self) -> Vec<Attribute>;
}

macro_rules! impl_to_attrs {
    ($($param:tt),*) => {
        #[allow(non_snake_case, unused_parens)]
        impl<$($param: Into<Attribute>),*> ToAttr for ($($param),*) {
            fn into_attrs(self) -> Vec<Attribute> {
                let ($($param),*) = self;
                vec![$($param.into()),*]
            }
        }
    }
}

impl_to_attrs!();
impl_to_attrs!(A);
impl_to_attrs!(A, B);
impl_to_attrs!(A, B, C);
impl_to_attrs!(A, B, C, D);
impl_to_attrs!(A, B, C, D, E);
impl_to_attrs!(A, B, C, D, E, F);
impl_to_attrs!(A, B, C, D, E, F, G);
impl_to_attrs!(A, B, C, D, E, F, G, H);
impl_to_attrs!(A, B, C, D, E, F, G, H, I);

pub trait ToHtml<M: Model>: Sized {
    fn into_html(self) -> Vec<Html<M>>;
}

impl<M: Model, I: Into<Html<M>>> ToHtml<M> for Vec<I> {
    fn into_html(self) -> Vec<Html<M>> {
        self.into_iter().map(Into::into).collect()
    }
}

macro_rules! impl_to_html {
    ($($param:tt),*) => {
        #[allow(non_snake_case, unused_parens)]
        impl<M: Model, $($param: Into<Html<M>>),*> ToHtml<M> for ($($param),*) {
            fn into_html(self) -> Vec<Html<M>> {
                let ($($param),*) = self;
                vec![$($param.into()),*]
            }
        }
    }
}

impl_to_html!();
impl_to_html!(A);
impl_to_html!(A, B);
impl_to_html!(A, B, C);
impl_to_html!(A, B, C, D);
impl_to_html!(A, B, C, D, E);
impl_to_html!(A, B, C, D, E, F);
impl_to_html!(A, B, C, D, E, F, G);
impl_to_html!(A, B, C, D, E, F, G, H);
impl_to_html!(A, B, C, D, E, F, G, H, I);
impl_to_html!(A, B, C, D, E, F, G, H, I, J);
impl_to_html!(A, B, C, D, E, F, G, H, I, J, K);
impl_to_html!(A, B, C, D, E, F, G, H, I, J, K, L);

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

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
