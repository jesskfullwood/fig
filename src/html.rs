use crate::{Element, Html, Model, Str};
use derive_more::Display;
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
                        use $crate::{Element, html::{Tag, ElemMod}};
                        let mut element = Element::tag(Tag::$typ);
                        $d($modifier.modify_element(&mut element);)*;
                        Html::from(element)
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
    Hr => hr,
    I => i,
    Input => input,
    Li => li,
    Option => option,
    P => p,
    Select => select,
    Span => span,
    Ul => ul
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
/// Represents an Element attribute.
// TODO move constructor funcs to own module
pub struct Attribute(pub(crate) AttributeInner);

#[derive(Debug, PartialEq)]
pub(crate) enum AttributeInner {
    Value(Str),
    Href(Str),
    Placeholder(Str),
    Class(Vec<Str>),
    Id(Str),
    Style(Style),
}

#[derive(Clone, Copy, Debug, PartialEq, Display)]
struct ClosureId(u64);

pub struct Event<M: Model> {
    id: ClosureId,
    pub(crate) inner: EventInner<M>,
}

pub(crate) enum EventInner<M: Model> {
    OnClick(Rc<Fn() -> M::Msg>),
    OnInput(Rc<Fn(String) -> M::Msg>),
}

impl<M: Model> fmt::Debug for Event<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            EventInner::OnClick(_) => write!(f, "OnClickEvent({})", self.id),
            EventInner::OnInput(_) => write!(f, "OnInputEvent({})", self.id),
        }
    }
}

impl<M: Model> PartialEq for Event<M> {
    fn eq(&self, other: &Event<M>) -> bool {
        self.id == other.id
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
    Event {
        id: ClosureId(hash),
        inner: EventInner::OnClick(Rc::new(move || f(&s))),
    }
}

pub fn on_input<M: Model, S: Hash + 'static>(s: S, f: fn(&S, String) -> M::Msg) -> Event<M> {
    // Can't hash fn ptr - compiler bug! Do very unsafe workaround
    // https://github.com/rust-lang/rust/issues/46989
    let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
    let hash = hash_closure(&s, ptr);
    Event {
        id: ClosureId(hash),
        inner: EventInner::OnInput(Rc::new(move |val| f(&s, val))),
    }
}

macro_rules! attr_key_value {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name(val: impl Into<Str>) -> Attribute {
            Attribute(AttributeInner::$tag(val.into()))
        }
    };
}

attr_key_value!(id, Id);
attr_key_value!(value, Value);
attr_key_value!(placeholder, Placeholder);
attr_key_value!(href, Href);

#[macro_export]
macro_rules! class {
    ($($item:expr),* $(,)?) => {
        $crate::html::class(vec![$(<::std::borrow::Cow<'static, str>>::from($item),)*])
    }
}

pub fn class(classes: Vec<Str>) -> Attribute {
    Attribute(AttributeInner::Class(classes))
}

#[derive(Debug, Clone, PartialEq)]
pub struct Style;

pub trait ElemMod<M: Model> {
    fn modify_element(self, elem: &mut Element<M>);
}

impl<M: Model> ElemMod<M> for &'static str {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(Str::from(self)))
    }
}

impl<M: Model> ElemMod<M> for String {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(Str::from(self)))
    }
}

impl<M: Model, E: ElemMod<M>> ElemMod<M> for Vec<E> {
    fn modify_element(self, elem: &mut Element<M>) {
        for modifier in self {
            modifier.modify_element(elem)
        }
    }
}

impl<M: Model> ElemMod<M> for Attribute {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.attrs.push(self)
    }
}

impl<M: Model> ElemMod<M> for Event<M> {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.events.push(self)
    }
}

impl<M: Model> ElemMod<M> for Html<M> {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.children.push(self)
    }
}
