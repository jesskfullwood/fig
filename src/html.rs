use crate::{Element, Html, Model, Str};
use derive_more::Constructor;
use std::collections::BTreeMap;
use std::hash::Hasher;

use crate::event::Event;

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
    Br => br,
    Button => button,
    Code => code,
    Div => div,
    Em => em,
    H1 => h1,
    H2 => h2,
    H3 => h3,
    H4 => h4,
    H5 => h5,
    H6 => h6,
    Img => img,
    Hr => hr,
    I => i,
    Input => input,
    Li => li,
    Ol => ol,
    Option => option,
    P => p,
    Pre => pre,
    Select => select,
    Small => small,
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

pub use web_sys::console::log_1;

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => ($crate::html::log_1(&format_args!($($t)*).to_string().into()))
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
/// Represents an Element attribute.
// TODO move constructor funcs to own module
pub struct Attribute(pub(crate) AttributeInner);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum AttributeInner {
    Class(Vec<Str>),
    Disabled,
    Href(Str),
    Id(Str),
    Placeholder(Str),
    Selected,
    Style(Style),
    Value(Str),
}

impl Attribute {
    pub fn style(style: Style) -> Attribute {
        Attribute(AttributeInner::Style(style))
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

pub fn selected() -> Attribute {
    Attribute(AttributeInner::Selected)
}

pub fn disabled() -> Attribute {
    Attribute(AttributeInner::Disabled)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Constructor)]
pub struct Style(BTreeMap<String, String>);

impl std::fmt::Display for Style {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for (k, v) in self.0.iter() {
            write!(f, "{}:{};", k, v)?
        }
        Ok(())
    }
}

#[macro_export]
macro_rules! style {
    ($($key:expr =>  $val:expr),* $(,)?) => {
        {
            let mut sty = ::std::collections::BTreeMap::new();
            $(sty.insert($key.into(), $val.into());)*;
            Attribute::style(Style::new(sty))
        }
    }
}

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

impl<M: Model> ElemMod<M> for Str {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(self))
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

impl<M: Model> ElemMod<M> for Option<Attribute> {
    fn modify_element(self, elem: &mut Element<M>) {
        if let Some(attr) = self {
            elem.attrs.push(attr)
        }
    }
}

impl<M: Model> ElemMod<M> for Event<M> {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.events.push(self)
    }
}

impl<M: Model> ElemMod<M> for Option<Event<M>> {
    fn modify_element(self, elem: &mut Element<M>) {
        if let Some(event) = self {
            elem.events.push(event)
        }
    }
}

impl<M: Model> ElemMod<M> for Html<M> {
    fn modify_element(self, elem: &mut Element<M>) {
        elem.children.push(self)
    }
}

impl<M: Model> ElemMod<M> for Option<Html<M>> {
    fn modify_element(self, elem: &mut Element<M>) {
        if let Some(inner) = self {
            elem.children.push(inner)
        }
    }
}
