use crate::{Element, Html, Model, Str};
use derive_more::Constructor;
use std::collections::BTreeMap;

use crate::event::Event;

macro_rules! make_html_tags {
    ($d:tt, $($typ:ident => $text:ident),* $(,)?) => {
        /// Represents an HTML element tag
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub enum Tag {
            // We construct an enum with variants for each HTML tag
            $($typ,)*
        }

        // Impl display. This is how the tag will actually appear in html.
        // Basically just lowercased varaint name
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
            // Now we create a macro corresponding to each variant.
            #[macro_export]
            macro_rules! $text {
                ($d($html:expr),* $d(,)?) => {
                    {
                        use $crate::{Element, html::{Tag, AcceptParent}};
                        // We create a new Element containing the particular Tag variant
                        let mut element = Element::tag(Tag::$typ);
                        // For each child Html<Model>, pass it the element
                        // (i.e. the element 'visits' each child).
                        // the '$d' is in fact the dollar symbol. Clever hack or nasty hack?
                        $d($html.accept_parent_element(&mut element);)*;
                        // Wrap the element as Html<Model> and return
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
    // Would be nice to remove the stuttering here, but I think that would
    // require a proc_macro
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
/// Represents an HTML element attribute.
// TODO move constructor funcs to own module
pub struct Attribute(pub(crate) AttributeInner);

macro_rules! attr_def {
    ($($enum:ident => $inner:ty => $name:ident),* $(,)?) => {
        #[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
        pub(crate) enum AttributeInner {
            $($enum($inner)),*
        }

        impl AttributeInner {
            fn key(&self) -> &str {
                use AttributeInner::*;
                match self {
                    $($enum(_) => stringify!($name),)*
                }
            }
        }
    }
}

attr_def! {
    Class => Vec<Str> => class,
    Disabled => () => disabled,
    Href => Str => href,
    Id => Str => id,
    Placeholder => Str => placeholder,
    Selected => () => selected,
    Style => Style => style,
    Type => Str => type_,
    Value => Str => value
}

impl Attribute {
    pub(crate) fn key(&self) -> &str {
        self.0.key()
    }

    pub(crate) fn value(&self) -> Str {
        use AttributeInner::*;
        match &self.0 {
            Class(classes) => classes.join(" ").into(),
            Disabled(()) => "disabled".into(),
            Selected(()) => "selected".into(),
            Href(val) | Id(val) | Placeholder(val) | Value(val) | Type(val) => val.clone(),
            Style(style) => style.to_string().into(),
        }
    }

    #[doc(hidden)] // Prefer the `style!` macro
    pub fn style(style: Style) -> Self {
        Self(AttributeInner::Style(style))
    }

    #[doc(hidden)] // prefer the `class!` macro
    pub fn class(classes: Vec<Str>) -> Self {
        Self(AttributeInner::Class(classes))
    }
}

macro_rules! attr_key_value_func {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name(val: impl Into<Str>) -> Attribute {
            Attribute(AttributeInner::$tag(val.into()))
        }
    };
}

attr_key_value_func!(href, Href);
attr_key_value_func!(id, Id);
attr_key_value_func!(placeholder, Placeholder);
attr_key_value_func!(type_, Type);
attr_key_value_func!(value, Value);

#[macro_export]
/// Add one or more classes to the element.
///
/// ### Example
/// ```rust
/// # #[macro_use] extern crate fig; use fig::*;
/// # fn main() { let _: Html<()> =
/// p![class!["warning", "unhappy", "bad"], "I am not happy :("]
/// # ;}
/// ```
macro_rules! class {
    ($($item:expr),* $(,)?) => {
        $crate::html::Attribute::class(vec![$(<::std::borrow::Cow<'static, str>>::from($item),)*])
    }
}

/// Add the `selected` attribute to an element
pub fn selected() -> Attribute {
    Attribute(AttributeInner::Selected(()))
}

/// Add the `disabled` attribute to an element
pub fn disabled() -> Attribute {
    Attribute(AttributeInner::Disabled(()))
}

#[doc(hidden)]
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

/// Add styling to an element
///
/// ### Example
/// ```rust
/// # #[macro_use] extern crate fig; use fig::*;
/// # fn main() { let _: Html<()> =
/// div![
///     style! { "height" => "100px", "width" => "500px", "background-color" => "blue" },
///     p!["Stylish div!"]
/// ]
/// # ;}
/// ```
#[macro_export]
macro_rules! style {
    ($($key:expr =>  $val:expr),* $(,)?) => {
        {
            let mut sty = ::std::collections::BTreeMap::new();
            $(sty.insert($key.into(), $val.into());)*;
            $crate::html::Attribute::style($crate::html::Style::new(sty))
        }
    }
}

/// Trait that allows that type to be visted by an Element
pub trait AcceptParent<M: Model> {
    fn accept_parent_element(self, elem: &mut Element<M>);
}

impl<M: Model> AcceptParent<M> for &'static str {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(Str::from(self)))
    }
}

impl<M: Model> AcceptParent<M> for String {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(Str::from(self)))
    }
}

impl<M: Model> AcceptParent<M> for Str {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.children.push(Html::from(self))
    }
}

impl<M: Model, E: AcceptParent<M>> AcceptParent<M> for Vec<E> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        for modifier in self {
            modifier.accept_parent_element(elem)
        }
    }
}

impl<M: Model> AcceptParent<M> for Attribute {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.attrs.push(self)
    }
}

impl<M: Model> AcceptParent<M> for Option<Attribute> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        if let Some(attr) = self {
            elem.attrs.push(attr)
        }
    }
}

impl<M: Model> AcceptParent<M> for Event<M> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.events.push(self)
    }
}

impl<M: Model> AcceptParent<M> for Option<Event<M>> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        if let Some(event) = self {
            elem.events.push(event)
        }
    }
}

impl<M: Model> AcceptParent<M> for Html<M> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        elem.children.push(self)
    }
}

impl<M: Model> AcceptParent<M> for Option<Html<M>> {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        if let Some(inner) = self {
            elem.children.push(inner)
        }
    }
}

impl<M: Model> AcceptParent<M> for () {
    fn accept_parent_element(self, _elem: &mut Element<M>) {}
}

// TODO macro-ify
impl<M: Model> AcceptParent<M> for (Html<M>, Html<M>) {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        let (l, r) = self;
        elem.children.push(l);
        elem.children.push(r);
    }
}

impl<M: Model> AcceptParent<M> for (Html<M>, Html<M>, Html<M>) {
    fn accept_parent_element(self, elem: &mut Element<M>) {
        let (l, m, r) = self;
        elem.children.push(l);
        elem.children.push(m);
        elem.children.push(r);
    }
}
