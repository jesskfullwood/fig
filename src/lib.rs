use derive_more::Constructor;
use wasm_bindgen::{JsValue};
use web_sys::{Element, Document, Attr};
use js_sys::{Function};
use std::rc::Rc;

type UpdateFn<Model, Msg> = fn(Msg, Model) -> (Model, Cmd<Msg>);
type ViewFn<Model> = fn(&Model) -> Html<Model>;

pub trait Model {
    type Msg;
}

struct App<M: Model> {
    model: M,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
}

impl<M: Model> App<M> {
    fn render_dom(&self) -> Result<(), JsValue> {
        let window = web_sys::window().expect("no global `window` exists");
        let document = window.document().expect("should have a document on window");
        let body = document.body().expect("document should have a body");
        let vdom = (self.view)(&self.model);
        let root_element = vdom.render(&document)?;
        body.append_child(&*root_element)?;
        Ok(())
    }
}

pub enum Cmd<Msg> {
    None,
    Msg(Msg),
    Fetch,
}

pub fn run<M: Model>(
    model: M,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>
) {
    let app = App { model, update, view };
    app.render_dom();
}

#[derive(Constructor)]
pub struct Html<M: Model> {
    elem: Tag,
    attrs: Vec<Attribute<M>>,
    children: Vec<Html<M>>,
}

impl<M: Model> std::fmt::Display for Html<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Tag::Text(s) = &self.elem {
            return write!(f, "{}", s)
        }
        write!(f, "<{}>", self.elem)?;
        for c in &self.children {
            write!(f, "{}", c)?;
        }
        write!(f, "</{}>", self.elem)
    }
}

fn fetch_app<M: Model>() -> App<M> {
    unimplemented!()
}

fn click_fn<M: Model>(f: Rc<Fn() -> M::Msg>) -> Function {
    let app = fetch_app::<M>();
    (app.update)(f(), app.model);
    unimplemented!()
    // Closure::new(|| (app.update)(f(), app.model))
}

fn input_fn<M: Model>(f: Rc<Fn(String) -> M::Msg>) -> Function {
    unimplemented!()
}

impl<M: Model> Html<M> {
    fn render(&self, document: &Document) -> Result<Element, JsValue> {
        let element = document.create_element(&self.elem.to_string())?;
        for attr in &self.attrs {
            use Attribute::*;
            match attr {
                Value(val) => element.set_attribute("value", val)?,
                // OnClick(cb) => element.add_event_listener_with_callback("click", &click_fn::<M>(Rc::clone(cb)))?,
                // OnInput(cb) => element.add_event_listener_with_callback("click", &input_fn(Rc::clone(cb)))?,
                _ => ()
            }
        }
        for child in &self.children {
            if let Tag::Text(text) = &child.elem {
                element.set_text_content(Some(text))
            } else {
                let child_elem = child.render(document)?;
                element.append_child(&*child_elem)?;
            }
        }
        Ok(element)
    }
}

#[derive(Clone, Debug)]
pub enum Tag {
    H1,
    H2,
    H3,
    H4,
    Div,
    P,
    Span,
    Select,
    Option,
    Text(String),
}

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Tag::*;
        let tag = match self {
            H1 => "h1",
            H2 => "h2",
            H3 => "h3",
            H4 => "h4",
            Div => "div",
            P => "p",
            Span => "span",
            Select => "select",
            Option => "option",
            Text(text) => return write!(f, "{}", text)
        };
        write!(f, "{}", tag)
    }
}

macro_rules! make_elem {
    ($func_name: ident, $tag: expr) => {
        pub fn $func_name<M: Model, A: ToAttr<M>, C: ToHtml<M>>(attrs: A, children: C) -> Html<M> {
            Html::new($tag, attrs.into_attrs(), children.into_html())
        }
    }
}

make_elem!(div, Tag::Div);
make_elem!(p, Tag::P);
make_elem!(select, Tag::Select);
make_elem!(option, Tag::Option);

pub fn text<M: Model>(s: impl ToString) -> Html<M> {
    Html::new(Tag::Text(s.to_string()), Vec::new(), Vec::new())
}

pub enum Attribute<M: Model> {
    Value(String),
    Id(String),
    Style(Style),
    OnClick(Rc<dyn Fn() -> M::Msg>),
    OnInput(Rc<Fn(String) -> M::Msg>)
}

pub struct Style;

pub fn value<M: Model>(val: impl ToString) -> Attribute<M> {
    Attribute::Value(val.to_string())
}

pub fn on_click<M: Model>(f: impl Fn() -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnClick(Rc::new(f))
}

pub fn on_input<M: Model>(f: impl Fn(String) -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnInput(Rc::new(f))
}

pub trait ToAttr<M: Model>: Sized {
    fn into_attrs(self) -> Vec<Attribute<M>>;
}

impl<M: Model> ToAttr<M> for () {
    fn into_attrs(self) -> Vec<Attribute<M>> {
        Vec::new()
    }
}

impl<M: Model> ToAttr<M> for Attribute<M> {
    fn into_attrs(self) -> Vec<Attribute<M>> {
        vec![self]
    }
}

impl<M: Model> ToAttr<M> for (Attribute<M>, Attribute<M>) {
    fn into_attrs(self) -> Vec<Attribute<M>> {
        vec![self.0, self.1]
    }
}

pub trait ToHtml<M: Model>: Sized {
    fn into_html(self) -> Vec<Html<M>>;
}

impl<M: Model> ToHtml<M> for () {
    fn into_html(self) -> Vec<Html<M>> {
        Vec::new()
    }
}
impl<M: Model> ToHtml<M> for Html<M> {
    fn into_html(self) -> Vec<Html<M>> {
        vec![self]
    }
}
impl<M: Model> ToHtml<M> for (Html<M>, Html<M>) {
    fn into_html(self) -> Vec<Html<M>> {
        vec![
            self.0,
            self.1,
        ]
    }
}
impl<M: Model> ToHtml<M> for (Html<M>, Html<M>, Html<M>) {
    fn into_html(self) -> Vec<Html<M>> {
        vec![
            self.0,
            self.1,
            self.2,
        ]
    }
}

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}

