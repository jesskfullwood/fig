use derive_more::Constructor;
use js_sys::Function;
use std::rc::Rc;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::{Attr, Document, Element};

use std::borrow::BorrowMut;
use std::fmt::{self, Debug};

type UpdateFn<Model, Msg> = fn(Msg, Model) -> (Model, Cmd<Msg>);
type ViewFn<Model> = fn(&Model) -> Html<Model>;

pub trait Model: 'static {
    type Msg;
}

struct App<M: Model> {
    model: Option<M>,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
}

impl<M: Model> Drop for App<M> {
    fn drop(&mut self) {
        log!("dropping app")
    }
}

impl<M: Model + Debug> Debug for App<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "App (model: {:?})", self.model)
    }
}

impl<M: Model> App<M> {
    fn render_dom(&self) -> Result<(), JsValue> {
        log!("Render dom");
        let window = web_sys::window().expect("no global `window` exists");
        let document = window.document().expect("should have a document on window");
        let body = document.body().expect("document should have a body");
        log!("get vdom");
        let vdom = (self.view)(self.model.as_ref().unwrap());
        let root_element = vdom.render(&document)?;
        body.append_child(&*root_element)?;
        Ok(())
    }

    fn with(f: impl Fn(&mut Self)) {
        APP.with(|mut ptr| {
            let ptr: *mut () = **ptr.borrow_mut();
            let ptr: *mut App<M> = ptr as *mut App<M>;
            unsafe { f(&mut *ptr) }
        })
    }
}

pub enum Cmd<Msg> {
    None,
    Msg(Msg),
    Fetch,
}

pub fn run<M: Model>(model: M, update: UpdateFn<M, M::Msg>, view: ViewFn<M>) {
    log!("Run");
    let app = App {
        model: Some(model),
        update,
        view,
    };

    // leak the app so we can put it in thread local
    let app = Box::new(app);
    let app_ptr = Box::leak::<'static>(app) as *mut App<M> as *mut ();

    // Place app inside thread-local
    APP.with(move |ptr| {
        let inner = ptr as *const *mut () as *mut *mut ();
        unsafe {
            *inner = app_ptr;
        }
    });

    App::<M>::with(|app| {
        app.render_dom().expect("Render failed");
    })
}

#[derive(Debug, Constructor)]
pub struct Html<M: Model> {
    elem: Tag,
    attrs: Vec<Attribute<M>>,
    children: Vec<Html<M>>,
}

impl<M: Model> std::fmt::Display for Html<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Tag::Text(s) = &self.elem {
            return write!(f, "{}", s);
        }
        write!(f, "<{}>", self.elem)?;
        for c in &self.children {
            write!(f, "{}", c)?;
        }
        write!(f, "</{}>", self.elem)
    }
}

thread_local! {
    pub static APP: *mut () = std::ptr::null_mut();
}

fn input_element<M: Model>(
    element: &Element,
    cb: Rc<dyn Fn(String) -> M::Msg>,
) -> Result<(), JsValue> {
    let closure = Closure::wrap(Box::new(move |event: web_sys::InputEvent| {
        App::<M>::with(|app| {
            let target: web_sys::EventTarget = event.target().expect("Missing target");
            let elem: &web_sys::HtmlInputElement =
                target.dyn_ref().expect("Bad cast the html input element");
            let msg = cb(elem.value());
            let model = app.model.take().unwrap();
            let (model, cmd) = (app.update)(msg, model);
            app.model.replace(model);
        });
    }) as Box<FnMut(web_sys::InputEvent)>);
    element.add_event_listener_with_callback("input", closure.as_ref().unchecked_ref())?;
    closure.forget();
    Ok(())
}

fn click_element<M: Model>(element: &Element, cb: Rc<dyn Fn() -> M::Msg>) -> Result<(), JsValue> {
    let closure = Closure::wrap(Box::new(move || {
        App::<M>::with(|app| {
            let msg = cb();
            let model = app.model.take().unwrap();
            let (model, cmd) = (app.update)(msg, model);
            app.model.replace(model);
        });
    }) as Box<FnMut()>);
    element.add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())?;
    closure.forget();
    Ok(())
}

impl<M: Model> Html<M> {
    fn render(&self, document: &Document) -> Result<Element, JsValue> {
        log!("render");
        let element = document.create_element(&self.elem.to_string())?;
        for attr in &self.attrs {
            use Attribute::*;
            match attr {
                Value(val) => element.set_attribute("value", val)?,
                Placeholder(val) => element.set_attribute("placeholder", val)?,
                OnClick(cb) => click_element::<M>(&element, cb.clone())?,
                OnInput(cb) => input_element::<M>(&element, cb.clone())?,
                _ => (),
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
    Button,
    Div,
    H1,
    H2,
    H3,
    H4,
    Input,
    Option,
    P,
    Select,
    Span,
    Text(String),
}

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Tag::*;
        let tag = match self {
            Button => "button",
            Div => "div",
            H1 => "h1",
            H2 => "h2",
            H3 => "h3",
            H4 => "h4",
            Input => "input",
            P => "p",
            Option => "option",
            Select => "select",
            Span => "span",
            Text(text) => return write!(f, "{}", text),
        };
        write!(f, "{}", tag)
    }
}

macro_rules! make_elem {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model, A: ToAttr<M>, C: ToHtml<M>>(attrs: A, children: C) -> Html<M> {
            Html::new(Tag::$tag, attrs.into_attrs(), children.into_html())
        }
    };
}

make_elem!(div, Div);
make_elem!(button, Button);
make_elem!(p, P);
make_elem!(select, Select);
make_elem!(option, Option);
make_elem!(input, Input);

pub fn text<M: Model>(s: impl ToString) -> Html<M> {
    Html::new(Tag::Text(s.to_string()), Vec::new(), Vec::new())
}

pub enum Attribute<M: Model> {
    Value(String),
    Placeholder(String),
    Id(String),
    Style(Style),
    OnClick(Rc<dyn Fn() -> M::Msg>),
    OnInput(Rc<Fn(String) -> M::Msg>),
}

impl<M: Model> fmt::Debug for Attribute<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Attribute")
    }
}

pub struct Style;

macro_rules! attr_key_value {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model>(val: impl ToString) -> Attribute<M> {
            Attribute::$tag(val.to_string())
        }
    };
}

attr_key_value!(value, Value);
attr_key_value!(placeholder, Placeholder);

pub fn on_click<M: Model>(f: impl Fn() -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnClick(Rc::new(f))
}

pub fn on_input<M: Model>(f: impl Fn(String) -> M::Msg + 'static) -> Attribute<M> {
    Attribute::OnInput(Rc::new(f))
}

pub trait ToAttr<M: Model>: Sized {
    fn into_attrs(self) -> Vec<Attribute<M>>;
}

macro_rules! impl_to_attrs {
    ($($param:tt),*) => {
        #[allow(non_snake_case)]
        impl<M: Model, $($param: Into<Attribute<M>>),*> ToAttr<M> for ($($param),*) {
            fn into_attrs(self) -> Vec<Attribute<M>> {
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

macro_rules! impl_to_html {
    ($($param:tt),*) => {
        #[allow(non_snake_case)]
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

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}
