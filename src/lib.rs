use derive_more::Constructor;
use js_sys::Function;
use std::rc::Rc;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::{Attr, Document, Element, HtmlDivElement, HtmlElement};

use std::borrow::Cow;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};

type UpdateFn<Model, Msg> = fn(Msg, Model) -> (Model, Cmd<Msg>);
type ViewFn<Model> = fn(&Model) -> Html<Model>;

type JsResult<T> = Result<T, JsValue>;

pub trait Model: 'static + Debug {
    type Msg;
}

type Str = Cow<'static, str>;

struct App<M: Model> {
    document: Document,
    target: HtmlDivElement,
    model: Option<M>,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
    current_vdom: Html<M>,
}

impl<M: Model> Drop for App<M> {
    fn drop(&mut self) {
        log!("Warning: dropping app!")
    }
}

impl<M: Model + Debug> Debug for App<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "App (model: {:?})", self.model)
    }
}

impl<M: Model> App<M> {
    fn loop_update(&mut self, mut cmd: Cmd<M::Msg>) -> JsResult<()> {
        let mut loopct = 0;
        loop {
            loopct += 1;
            log!("Loop update (ct: {})", loopct);
            match cmd {
                Cmd::None => break,
                Cmd::Fetch => unimplemented!(),
                Cmd::Msg(msg) => {
                    let model = self.model.take().unwrap();
                    let (new_model, new_cmd) = (self.update)(msg, model);
                    self.model.replace(new_model);
                    cmd = new_cmd; // we go again
                }
            }
            if loopct > 100 {
                panic!("Infinite loop!")
            }
        }
        // Don't render the new dom until we finish looping
        log!("update vdom");
        self.current_vdom = self.render_dom()?;
        Ok(())
    }

    fn render_dom(&self) -> JsResult<Html<M>> {
        log!("render dom");
        let mut new_vdom = (self.view)(self.model.as_ref().unwrap());
        if let Some(diff) = diff_vdom(&self.current_vdom, &new_vdom) {
            log!("vdom diff: {:?}", diff);
            let mut tmp_div = div((), new_vdom);
            render_diff(&tmp_div, &[(0, diff)], &self.target, &self.document)?;
            new_vdom = tmp_div.children.remove(0);
        } else {
            log!("No change");
        }
        Ok(new_vdom)
    }

    fn with(f: impl Fn(&mut Self)) {
        APP.with(|mut ptr| {
            let ptr: *mut () = **ptr.borrow_mut();
            let ptr: *mut App<M> = ptr as *mut App<M>;
            unsafe { f(&mut *ptr) }
        })
    }
}

/// A tree describing which nodes have changed and how
#[derive(Debug, Clone)]
enum Diff {
    Create,
    Remove,
    Replace,
    Update {
        text: bool,
        attrs: bool,
        children: Vec<(u32, Diff)>,
    },
}

fn diff_vdom<M: Model>(current: &Html<M>, next: &Html<M>) -> Option<Diff> {
    if current.tag != next.tag {
        // assume everything can be nuked
        return Some(Diff::Replace);
    }

    let text_changed = current.text != next.text;
    let attr_changed = false; // TODO

    // TODO ordering may change?? Is it even possible to add new attrs?
    // for (old_attr, new_attr) in current.attrs.iter().zip(next.attrs.iter()) {
    //     if old_attr !=  new_attr {
    //         // update the attribute
    //     }
    // }

    let mut child_diffs = Vec::new();

    // Find nodes which have been added/removed
    // TODO better way to detect 'inserted' nodes with some kind of uuid
    let curct = current.children.len() as isize;
    let nextct = next.children.len() as isize;
    if nextct > curct {
        for ix in 0..(nextct - curct) {
            child_diffs.push(((ix + curct) as u32, Diff::Create))
        }
    } else {
        for ix in 0..(curct - nextct) {
            child_diffs.push(((ix + nextct) as u32, Diff::Remove))
        }
    }

    for (ix, (old, new)) in current
        .children
        .iter()
        .zip(next.children.iter())
        .enumerate()
    {
        if let Some(diff) = diff_vdom(old, new) {
            child_diffs.push((ix as u32, diff))
        }
    }

    if !text_changed && !attr_changed && child_diffs.is_empty() {
        None
    } else {
        Some(Diff::Update {
            text: text_changed,
            attrs: attr_changed,
            children: child_diffs,
        })
    }
}

fn render_diff<M: Model>(
    this_vnode: &Html<M>,
    child_diffs: &[(u32, Diff)],
    this_el: &HtmlElement,
    doc: &Document,
) -> JsResult<()> {
    // This might seem slightly odd. Why are we applying changes to the children
    // rather than this_el? Because we need to create, remove, replace them and
    // these operations can only be done from the parent node
    let child_els = this_el.child_nodes();
    for &(ix, ref diff) in child_diffs {
        match diff {
            Diff::Create => {
                let new_el = this_vnode.children[ix as usize].render(doc)?;
                this_el.append_child(&*new_el)?;
            }
            Diff::Replace => {
                let old_el = child_els.get(ix).expect("bad node index");
                let new_el = this_vnode.children[ix as usize].render(doc)?;
                this_el.replace_child(&*new_el, &old_el)?;
            }
            Diff::Remove => {
                let old_el = child_els.get(ix).expect("bad node index");
                this_el.remove_child(&old_el)?;
            }
            Diff::Update {
                text,
                attrs,
                children,
            } => {
                let child_vnode = &this_vnode.children[ix as usize];
                let child_el = child_els.get(ix);
                let child_el: &HtmlElement = child_el
                    .as_ref()
                    .and_then(|e| e.dyn_ref())
                    .expect("bad node index");
                if *text {
                    child_el.set_text_content(child_vnode.text.as_ref().map(|t| t.borrow()))
                }
                render_diff(&child_vnode, &*children, child_el, doc)?;
            }
        }
    }
    Ok(())
}

pub enum Cmd<Msg> {
    None,
    Msg(Msg),
    Fetch,
}

pub fn run<M: Model>(
    model: M,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
    target: &str,
) -> JsResult<()> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let target = document
        .get_element_by_id(target)
        .expect("Target element not found");
    let target: HtmlDivElement = target.dyn_into()?;
    let initial = document.create_element(&Tag::Div.to_string())?;
    target.set_inner_html(""); // blank the target div and create an initial root
    target.append_child(&*initial)?;
    let app = App {
        document,
        target,
        model: Some(model),
        update,
        view,
        current_vdom: Html::new(Tag::Div, None, Vec::new(), Vec::new()), // ^^ now the dome and vdom are in sync
    };

    // put app on the heap...
    let app = Box::new(app);
    // and leak it so we can put it in a thread-local
    let app_ptr = Box::leak::<'static>(app) as *mut App<M> as *mut ();
    // super unsafe. We swap our global void pointer to point to our app.
    // We to do it this way because it isn't possible to have
    // generics in globals. That is, the user of the library chooses their own Model
    // so we can't know the full type of App in advance.
    APP.with(move |ptr| {
        let inner = ptr as *const *mut () as *mut *mut ();
        unsafe {
            *inner = app_ptr;
        }
    });

    // From this point on we only interact with App through App::with.
    // Then it's safe, hopefully.
    App::<M>::with(|app| {
        // initial render
        app.current_vdom = app.render_dom().expect("Render failed");
    });
    Ok(())
}

#[derive(Debug, Constructor)]
pub struct Html<M: Model> {
    tag: Tag,
    text: Option<Str>,
    attrs: Vec<Attribute<M>>,
    children: Vec<Html<M>>,
}

impl<M: Model> std::fmt::Display for Html<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.tag)?;
        if let Some(text) = &self.text {
            return write!(f, "{}", text);
        }
        for c in &self.children {
            write!(f, "{}", c)?;
        }
        write!(f, "</{}>", self.tag)
    }
}

thread_local! {
    pub static APP: *mut () = std::ptr::null_mut();
}

fn input_handler<M: Model>(element: &Element, cb: Rc<dyn Fn(String) -> M::Msg>) -> JsResult<()> {
    log!("New input closure");
    let closure = Closure::wrap(Box::new(move |event: web_sys::InputEvent| {
        App::<M>::with(|app| {
            web_sys::console::time();
            let target: web_sys::EventTarget = event.target().expect("Missing target");
            let target_el: &HtmlElement = target.dyn_ref().expect("Not an Html Element");
            if let Some(val) = target_el.get_attribute("value") {
                let cmd = Cmd::Msg(cb(val));
                app.loop_update(cmd).expect("Closure error");
            } else {
                log!("Element does not have a 'value' attribute")
            }
            web_sys::console::time_end();
        });
    }) as Box<FnMut(web_sys::InputEvent)>);
    element.add_event_listener_with_callback("input", closure.as_ref().unchecked_ref())?;
    closure.forget();
    Ok(())
}

fn click_handler<M: Model>(element: &Element, cb: Rc<dyn Fn() -> M::Msg>) -> JsResult<()> {
    log!("New click closure");
    let closure = Closure::wrap(Box::new(move || {
        web_sys::console::time();
        App::<M>::with(|app| {
            let cmd = Cmd::Msg(cb());
            app.loop_update(cmd).expect("Closure error")
        });
        web_sys::console::time_end();
    }) as Box<FnMut()>);
    element.add_event_listener_with_callback("click", closure.as_ref().unchecked_ref())?;
    closure.forget();
    Ok(())
}

impl<M: Model> Html<M> {
    fn render(&self, document: &Document) -> JsResult<Element> {
        let element = document.create_element(&self.tag.to_string())?;
        for attr in &self.attrs {
            use Attribute::*;
            match attr {
                Value(val) => element.set_attribute("value", val)?,
                Placeholder(val) => element.set_attribute("placeholder", val)?,
                Id(val) => element.set_attribute("id", val)?,
                OnClick(cb) => click_handler::<M>(&element, cb.clone())?,
                OnInput(cb) => input_handler::<M>(&element, cb.clone())?,
                Class(classes) => {
                    element.set_attribute("class", &classes.join(" "))?
                },
                Style(style) => (),
            }
        }
        if let Some(text) = &self.text {
            element.set_text_content(Some(text.borrow()))
        }
        for child in &self.children {
            let child_elem = child.render(document)?;
            element.append_child(&*child_elem)?;
        }
        Ok(element)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tag {
    Button,
    Div,
    H1,
    H2,
    H3,
    H4,
    Input,
    Li,
    Option,
    P,
    Select,
    Span,
    Ul,
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
            Li => "li",
            P => "p",
            Option => "option",
            Select => "select",
            Span => "span",
            Ul => "ul",
        };
        write!(f, "{}", tag)
    }
}

macro_rules! make_elem {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model, A: ToAttr<M>, C: ToHtml<M>>(attrs: A, children: C) -> Html<M> {
            Html::new(Tag::$tag, None, attrs.into_attrs(), children.into_html())
        }
    };
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

macro_rules! make_elem_with_text {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model>(
            attrs: impl ToAttr<M>,
            children: impl ToHtml<M>,
            text: impl Stringify,
        ) -> Html<M> {
            Html::new(
                Tag::$tag,
                text.stringify(),
                attrs.into_attrs(),
                children.into_html(),
            )
        }
    };
}

macro_rules! make_elem_no_kids {
    ($func_name: ident, $tag: ident) => {
        pub fn $func_name<M: Model>(attrs: impl ToAttr<M>, text: impl Stringify) -> Html<M> {
            Html::new(Tag::$tag, text.stringify(), attrs.into_attrs(), Vec::new())
        }
    };
}

make_elem_no_kids!(button, Button);
make_elem_no_kids!(li, Li);
make_elem_no_kids!(option, Option);
make_elem_no_kids!(input, Input);
make_elem_with_text!(p, P);
make_elem!(div, Div);
make_elem!(select, Select);
make_elem!(ul, Ul);

pub enum Attribute<M: Model> {
    Value(String),
    Placeholder(String),
    Class(Vec<Str>),
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

pub trait Classify {
    fn classify(self) -> Vec<Str>;
}

impl<S: Into<Str>> Classify for S {
    fn classify(self) -> Vec<Str> {
        vec![self.into()]
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

attr_key_value!(id, Id);
attr_key_value!(value, Value);
attr_key_value!(placeholder, Placeholder);

pub fn class<M: Model>(c: impl Classify) -> Attribute<M> {
    Attribute::Class(c.classify())
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

macro_rules! impl_to_attrs {
    ($($param:tt),*) => {
        #[allow(non_snake_case, unused_parens)]
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

#[macro_export]
macro_rules! log {
    ($($t:tt)*) => (web_sys::console::log_1(&format_args!($($t)*).to_string().into()))
}
