use derive_more::Constructor;
use std::rc::Rc;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::{Document, Element, Event as DomEvent, HtmlDivElement, HtmlElement};

use std::borrow::Cow;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};

use html::{Attribute, Event};

pub mod html;
pub mod diff;

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

thread_local! {
    pub static APP: *mut () = std::ptr::null_mut();
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
        let diff = diff_vdom(&self.current_vdom, &new_vdom);
        if let Diff::Unchanged = diff {
            log!("No change");
        } else {
            log!("vdom diff: {:?}", diff);
            let mut tmp_div = Html::new(Tag::Div, None, Vec::new(), Vec::new(), vec![new_vdom]);
            render_diff(&tmp_div, &[(0, diff)], &self.target, &self.document)?;
            new_vdom = tmp_div.children.remove(0);
        }
        Ok(new_vdom)
    }

    fn with(f: impl FnOnce(&mut Self)) {
        APP.with(|mut ptr| {
            let ptr: *mut () = **ptr.borrow_mut();
            let ptr: *mut App<M> = ptr as *mut App<M>;
            unsafe { f(&mut *ptr) }
        })
    }
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
        current_vdom: Html::tag(Tag::Div), // now the dom and vdom are in sync
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

/// A tree describing which nodes have changed and how
#[derive(Debug, Clone)]
enum Diff {
    Create,
    Replace,
    Remove,
    Update {
        text: bool,
        attrs: bool,
        children: Vec<(u32, Diff)>,
    },
    Unchanged,
}

fn diff_vdom<M: Model>(current: &Html<M>, next: &Html<M>) -> Diff {
    if current.tag != next.tag {
        // assume everything can be nuked
        return Diff::Replace;
    }

    let text_changed = current.text != next.text;
    let mut attr_changed = false;

    // TODO ordering may change?? Is it even possible to add new attrs?
    for (old_attr, new_attr) in current.attrs.iter().zip(next.attrs.iter()) {
        if old_attr != new_attr {
            attr_changed = true
        }
    }

    for (old_attr, new_attr) in current.attrs.iter().zip(next.attrs.iter()) {
        if old_attr != new_attr {
            attr_changed = true
        }
    }

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
            child_diffs.push((ix as u32, diff_vdom(old, new)))
    }

    if !text_changed && !attr_changed && child_diffs.is_empty() {
        Diff::Unchanged
    } else {
        Diff::Update {
            text: text_changed,
            attrs: attr_changed,
            children: child_diffs,
        }
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
    let child_els = this_el.children();
    for &(ix, ref diff) in child_diffs {
        match diff {
            Diff::Unchanged => (),
            Diff::Create => {
                let new_el = this_vnode.children[ix as usize].render_to_dom(doc)?;
                this_el.append_child(&*new_el)?;
            }
            Diff::Replace => {
                let old_el = child_els.get_with_index(ix).expect("bad node index");
                let new_el = this_vnode.children[ix as usize].render_to_dom(doc)?;
                this_el.replace_child(&*new_el, &old_el)?;
            }
            Diff::Remove => {
                let old_el = child_els.get_with_index(ix).expect("bad node index");
                this_el.remove_child(&old_el)?;
            }
            Diff::Update {
                text,
                attrs,
                children,
            } => {
                let child_vnode = &this_vnode.children[ix as usize];
                let child_el: Element = child_els.get_with_index(ix).expect("bad node index");
                let child_el: &HtmlElement = child_el.dyn_ref().expect("bad element cast");
                if *text {
                    child_el.set_text_content(child_vnode.text.as_ref().map(|t| t.borrow()));
                }
                if *attrs {
                    child_vnode.reapply_attrs(child_el)?;
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

#[derive(Debug, Constructor)]
pub struct Html<M: Model> {
    tag: Tag,
    text: Option<Str>,
    attrs: Vec<Attribute>,
    events: Vec<Event<M>>,
    children: Vec<Html<M>>,
}

impl<M: Model> Html<M> {
    /// Create an empty tagged element
    pub fn tag(tag: Tag) -> Html<M> {
        Html::new(tag, None, Vec::new(), Vec::new(), Vec::new())
    }
}

impl<M: Model> std::fmt::Display for Html<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.tag)?;
        if let Some(text) = &self.text {
            write!(f, "{}", text)?;
        }
        for c in &self.children {
            write!(f, "{}", c)?;
        }
        write!(f, "</{}>", self.tag)
    }
}

pub struct Listener<M: Model> {
    element: Element,
    type_: Str,
    closure: Closure<FnMut(DomEvent)>,
    marker: std::marker::PhantomData<M>,
}

impl<M: Model> Drop for Listener<M> {
    fn drop(&mut self) {
        log!("Detching listener");
        self.element
            .remove_event_listener_with_callback(&self.type_, self.closure.as_ref().unchecked_ref())
            .expect("failed to remove");
    }
}

impl<M: Model> Debug for Listener<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Listener")
    }
}

impl<M: Model> Listener<M> {
    fn new(element: Element, type_: Str, closure: Closure<FnMut(DomEvent)>) -> Listener<M> {
        Listener {
            element,
            type_,
            closure,
            marker: std::marker::PhantomData,
        }
    }
}

fn event_handler<M: Model, S: Into<Str>, F: Fn(DomEvent) -> M::Msg + 'static>(
    element: Element,
    event_name: S,
    handler: F,
) -> JsResult<Listener<M>> {
    let event_name = event_name.into();
    log!("New event hander closure");
    let closure = Closure::wrap(Box::new(move |event: DomEvent| {
        web_sys::console::time();
        let cmd = Cmd::Msg(handler(event));
        App::<M>::with(move |app| {
            app.loop_update(cmd).expect("Update error");
        });
        web_sys::console::time_end();
    }) as Box<FnMut(DomEvent)>);
    let jsfunction = closure.as_ref().unchecked_ref();
    element.add_event_listener_with_callback(&event_name, jsfunction);
    Ok(Listener::new(element, event_name, closure))
}

fn input_handler<M: Model>(
    element: &Element,
    handler: fn(String) -> M::Msg,
) -> JsResult<Listener<M>> {
    let key = JsValue::from_str("value");
    let inner = move |event: DomEvent| {
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &HtmlElement = target.dyn_ref().expect("Not an Html Element");
        let val = js_sys::Reflect::get(target_el, &key).expect("Failed to get property");
        handler(val.as_string().expect("value not a string"))
    };
    event_handler::<M, _, _>(element.clone(), "input", inner)
}

fn click_handler<M: Model>(
    element: &Element,
    handler: fn() -> M::Msg,
) -> JsResult<Listener<M>> {
    let inner = move |_event: DomEvent| handler();
    event_handler::<M, _, _>(element.clone(), "click", inner)
}

fn attach_event_listener<M: Model>(event: &Event<M>, element: &Element) -> JsResult<Listener<M>> {
    match event {
        Event::OnClick(cb) => click_handler::<M>(&element, *cb),
        Event::OnInput(cb) => input_handler::<M>(&element, *cb),
    }
}

fn apply_attr_to_elem(attr: &Attribute, element: &Element) -> JsResult<()> {
    use Attribute::*;
    match attr {
        Value(val) => element.set_attribute("value", val)?,
        Placeholder(val) => element.set_attribute("placeholder", val)?,
        Id(val) => element.set_attribute("id", val)?,
        Class(classes) => element.set_attribute("class", &classes.join(" "))?,
        Style(style) => (),
    };
    Ok(())
}

impl<M: Model> Html<M> {
    fn apply_attrs(&self, element: &Element) -> JsResult<()> {
        for attr in &self.attrs {
            apply_attr_to_elem(attr, element)?
        }
        Ok(())
    }

    fn reapply_attrs(&self, element: &Element) -> JsResult<()> {
        for attr in &self.attrs {
            apply_attr_to_elem(attr, element)?
        }
        Ok(())
    }

    fn render_to_dom(&self, document: &Document) -> JsResult<Element> {
        let element = document.create_element(&self.tag.to_string())?;
        self.apply_attrs(&element)?;
        if let Some(text) = &self.text {
            element.set_text_content(Some(text.borrow()))
        }
        for event in &self.events {
            let listener = attach_event_listener(event, &element)?;
            Box::leak(Box::new(listener));
        }
        for child in &self.children {
            let child_elem = child.render_to_dom(document)?;
            element.append_child(&*child_elem)?;
        }
        Ok(element)
    }
}

macro_rules! make_tags {
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
                        use $crate::{Html, Tag, html::ElemMod};
                        let mut html = Html::tag(Tag::$typ);
                        $d($modifier.modify_element(&mut html);)*;
                        html
                    }
                }
            }
        )*
    }
}

// Why do we pass in the weird'$' symbol? Workaround for macro_rules bug - see
// https://github.com/rust-lang/rust/issues/35853#issuecomment-415993963
make_tags! {
    $,
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
