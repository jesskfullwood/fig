//! A web-framework which prioritizes correctness and type-safety.
//!
//! Based closely upon The [Elm](https://elm-lang.org/) Architecture.
//!

use derive_more::{Constructor, From};
use futures::Future;
use wasm_bindgen::{closure::Closure, JsCast};
use web_sys::{
    Document, Element as DomElement, Event as DomEvent, HtmlDivElement, HtmlElement, Location,
    Node, Text, Window,
};

use std::borrow::BorrowMut;
use std::borrow::Cow;
use std::fmt::{self, Debug};
use std::rc::Rc;

use html::{Attribute, Event, EventInner, Tag};

pub mod fetch;
pub mod html;
pub mod program;

pub use program::{application, sandbox};
pub use url;

pub use wasm_bindgen::JsValue;

type JsResult<T> = Result<T, JsValue>;

/// A trait to link togather a Model type and a Msg type.
pub trait Model: 'static + Debug {
    type Msg;
}

type Str = Cow<'static, str>;

fn get_str_prop(elem: &DomElement, key: &str) -> JsResult<String> {
    let key = JsValue::from_str(key);
    js_sys::Reflect::get(elem, &key).and_then(|val| val.as_string().ok_or(val))
}

struct App<M: Model> {
    window: Window,
    target: HtmlDivElement,
    model: Option<M>,
    update: Box<Fn(M::Msg, M) -> (M, Cmd<M::Msg>)>,
    view: Box<Fn(&M) -> Html<M>>,
    // subscriptions: Box<Fn(&M) -> Sub<M::Msg>>,
    on_url_change: Box<Fn(url::Url) -> Cmd<M::Msg>>,
    current_vdom: Html<M>,
}

thread_local! {
    static APP: *mut () = std::ptr::null_mut();
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
    fn loop_update(&mut self, Cmd(mut cmd): Cmd<M::Msg>) -> JsResult<()> {
        let mut loopct = 0;
        loop {
            loopct += 1;
            log!("Loop update (ct: {})", loopct);
            match cmd {
                CmdInner::None => break,
                // return without rendering
                CmdInner::NoOp => return Ok(()),
                CmdInner::Msg(msg) => {
                    let model = self.model.take().unwrap();
                    let (new_model, Cmd(new_cmd)) = (self.update)(msg, model);
                    self.model.replace(new_model);
                    cmd = new_cmd; // we go again
                }
                CmdInner::Fetch(request) => {
                    let fut = request.map(|msg: M::Msg| {
                        App::<M>::with(|app| app.loop_update(Cmd::msg(msg)).expect("update failed"))
                    });
                    fetch::spawn_local(fut);
                    break;
                }
                CmdInner::LoadUrl(urlstr) => {
                    let loc = self.window.location();
                    loc.set_href(&urlstr).expect("Failed to set location");
                    // This should ALWAYS force a reload so return without rendering
                    return Ok(());
                }
                CmdInner::PushUrl(urlstr) => {
                    // push the state...
                    self.push_state(&urlstr).expect("Failed to push state");
                    // Then grab the new href from Location
                    let url = self.location().expect("No location");
                    cmd = ((self.on_url_change)(url)).0;
                }
            }
            if loopct > 100 {
                panic!("Infinite loop!")
            }
        }
        // Don't render the new dom until we finish looping
        log!("update vdom");
        self.current_vdom = self.render_dom()?;
        // returns that it did rerender
        Ok(())
    }

    fn render_dom(&self) -> JsResult<Html<M>> {
        log!("render dom");
        let new_vdom = (self.view)(self.model.as_ref().unwrap());
        let diff = diff_vdom(&self.current_vdom, &new_vdom);
        if let Diff::Unchanged = diff {
            log!("No change");
        } else {
            log!("vdom diff: {:?}", diff);
            let document = self.window.document().expect("No document");
            render_diff(&self.target, &[(0, diff)], &document)?;
        }
        Ok(new_vdom)
    }

    fn with<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        APP.with(|mut ptr| {
            let ptr: *mut () = **ptr.borrow_mut();
            let ptr: *mut App<M> = ptr as *mut App<M>;
            unsafe { f(&mut *ptr) }
        })
    }

    fn push_state(&self, url: &str) -> JsResult<()> {
        let history = self.window.history().expect("No history");
        history.push_state_with_url(&JsValue::NULL, "", Some(&url))
    }

    fn location(&self) -> JsResult<url::Url> {
        let urlstr = self.window.location().href()?;
        url::Url::parse(&urlstr).map_err(|e| JsValue::from_str(&e.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UrlRequest {
    Internal(url::Url),
    External(String),
}

fn intercept_links<M: Model, F: Fn(UrlRequest) -> Cmd<M::Msg> + 'static>(
    location: Location,
    root: DomElement,
    handler: F,
) -> JsResult<Listener<M>> {
    log!("Intercepting links");
    let cb = move |event: DomEvent| -> Cmd<M::Msg> {
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &DomElement = target.dyn_ref().expect("Not an Element");
        if target_el.tag_name() != "A" {
            // short circuit
            return Cmd(CmdInner::NoOp);
        }
        event.prevent_default();
        let target_el: &web_sys::HtmlAnchorElement = target_el.dyn_ref().expect("Not an anchor");
        let urlstr = get_str_prop(target_el, "href").expect("No href");
        let ahost = get_str_prop(target_el, "host").expect("No anchor host");
        let req = if ahost == location.host().expect("No location host") {
            let url = url::Url::parse(&urlstr)
                .map_err(|e| JsValue::from_str(&e.to_string()))
                .expect("Url parse failed");
            UrlRequest::Internal(url)
        } else {
            UrlRequest::External(urlstr)
        };
        handler(req)
    };
    event_handler(root, "click", cb)
}

/// A tree describing which nodes have changed and how
#[derive(Clone)]
enum Diff<'a, M: Model> {
    Insert(&'a Html<M>),
    Replace(&'a Html<M>),
    Remove,
    Update {
        attrs: &'a [Attribute],
        events: &'a [Event<M>],
        children: Vec<(u32, Diff<'a, M>)>,
    },
    Unchanged,
}

impl<'a, M: Model> Debug for Diff<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Diff::*;
        let txt = match self {
            Insert(_) => "Insert",
            Replace(_) => "Replace",
            Remove => "Remove",
            Unchanged => "Unchanged",
            Update {
                attrs,
                events,
                children,
            } => {
                write!(f, "Update {{ ")?;
                if !attrs.is_empty() {
                    write!(f, "attrs ")?;
                }
                if !events.is_empty() {
                    write!(f, "events ")?;
                }
                if !children.is_empty() {
                    write!(f, "children: [")?;
                    for c in children {
                        write!(f, "{:?}", c)?;
                    }
                    write!(f, "]")?;
                }
                return write!(f, "}}");
            }
        };
        write!(f, "{}", txt)
    }
}

impl<'a, M: Model> Diff<'a, M> {
    fn is_unchanged(&self) -> bool {
        if let Diff::Unchanged = self {
            true
        } else {
            false
        }
    }
}

fn diff_vdom<'a, M: Model>(old: &Html<M>, new: &'a Html<M>) -> Diff<'a, M> {
    let (old_el, new_el) = match (old, new) {
        (Html::Text(t1), Html::Text(t2)) => {
            return if t1 == t2 {
                Diff::Unchanged
            } else {
                Diff::Replace(new)
            }
        }
        (Html::Element(e1), Html::Element(e2)) => (e1, e2),
        _ => return Diff::Replace(new),
    };

    if old_el.tag != new_el.tag {
        // assume everything can be nuked
        return Diff::Replace(new);
    }

    let attrs = if old_el.attrs == new_el.attrs {
        &[]
    } else {
        &new_el.attrs[..]
    };
    let events = if old_el.events == new_el.events {
        &[]
    } else {
        &new_el.events[..]
    };

    let mut child_diffs = Vec::new();

    for (ix, (cold, cnew)) in old_el
        .children
        .iter()
        .zip(new_el.children.iter())
        .enumerate()
    {
        let diff = diff_vdom(cold, cnew);
        if !diff.is_unchanged() {
            child_diffs.push((ix as u32, diff_vdom(cold, cnew)))
        }
    }

    // Find nodes which have been added/removed
    // TODO better way to detect 'inserted' nodes with some kind of uuid
    let curct = old_el.children.len();
    let nextct = new_el.children.len();
    if nextct > curct {
        for ix in curct..nextct {
            child_diffs.push((ix as u32, Diff::Insert(&new_el.children[ix])));
        }
    } else {
        for ix in nextct..curct {
            child_diffs.push((ix as u32, Diff::Remove))
        }
    }

    if attrs.is_empty() && events.is_empty() && child_diffs.is_empty() {
        Diff::Unchanged
    } else {
        Diff::Update {
            attrs,
            events,
            children: child_diffs,
        }
    }
}

fn render_diff<'a, M: Model>(
    this_el: &Node,
    child_diffs: &[(u32, Diff<'a, M>)],
    doc: &Document,
) -> JsResult<()> {
    // This might seem slightly odd. Why are we applying changes to the children
    // rather than this_el? Because we need to create, remove, replace them and
    // these operations can only be done from the parent node
    if child_diffs.is_empty() {
        return Ok(());
    }
    let child_els = this_el.child_nodes();
    for &(ix, ref diff) in child_diffs {
        match diff {
            Diff::Unchanged => (),
            Diff::Insert(node) => {
                let new_el = node.render_to_html(doc)?;
                // XXX insert child
                this_el.append_child(&new_el)?;
            }
            Diff::Replace(node) => {
                let old_el = child_els.get(ix).expect("bad replace node index");
                let new_el = node.render_to_html(doc)?;
                this_el.replace_child(&new_el, &old_el)?;
            }
            Diff::Remove => {
                let old_el = child_els.get(ix).expect("bad remove node index");
                this_el.remove_child(&old_el)?;
            }
            Diff::Update {
                attrs,
                events,
                children,
            } => {
                let mut child_el: Node = child_els.get(ix).expect("bad node index");
                if !events.is_empty() {
                    let el: &DomElement = child_el.dyn_ref().expect("Not an element");
                    child_el = replace_events(&el, events)?.unchecked_into();
                }
                if !attrs.is_empty() {
                    let el: &DomElement = child_el.dyn_ref().expect("Not an element");
                    reapply_attrs(&el, attrs)?;
                }
                render_diff(&child_el, &*children, doc)?;
            }
        }
    }
    Ok(())
}

pub struct Cmd<Msg>(CmdInner<Msg>);

enum CmdInner<Msg> {
    None,
    /// Indicates that no work should be done (no diffing or rendering)
    NoOp,
    Msg(Msg),
    Fetch(Box<Future<Item = Msg, Error = JsValue>>),
    LoadUrl(Str),
    PushUrl(Str),
}

impl<Msg> Cmd<Msg> {
    pub fn none() -> Self {
        Cmd(CmdInner::None)
    }

    pub fn msg(msg: Msg) -> Self {
        Cmd(CmdInner::Msg(msg))
    }

    pub fn fetch(fut: impl Future<Item = Msg, Error = JsValue> + 'static) -> Self {
        Cmd(CmdInner::Fetch(Box::new(fut)))
    }

    pub fn push_url(s: impl Into<Str>) -> Self {
        Cmd(CmdInner::PushUrl(s.into()))
    }

    pub fn load_url(s: impl Into<Str>) -> Self {
        Cmd(CmdInner::LoadUrl(s.into()))
    }
}

pub struct Timer {
    id: i32,
    #[allow(dead_code)]
    cb: Closure<FnMut()>,
}

impl Timer {
    pub fn set_interval<M: Model>(
        window: &Window,
        interval_ms: i32,
        f: impl Fn(&M) -> Cmd<M::Msg> + 'static,
    ) -> JsResult<Timer> {
        let cl = move || -> Cmd<M::Msg> {
            App::<M>::with(|app| f(app.model.as_ref().expect("No model")))
        };
        let cb = closure0::<M, _>(cl);
        let jsfunction = cb.as_ref().unchecked_ref();
        window
            .set_interval_with_callback_and_timeout_and_arguments_0(jsfunction, interval_ms)
            .map(|id| Timer { id, cb })
    }
}

impl Drop for Timer {
    fn drop(&mut self) {
        web_sys::window()
            .map(|window| {
                window.clear_interval_with_handle(self.id);
            })
            .unwrap_or(())
    }
}

// See https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeType
/// Represents a DOM Node
#[derive(Debug, From)]
pub enum Html<M: Model> {
    Text(Str),
    Element(Element<M>),
}

impl<M: Model> std::fmt::Display for Html<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Html::Text(text) => write!(f, "{}", text),
            Html::Element(elem) => write!(f, "{}", elem),
        }
    }
}

impl<M: Model> Html<M> {
    fn render_to_html(&self, doc: &Document) -> JsResult<Node> {
        match self {
            Html::Text(text) => Text::new_with_data(text).map(|t| t.unchecked_into()),
            Html::Element(elem) => elem.render_to_html(doc).map(|t| t.unchecked_into()),
        }
    }
}

#[derive(Debug, Constructor)]
/// Represents an HTML Element
pub struct Element<M: Model> {
    tag: Tag,
    attrs: Vec<Attribute>,
    events: Vec<Event<M>>,
    children: Vec<Html<M>>,
}

impl<M: Model> Element<M> {
    /// Create an empty tagged element
    pub fn tag(tag: Tag) -> Element<M> {
        Element::new(tag, Vec::new(), Vec::new(), Vec::new())
    }
}

impl<M: Model> std::fmt::Display for Element<M> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<{}>", self.tag)?;
        for c in &self.children {
            write!(f, "{}", c)?;
        }
        write!(f, "</{}>", self.tag)
    }
}

/// Represents a listener attached to the DOM.
/// When it is dropped it will detach the corresponding listener.
struct Listener<M: Model> {
    element: DomElement,
    type_: Str,
    closure: Closure<FnMut(DomEvent)>,
    marker: std::marker::PhantomData<M>,
}

impl<M: Model> Drop for Listener<M> {
    fn drop(&mut self) {
        log!("Detaching listener for {}", self.type_);
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
    fn new(element: DomElement, type_: Str, closure: Closure<FnMut(DomEvent)>) -> Listener<M> {
        Listener {
            element,
            type_,
            closure,
            marker: std::marker::PhantomData,
        }
    }
}

fn closure0<M: Model, F: FnMut() -> Cmd<M::Msg> + 'static>(mut handler: F) -> Closure<FnMut()> {
    Closure::wrap(Box::new(move || {
        web_sys::console::time();
        let cmd = handler();
        App::<M>::with(move |app| {
            app.loop_update(cmd).expect("Update error");
        });
        web_sys::console::time_end();
    }) as Box<FnMut()>)
}

fn closure1<M: Model, T, F: FnMut(T) -> Cmd<M::Msg> + 'static>(mut handler: F) -> Closure<FnMut(T)>
where
    T: wasm_bindgen::convert::FromWasmAbi + 'static,
{
    Closure::wrap(Box::new(move |val: T| {
        web_sys::console::time();
        let cmd = handler(val);
        App::<M>::with(move |app| {
            app.loop_update(cmd).expect("Update error");
        });
        web_sys::console::time_end();
    }) as Box<FnMut(T)>)
}

fn event_handler<M: Model, S: Into<Str>, F: Fn(DomEvent) -> Cmd<M::Msg> + 'static>(
    element: DomElement,
    event_name: S,
    handler: F,
) -> JsResult<Listener<M>> {
    let event_name = event_name.into();
    log!("New event hander closure");
    let cb = closure1::<M, _, _>(handler);
    let jsfunction = cb.as_ref().unchecked_ref();
    element.add_event_listener_with_callback(&event_name, jsfunction)?;
    Ok(Listener::new(element, event_name, cb))
}

fn input_handler<M: Model>(
    element: &DomElement,
    handler: Rc<dyn Fn(String) -> M::Msg>,
) -> JsResult<Listener<M>> {
    let inner = move |event: DomEvent| {
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &HtmlElement = target.dyn_ref().expect("Not an Html Element");
        Cmd::msg(handler(
            get_str_prop(target_el, "value").expect("missing value"),
        ))
    };
    event_handler::<M, _, _>(element.clone(), "input", inner)
}

fn click_handler<M: Model>(
    element: &DomElement,
    handler: Rc<dyn Fn() -> M::Msg>,
) -> JsResult<Listener<M>> {
    let inner = move |_event: DomEvent| Cmd::msg(handler());
    event_handler::<M, _, _>(element.clone(), "click", inner)
}

fn attach_event_listener<M: Model>(
    event: &Event<M>,
    element: &DomElement,
) -> JsResult<Listener<M>> {
    match &event.inner {
        EventInner::OnClick(cb) => click_handler::<M>(&element, cb.clone()),
        EventInner::OnInput(cb) => input_handler::<M>(&element, cb.clone()),
    }
}

fn apply_attr_to_elem(attr: &Attribute, element: &DomElement) -> JsResult<()> {
    use html::AttributeInner::*;
    match &attr.0 {
        Class(classes) => element.set_attribute("class", &classes.join(" "))?,
        Href(val) => element.set_attribute("href", val)?,
        Id(val) => element.set_attribute("id", val)?,
        Placeholder(val) => element.set_attribute("placeholder", val)?,
        Style(style) => (),
        Value(val) => element.set_attribute("value", val)?,
    };
    Ok(())
}

impl<M: Model> Element<M> {
    fn apply_attrs(&self, element: &DomElement) -> JsResult<()> {
        for attr in &self.attrs {
            apply_attr_to_elem(attr, element)?
        }
        Ok(())
    }

    fn render_to_html(&self, document: &Document) -> JsResult<DomElement> {
        let element = document.create_element(&self.tag.to_string())?;
        self.apply_attrs(&element)?;
        for event in &self.events {
            let listener = attach_event_listener(event, &element)?;
            Box::leak(Box::new(listener));
        }
        for child in &self.children {
            let child_elem = child.render_to_html(document)?;
            element.append_child(&child_elem)?;
        }
        Ok(element)
    }
}

fn replace_events<M: Model>(element: &DomElement, events: &[Event<M>]) -> JsResult<DomElement> {
    log!("Replacing listeners");

    // https://stackoverflow.com/questions/4386300/javascript-dom-how-to-remove-all-events-of-a-dom-object
    // Stupid hack IMO
    let new_element = element.clone_node_with_deep(true)?;
    let new_element: DomElement = new_element.dyn_into()?;

    element
        .parent_node()
        .unwrap()
        .replace_child(&new_element, &element)?;
    for event in events {
        let listener = attach_event_listener(event, &new_element)?;
        // TODO stop leaking the listener!
        Box::leak(Box::new(listener));
    }
    Ok(new_element)
}

fn reapply_attrs(element: &DomElement, attrs: &[Attribute]) -> JsResult<()> {
    for attr in attrs {
        apply_attr_to_elem(attr, element)?
    }
    Ok(())
}
