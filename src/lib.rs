use derive_more::Constructor;
use futures::Future;
use wasm_bindgen::{closure::Closure, JsCast, JsValue};
use web_sys::{
    Document, Element, Event as DomEvent, HtmlDivElement, HtmlElement, Location, Window,
};

use std::borrow::Cow;
use std::borrow::{Borrow, BorrowMut};
use std::fmt::{self, Debug};
use std::rc::Rc;

use html::{Attribute, Event, Tag};

pub mod fetch;
pub mod html;
pub use url;

type UpdateFn<Model, Msg> = fn(Msg, Model) -> (Model, Cmd<Msg>);
type ViewFn<Model> = fn(&Model) -> Html<Model>;
type OnUrlRequestFn<Msg> = fn(UrlRequest) -> Cmd<Msg>;
type OnUrlChangeFn<Msg> = fn(url::Url) -> Cmd<Msg>;

type JsResult<T> = Result<T, JsValue>;

pub trait Model: 'static + Debug {
    type Msg;
}

type Str = Cow<'static, str>;

fn get_str_prop(elem: &Element, key: &str) -> JsResult<String> {
    let key = JsValue::from_str(key);
    js_sys::Reflect::get(elem, &key).and_then(|val| val.as_string().ok_or(val))
}

fn intercept_links<M: Model>(
    location: Location,
    root: Element,
    handler: fn(UrlRequest) -> Cmd<M::Msg>,
) -> JsResult<Listener<M>> {
    log!("Intercepting links");
    let cb = move |event: DomEvent| -> Cmd<M::Msg> {
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &Element = target.dyn_ref().expect("Not an Element");
        if target_el.tag_name() != "A" {
            // short circuit
            return Cmd::None;
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

struct App<M: Model> {
    window: Window,
    target: HtmlDivElement,
    model: Option<M>,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
    on_url_request: OnUrlRequestFn<M::Msg>,
    on_url_change: OnUrlChangeFn<M::Msg>,
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
    fn loop_update(&mut self, mut cmd: Cmd<M::Msg>) -> JsResult<()> {
        if let Cmd::None = cmd {
            // null op
            log!("Update: nil op");
            return Ok(());
        }
        let mut loopct = 0;
        loop {
            loopct += 1;
            log!("Loop update (ct: {})", loopct);
            match cmd {
                Cmd::None => break,
                Cmd::Msg(msg) => {
                    let model = self.model.take().unwrap();
                    let (new_model, new_cmd) = (self.update)(msg, model);
                    self.model.replace(new_model);
                    cmd = new_cmd; // we go again
                }
                Cmd::Fetch(request) => {
                    let fut = request.map(|msg: M::Msg| {
                        App::<M>::with(|app| app.loop_update(Cmd::Msg(msg)).expect("update failed"))
                    });
                    fetch::spawn_local(fut);
                    break;
                }
                Cmd::NavigateAway(urlstr) => {
                    let loc = self.window.location();
                    loc.set_href(&urlstr).expect("Failed to set location");
                    cmd = Cmd::None;
                }
                Cmd::PushUrl(url) => {
                    self.push_state(&url).expect("Failed to push state");
                    cmd = (self.on_url_change)(url);
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
            let document = self.window.document().expect("No document");
            render_diff(&tmp_div, &[(0, diff)], &self.target, &document)?;
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

    fn push_state(&self, url: &url::Url) -> JsResult<()> {
        let history = self.window.history().expect("No history");
        history.push_state_with_url(&JsValue::NULL, "", Some(&url.to_string()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UrlRequest {
    Internal(url::Url),
    External(String),
}

pub fn run<M: Model>(
    model: M,
    update: UpdateFn<M, M::Msg>,
    view: ViewFn<M>,
    on_url_request: OnUrlRequestFn<M::Msg>,
    on_url_change: OnUrlChangeFn<M::Msg>,
    target: &str,
) -> JsResult<()> {
    console_error_panic_hook::set_once();
    let window = web_sys::window().expect("No global `window` exists");
    let document = window.document().expect("No document");
    let location = document.location().expect("No location");
    let target = document
        .get_element_by_id(target)
        .expect("Target element not found");
    let target: HtmlDivElement = target.dyn_into()?;
    let downcast_cpy: Element = target.clone().dyn_into().unwrap();
    let initial = document.create_element(&Tag::Div.to_string())?;
    target.set_inner_html(""); // blank the target div and create an initial root
    target.append_child(&*initial)?;
    let app = App {
        window,
        target,
        model: Some(model),
        update,
        view,
        on_url_request,
        on_url_change,
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

    let link_listener = intercept_links::<M>(location, downcast_cpy, on_url_request)?;
    Box::leak(Box::new(link_listener));

    Ok(())
}

/// A tree describing which nodes have changed and how
#[derive(Clone)]
enum Diff {
    Create,
    Replace,
    Remove,
    Update {
        text: bool,
        attrs: bool,
        events: bool,
        children: Vec<(u32, Diff)>,
    },
    Unchanged,
}

impl Debug for Diff {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Diff::*;
        let txt = match self {
            Create => "Create",
            Replace => "Replace",
            Remove => "Remove",
            Unchanged => "Unchanged",
            Update {
                text,
                attrs,
                events,
                children,
            } => {
                write!(f, "Update {{ ")?;
                if *text {
                    write!(f, "text ")?;
                }
                if *attrs {
                    write!(f, "attrs ")?;
                }
                if *events {
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

impl Diff {
    fn is_unchanged(&self) -> bool {
        if let Diff::Unchanged = self {
            true
        } else {
            false
        }
    }
}

fn diff_vdom<M: Model>(current: &Html<M>, next: &Html<M>) -> Diff {
    if current.tag != next.tag {
        // assume everything can be nuked
        return Diff::Replace;
    }

    let text_changed = current.text != next.text;
    let attrs_changed = current.attrs != next.attrs;
    let events_changed = current.events != next.events;

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
        let diff = diff_vdom(old, new);
        if !diff.is_unchanged() {
            child_diffs.push((ix as u32, diff_vdom(old, new)))
        }
    }

    if !text_changed && !attrs_changed && !events_changed && child_diffs.is_empty() {
        Diff::Unchanged
    } else {
        Diff::Update {
            text: text_changed,
            attrs: attrs_changed,
            events: events_changed,
            children: child_diffs,
        }
    }
}

fn render_diff<M: Model>(
    this_vnode: &Html<M>,
    child_diffs: &[(u32, Diff)],
    this_el: &Element,
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
                let old_el = child_els
                    .get_with_index(ix)
                    .expect("bad replace node index");
                let new_el = this_vnode.children[ix as usize].render_to_dom(doc)?;
                this_el.replace_child(&*new_el, &old_el)?;
            }
            Diff::Remove => {
                let old_el = child_els.get_with_index(ix).expect("bad remove node index");
                this_el.remove_child(&old_el)?;
            }
            Diff::Update {
                text,
                attrs,
                events,
                children,
            } => {
                let child_vnode = &this_vnode.children[ix as usize];
                let mut child_el: Element = child_els.get_with_index(ix).expect("bad node index");
                if *events {
                    child_el = child_vnode.replace_events(&child_el)?;
                }
                if *text {
                    child_el.set_text_content(child_vnode.text.as_ref().map(|t| t.borrow()));
                }
                if *attrs {
                    child_vnode.reapply_attrs(&child_el)?;
                }
                render_diff(&child_vnode, &*children, &child_el, doc)?;
            }
        }
    }
    Ok(())
}

pub enum Cmd<Msg> {
    None,
    Msg(Msg),
    Fetch(Box<Future<Item = Msg, Error = JsValue>>),
    NavigateAway(String),
    PushUrl(url::Url),
}

#[derive(Debug, Constructor)]
/// Represents an HTML node
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

/// Represents a listener attached to the DOM.
/// When it is dropped it will detach the corresponding listener.
pub struct Listener<M: Model> {
    element: Element,
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
    fn new(element: Element, type_: Str, closure: Closure<FnMut(DomEvent)>) -> Listener<M> {
        Listener {
            element,
            type_,
            closure,
            marker: std::marker::PhantomData,
        }
    }
}

fn event_handler<M: Model, S: Into<Str>, F: Fn(DomEvent) -> Cmd<M::Msg> + 'static>(
    element: Element,
    event_name: S,
    handler: F,
) -> JsResult<Listener<M>> {
    let event_name = event_name.into();
    log!("New event hander closure");
    let closure = Closure::wrap(Box::new(move |event: DomEvent| {
        web_sys::console::time();
        let cmd = handler(event);
        App::<M>::with(move |app| {
            app.loop_update(cmd).expect("Update error");
        });
        web_sys::console::time_end();
    }) as Box<FnMut(DomEvent)>);
    let jsfunction = closure.as_ref().unchecked_ref();
    element.add_event_listener_with_callback(&event_name, jsfunction)?;
    Ok(Listener::new(element, event_name, closure))
}

fn input_handler<M: Model>(
    element: &Element,
    handler: Rc<dyn Fn(String) -> M::Msg>,
) -> JsResult<Listener<M>> {
    let inner = move |event: DomEvent| {
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &HtmlElement = target.dyn_ref().expect("Not an Html Element");
        Cmd::Msg(handler(
            get_str_prop(target_el, "value").expect("missing value"),
        ))
    };
    event_handler::<M, _, _>(element.clone(), "input", inner)
}

fn click_handler<M: Model>(
    element: &Element,
    handler: Rc<dyn Fn() -> M::Msg>,
) -> JsResult<Listener<M>> {
    let inner = move |_event: DomEvent| Cmd::Msg(handler());
    event_handler::<M, _, _>(element.clone(), "click", inner)
}

fn attach_event_listener<M: Model>(event: &Event<M>, element: &Element) -> JsResult<Listener<M>> {
    match event {
        Event::OnClick { cb, .. } => click_handler::<M>(&element, cb.clone()),
        Event::OnInput { cb, .. } => input_handler::<M>(&element, cb.clone()),
    }
}

fn apply_attr_to_elem(attr: &Attribute, element: &Element) -> JsResult<()> {
    use Attribute::*;
    match attr {
        Class(classes) => element.set_attribute("class", &classes.join(" "))?,
        Href(val) => element.set_attribute("href", val)?,
        Id(val) => element.set_attribute("id", val)?,
        Placeholder(val) => element.set_attribute("placeholder", val)?,
        Style(style) => (),
        Value(val) => element.set_attribute("value", val)?,
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

    fn replace_events(&self, element: &Element) -> JsResult<Element> {
        log!("Replacing listeners");

        // https://stackoverflow.com/questions/4386300/javascript-dom-how-to-remove-all-events-of-a-dom-object
        // Stupid hack IMO
        let new_element = element.clone_node_with_deep(true)?;
        let new_element: Element = new_element.dyn_into()?;

        element
            .parent_node()
            .unwrap()
            .replace_child(&new_element, &element)?;
        for event in &self.events {
            let listener = attach_event_listener(event, &new_element)?;
            // TODO stop leaking the listener!
            Box::leak(Box::new(listener));
        }
        Ok(new_element)
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
