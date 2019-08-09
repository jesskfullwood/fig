//! A web-framework which prioritizes correctness and type-safety.
//!
//! Based closely upon The [Elm](https://elm-lang.org/) Architecture.
//!

#[macro_use]
extern crate log;

use derive_more::{Constructor, From};
use downcast_rs::{impl_downcast, Downcast};
use futures::Future;
use js_sys::Function;
use wasm_bindgen::{closure::Closure, JsCast};
use web_sys::{
    Document, Element as DomElement, Event as DomEvent, HtmlDivElement, Location, Node,
    PopStateEvent, Text, Window,
};

use std::borrow::{BorrowMut, Cow};
use std::collections::{BTreeSet, HashMap};
use std::fmt::{self, Debug};
use std::marker::PhantomData;

use event::{Event, EventId, Listener};
use html::{Attribute, Tag};

pub mod event;
pub mod fetch;
pub mod html;
pub mod program;
pub mod socket;
pub mod timer;
pub mod util;

pub use event::{on_click, on_input};
pub use program::run;
pub use url::Url;

pub use wasm_bindgen::JsValue;

type JsResult<T> = Result<T, JsValue>;

/// A trait to associate a Model with a Msg type.
///
/// Necessary to ensure a Model is used with the corrent Msg and vice-versa
// TODO is it actually necessary?
pub trait Model: 'static + Sized + Debug {
    type Msg;

    fn view(&self) -> Html<Self>;
    fn update(&mut self, msg: Self::Msg) -> Cmd<Self::Msg>;

    fn subscribe(&self) -> Sub<Self> {
        Sub::none()
    }

    fn init(&mut self, url: url::Url) -> Cmd<Self::Msg> {
        Self::on_url_change(url)
    }
    fn on_url_request(req: UrlRequest) -> Cmd<Self::Msg> {
        util::on_url_request_intercept(req)
    }
    #[allow(unused_variables)]
    fn on_url_change(url: url::Url) -> Cmd<Self::Msg> {
        Cmd::none()
    }
}

// This impl is so we can do quick examples and tests for Html layout.
// It might be best only to activate for tests
// (but see https://github.com/rust-lang/rust/issues/45599)
impl Model for () {
    type Msg = ();
    fn view(&self) -> Html<Self> {
        div![]
    }
    fn update(&mut self, _: ()) -> Cmd<()> {
        Cmd::none()
    }
}

// Convenience alias
type Str = Cow<'static, str>;

/// The core application.
///
/// At page load, the user create calls `application(..)` or similar, which creates an App
/// in a thread-local. Unfortunately we have to use `unsafe` and stash it as a void pointer,
/// because the App is generic over the Model and this cannot be expressed in safe Rust.
///
/// The app kicks off an initial page render. Thereafter, any page event will call an internal function
/// which casts the App back from the void pointer and call `app.update(msg)`, creating a new
/// Model and forcing another page render. This is why all functions need to be tagged with the
/// Model trait, so we know which Model to cast too!
///
/// Any side-effect (e.g. fetching url, updating a model value) is handled ONLY through
/// passing a `Cmd` to the `update` function. This ensures the functional reactive loop
/// (command -> update -> view -> command...) is never broken
///
/// For simplicity and safety, we keep App hidden from the user at all times.
struct App<M: Model> {
    window: Window,
    target: HtmlDivElement,
    model: M,
    current_vdom: Html<M>,
    listeners: HashMap<EventId, (usize, Vec<Listener<M>>)>,
    subscriptions: Vec<Box<dyn Subscription<M>>>,
}

thread_local! {
    static APP: *mut u8 = std::ptr::null_mut();
}

impl<M: Model> Drop for App<M> {
    fn drop(&mut self) {
        error!("Dropping app! This is an error")
    }
}

impl<M: Model + Debug> Debug for App<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "App (model: {:?})", self.model)
    }
}

impl<M: Model> App<M> {
    /// Update the model with the given Cmd
    ///
    /// Each command may trigger another command, and we do not want to render
    /// each time, so we call update in an infinite loop and explicitly break
    /// when we eventually receive a `None` command.
    ///
    /// After breaking the loop, we re-render the DOM
    fn loop_update(&mut self, Cmd(mut cmd): Cmd<M::Msg>) -> JsResult<()> {
        let mut loopct = 0;
        loop {
            loopct += 1;
            match cmd {
                CmdInner::None => break,
                // return without rendering. Generally need a Very Good Reason for this
                CmdInner::NoOp => return Ok(()),
                CmdInner::Msg(msg) => {
                    let Cmd(new_cmd) = self.model.update(msg);
                    cmd = new_cmd; // we go again
                }
                CmdInner::Multiple(cmds) => {
                    for cmd in cmds {
                        // TODO hmm, this will actually re-render after each command
                        // not sure if we the effort to avoid it
                        self.loop_update(cmd)?
                    }
                    break;
                }
                CmdInner::Spawn(request) => {
                    let fut = request.then(|res: Result<Cmd<M::Msg>, Cmd<M::Msg>>| {
                        let cmd = match res {
                            Ok(ok) => ok,
                            Err(e) => e,
                        };
                        App::<M>::with(|app| app.loop_update(cmd).expect("update failed"));
                        Ok(())
                    });
                    wasm_bindgen_futures::spawn_local(fut);
                    break;
                }
                CmdInner::LoadUrl(urlstr) => {
                    // Navigating away from the page
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
                    // and go round again
                    cmd = M::on_url_change(url).0;
                }
            }
            if loopct > 100 {
                panic!("Infinite loop!")
            }
        }
        trace!("Update subscriptions");
        self.update_subscriptions();
        // Don't render the new dom until we finish looping
        trace!("Update vdom");
        self.current_vdom = self.render_dom()?;
        trace!("Registered events: {}", self.listeners.len());
        // returns that it did rerender
        Ok(())
    }

    fn update_subscriptions(&mut self) {
        // TODO I don't think this function is very elegant

        let poss_new_subs = self.model.subscribe();
        let mut new_subs = Vec::new();
        let mut seen_subs = bit_set::BitSet::new();

        // check the possibly-new subs against existing ones
        'outer: for mut nsub in poss_new_subs.0.into_iter() {
            for (ix, sub) in self.subscriptions.iter().enumerate() {
                if nsub.sub_eq(&**sub) {
                    // we are already subscribed to nsub, make a note
                    seen_subs.insert(ix);
                    continue 'outer;
                }
            }
            // Fell through above loop, so this is a new subscription.
            trace!("New subscription");
            nsub.subscribe(Key::new());
            new_subs.push(nsub);
        }
        // Remove defunct subs
        for ix in (0..self.subscriptions.len()).rev() {
            // If we didn't identify an existing sub in poss_new_subs, then the subscription
            // is defunct - remove it
            if !seen_subs.contains(ix) {
                trace!("Removing subscription");
                self.subscriptions.remove(ix);
            }
        }
        self.subscriptions.extend(new_subs);
    }

    fn render_dom(&self) -> JsResult<Html<M>> {
        let new_vdom = self.model.view();
        let diff = diff_vdom(&self.current_vdom, &new_vdom);
        if let Diff::Unchanged = diff {
            trace!("No change");
        } else {
            trace!("vdom diff: {:?}", diff);
            let document = self.window.document().expect("No document");
            render_diff(&self.target, &[(0, diff)], &document)?;
        }
        Ok(new_vdom)
    }

    /// Run a function with the App as an argument. This involves unsafely casting from a
    /// void pointer stashed in a thread-local!
    fn with<R>(f: impl FnOnce(&mut Self) -> R) -> R {
        APP.with(|mut ptr| {
            let ptr: *mut u8 = **ptr.borrow_mut();
            let ptr: *mut App<M> = ptr as *mut App<M>;
            unsafe { f(&mut *ptr) }
        })
    }

    /// Update the browser url
    fn push_state(&self, url: &str) -> JsResult<()> {
        let history = self.window.history().expect("No history");
        history.push_state_with_url(&JsValue::NULL, "", Some(&url))
    }

    /// Fetch the browser url
    fn location(&self) -> JsResult<url::Url> {
        let urlstr = self.window.location().href()?;
        url::Url::parse(&urlstr).map_err(|e| JsValue::from_str(&e.to_string()))
    }

    fn stash_event_listener(&mut self, id: EventId, listener: Listener<M>) {
        // Here we wish to stash the Listeners until the corresponding events are removed from the dom.
        // However, the EventId is not guaranteed to be unique because it is possible for the "same"
        // closure to exist on the page in several places at once
        // (e.g. a logout button) SO we maintain a refcount the ensure we do not
        // free it before we should. This is a bit of a hack, because we are forced to
        // keep all listeners of the same EventId alive until none of them are needed any more
        // XXX This is a potential memory leak.
        let mut entry = self.listeners.entry(id).or_insert((0, Vec::new()));
        entry.0 += 1; // increment refct
        entry.1.push(listener);
    }

    fn remove_event_listener(&mut self, id: &EventId) {
        if let Some(entry) = self.listeners.get_mut(id) {
            entry.0 -= 1;
            if entry.0 == 0 {
                self.listeners.remove(id);
            }
        }
    }

    /// Set the handler that will trigger a Route change when `window.popstate`
    /// event is fired, e.g by the `back` button
    fn set_popstate_handler(&self) {
        let handler = move |_e: PopStateEvent| {
            // Event state is just some state that was associated with the current url
            // when pushState was called. Since we don't use it, it will be `null`
            // We just want to force a page update with the new Route
            App::<M>::with(|app| {
                let url = app.location().expect("No location");
                M::on_url_change(url)
            })
        };
        let cb = crate::event::closure1::<M, _, _>(handler);
        let onpop: &Function = cb.as_ref().unchecked_ref();
        self.window.set_onpopstate(Some(onpop));
        Box::leak(Box::new(cb));
    }
}

/// A token which grants permission to use various library features
pub struct Key<M: Model>(PhantomData<M>);

impl<M: Model> Key<M> {
    fn new() -> Key<M> {
        Key(PhantomData)
    }

    /// Take a zero-argument callback and hook it into the main event loop
    pub fn closure0<F: FnMut() -> Cmd<M::Msg> + 'static>(
        &self,
        handler: F,
    ) -> Closure<dyn FnMut()> {
        event::closure0::<M, F>(handler)
    }

    /// Take a single-argument callback and hook it into the main event loop
    pub fn closure1<T, F: FnMut(T) -> Cmd<M::Msg> + 'static>(
        &self,
        handler: F,
    ) -> Closure<dyn FnMut(T)>
    where
        T: wasm_bindgen::convert::FromWasmAbi + 'static,
    {
        event::closure1::<M, T, F>(handler)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UrlRequest {
    Internal(url::Url),
    External(String),
}

// Intercept link clicks and handle them ourselves.
// Requires us to set a global listener for clicks.
fn set_link_click_handler<M: Model, F: Fn(UrlRequest) -> Cmd<M::Msg> + 'static>(
    location: Location,
    root: DomElement,
    handler: F,
) -> JsResult<Listener<M>> {
    let cb = move |event: DomEvent| -> Cmd<M::Msg> {
        // Was it an anchor tag that was clicked?
        let target: web_sys::EventTarget = event.target().expect("Missing target");
        let target_el: &DomElement = target.dyn_ref().expect("Not an Element");
        if target_el.tag_name() != "A" {
            // if not, do nothing
            return Cmd(CmdInner::NoOp);
        }
        // if so, first stop default (navigating away)
        event.prevent_default();
        let target_el: &web_sys::HtmlAnchorElement = target_el.dyn_ref().expect("Not an anchor");
        // TODO no need for reflection here
        let urlstr = util::get_str_prop(target_el, "href").expect("No href");
        let ahost = util::get_str_prop(target_el, "host").expect("No anchor host");
        let req = if ahost == location.host().expect("No location host") {
            // Then parse the url
            let url = url::Url::parse(&urlstr)
                .map_err(|e| JsValue::from_str(&e.to_string()))
                .expect("Url parse failed");
            UrlRequest::Internal(url)
        } else {
            UrlRequest::External(urlstr)
        };
        // and send to the handler
        handler(req)
    };
    // set the handler on all clicks, on the given (root) node
    event::attach_event_handler(root, "click", cb)
}

#[derive(Clone, Debug)]
pub enum Delta<T> {
    Add(T),
    Remove(T),
}

/// A fig describing which nodes have changed and how
#[derive(Clone)]
enum Diff<'a, M: Model> {
    Insert(&'a Html<M>),
    Replace {
        with: &'a Html<M>,
        events_to_rm: Vec<EventId>,
    },
    Remove {
        events_to_rm: Vec<EventId>,
    },
    Update {
        attrs: Vec<Delta<&'a Attribute>>,
        events: Vec<Delta<&'a Event<M>>>,
        children: Vec<(u32, Diff<'a, M>)>,
    },
    Unchanged,
}

impl<'a, M: Model> Debug for Diff<'a, M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Diff::*;
        let txt = match self {
            Insert(_) => "Insert",
            Replace { .. } => "Replace",
            Remove { .. } => "Remove",
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

fn diff_vdom<'a, M: Model>(old: &'a Html<M>, new: &'a Html<M>) -> Diff<'a, M> {
    let (old_el, new_el) = match (old, new) {
        (Html::Text(t1), Html::Text(t2)) => {
            return if t1 == t2 {
                Diff::Unchanged
            } else {
                Diff::Replace {
                    with: new,
                    events_to_rm: Vec::new(),
                }
            }
        }
        (Html::Text(_), Html::Element(_)) => {
            return Diff::Replace {
                with: new,
                events_to_rm: Vec::new(),
            }
        }
        (Html::Element(_), Html::Text(_)) => {
            return Diff::Replace {
                with: new,
                events_to_rm: old.get_nested_event_ids(),
            }
        }
        (Html::Element(e1), Html::Element(e2)) => (e1, e2),
    };

    if old_el.tag != new_el.tag {
        // assume everything can be nuked
        return Diff::Replace {
            with: new,
            events_to_rm: old.get_nested_event_ids(),
        };
    }

    let attrs = if old_el.attrs == new_el.attrs {
        Vec::new()
    } else {
        let mut deltas = Vec::new();
        let oldset: BTreeSet<&Attribute> = old_el.attrs.iter().collect();
        let newset: BTreeSet<&Attribute> = new_el.attrs.iter().collect();
        for &attr in oldset.difference(&newset) {
            deltas.push(Delta::Remove(attr))
        }
        for &attr in newset.difference(&oldset) {
            deltas.push(Delta::Add(attr))
        }
        deltas
    };
    let events = if old_el.events == new_el.events {
        Vec::new()
    } else {
        let mut deltas = Vec::new();
        let oldset: BTreeSet<&Event<M>> = old_el.events.iter().collect();
        let newset: BTreeSet<&Event<M>> = new_el.events.iter().collect();
        for &attr in oldset.difference(&newset) {
            deltas.push(Delta::Remove(attr))
        }
        for &attr in newset.difference(&oldset) {
            deltas.push(Delta::Add(attr))
        }
        deltas
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
            child_diffs.push((ix as u32, diff))
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
            let events_to_rm = old_el.children[ix].get_nested_event_ids();
            child_diffs.push((ix as u32, Diff::Remove { events_to_rm }))
        }
    }

    child_diffs.sort_by_key(|t| t.0);
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
    let mut rmct = 0;
    for &(ix, ref diff) in child_diffs.iter() {
        let ix = ix - rmct; // adjust index for previously-removed nodes
        match diff {
            Diff::Unchanged => (),
            Diff::Insert(node) => {
                // Is there already a node at this index?
                let new_el = node.render_to_html(doc)?;
                // XXX insert child, not append!
                this_el.append_child(&new_el)?;
            }
            Diff::Replace {
                with: node,
                events_to_rm,
            } => {
                for event_id in events_to_rm {
                    App::<M>::with(|app| app.remove_event_listener(event_id));
                }
                let old_el = child_els.get(ix).expect("bad replace node index");
                let new_el = node.render_to_html(doc)?;
                this_el.replace_child(&new_el, &old_el)?;
            }
            Diff::Remove { events_to_rm } => {
                for event_id in events_to_rm {
                    App::<M>::with(|app| app.remove_event_listener(event_id));
                }
                let old_el = child_els.get(ix).expect("bad remove node index");
                this_el.remove_child(&old_el)?;
                rmct += 1;
            }
            Diff::Update {
                attrs,
                events,
                children,
            } => {
                let child_el: Node = child_els.get(ix).expect("bad node index");
                if !events.is_empty() {
                    let el: &DomElement = child_el.dyn_ref().expect("Not an element");
                    update_events(&el, &events)?;
                }
                if !attrs.is_empty() {
                    let el: &DomElement = child_el.dyn_ref().expect("Not an element");
                    update_attrs(&el, &attrs)?;
                }
                render_diff(&child_el, &*children, doc)?;
            }
        }
    }
    Ok(())
}

/// An event loop command.
///
/// Can cause various side effects (e.g. `fetch` requests, access local storage).
/// See the various constructors for further explanation
pub struct Cmd<Msg>(CmdInner<Msg>);

enum CmdInner<Msg> {
    None,
    /// Indicates that no work should be done (no diffing or rendering)
    NoOp,
    Msg(Msg),
    Multiple(Vec<Cmd<Msg>>),
    Spawn(Box<dyn Future<Item = Cmd<Msg>, Error = Cmd<Msg>>>),
    LoadUrl(Str),
    PushUrl(Str),
}

impl<Msg> Cmd<Msg> {
    /// Do nothing
    pub fn none() -> Self {
        Cmd(CmdInner::None)
    }

    /// Send a message to the `update` function. The page is rendered after all messages
    /// in a chain have been run.
    ///
    /// Care should be taken not to create an infinite chain of messages as this will
    /// effectively block the event loop
    pub fn msg(msg: Msg) -> Self {
        Cmd(CmdInner::Msg(msg))
    }

    /// Run multiple commands. The commands are run in turn and the page is
    /// rendered after each has completed.
    ///
    /// This command is useful for spawning multiple futures at once
    pub fn multiple(msgs: impl IntoIterator<Item = Cmd<Msg>>) -> Self {
        Cmd(CmdInner::Multiple(msgs.into_iter().collect()))
    }

    /// Spawn a future. When the future resolves, the message will be run in the
    /// event loop.
    pub fn spawn(fut: impl Future<Item = Cmd<Msg>, Error = Cmd<Msg>> + 'static) -> Self {
        Cmd(CmdInner::Spawn(Box::new(fut)))
    }

    // TODO require a Key to change the url
    /// Change the page url to the supplied path
    pub fn push_url(s: impl std::fmt::Display) -> Self {
        Cmd(CmdInner::PushUrl(format!("{}", s).into()))
    }

    // TODO require a Key to load the url
    /// Load the supplied url. This forces a page reload (destroying the current app).
    pub fn load_url(s: impl Into<Str>) -> Self {
        Cmd(CmdInner::LoadUrl(s.into()))
    }
}

impl<T> From<T> for Cmd<T> {
    fn from(t: T) -> Self {
        Cmd::msg(t)
    }
}

pub struct Sub<M: Model>(Vec<Box<dyn Subscription<M>>>);

impl<M: Model> Sub<M> {
    pub fn none() -> Self {
        Sub(Vec::new())
    }

    pub fn new(ss: Vec<Box<dyn Subscription<M>>>) -> Self {
        Sub(ss)
    }
}

pub trait Subscription<M: Model>: Downcast {
    fn subscribe(&mut self, key: Key<M>);
    fn sub_eq(&self, other: &dyn Subscription<M>) -> bool;
}

impl_downcast!(Subscription<M> where M: Model);

// See https://developer.mozilla.org/en-US/docs/Web/API/Node/nodeType
/// Represents a HTML DOM Node
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

    /// Find all events attached the current node and all child nodes. This is useful
    /// because we have to clean up the events 'manually' when a node is removed
    fn get_nested_event_ids(&self) -> Vec<EventId> {
        let mut events_to_rm = Vec::new();
        self.recursively_gather_event_ids(&mut events_to_rm);
        events_to_rm
    }

    fn recursively_gather_event_ids(&self, ids: &mut Vec<EventId>) {
        match &self {
            Html::Text(_) => (),
            Html::Element(elem) => {
                for ev in &elem.events {
                    ids.push(ev.id());
                }
                for c in &elem.children {
                    c.recursively_gather_event_ids(ids)
                }
            }
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

impl<M: Model> Element<M> {
    fn add_attrs(&self, element: &DomElement) -> JsResult<()> {
        for attr in &self.attrs {
            add_attr_to_element(attr, element)?
        }
        Ok(())
    }

    fn render_to_html(&self, document: &Document) -> JsResult<DomElement> {
        let element = document.create_element(&self.tag.to_string())?;
        self.add_attrs(&element)?;
        for event in &self.events {
            let listener = event::attach_event_listener(event, &element)?;
            App::<M>::with(|app| app.stash_event_listener(event.id(), listener));
        }
        for child in &self.children {
            let child_elem = child.render_to_html(document)?;
            element.append_child(&child_elem)?;
        }
        Ok(element)
    }
}

fn add_attr_to_element(attr: &Attribute, element: &DomElement) -> JsResult<()> {
    let key = attr.key();
    let val = attr.value();
    element.set_attribute(key, &val)
}

fn remove_attr_from_element(attr: &Attribute, element: &DomElement) -> JsResult<()> {
    let key = attr.key();
    element.remove_attribute(key)
}

fn update_events<M: Model>(element: &DomElement, events: &[Delta<&Event<M>>]) -> JsResult<()> {
    for delta in events {
        match delta {
            Delta::Add(event) => {
                let listener = event::attach_event_listener(event, &element)?;
                App::<M>::with(|app| app.stash_event_listener(event.id(), listener));
            }
            Delta::Remove(event) => {
                // When the listener is dropped it is automatically removed from the DOM
                App::<M>::with(|app| app.remove_event_listener(&event.id()));
            }
        }
    }
    Ok(())
}

fn update_attrs(element: &DomElement, attrs: &[Delta<&Attribute>]) -> JsResult<()> {
    for delta in attrs {
        match delta {
            Delta::Add(attr) => add_attr_to_element(attr, element)?,
            Delta::Remove(attr) => remove_attr_from_element(attr, element)?,
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_erased_eq() {
        // ErasedEq experiment
        // TODO finish or remove
        #[doc(hidden)]
        pub trait ErasedEq: Downcast {
            fn erased_eq(&self, other: &dyn ErasedEq) -> bool;
        }

        impl_downcast!(ErasedEq);

        impl<T> ErasedEq for T
        where
            T: PartialEq + Sized + 'static,
        {
            fn erased_eq(&self, other: &dyn ErasedEq) -> bool {
                if let Some(o) = other.downcast_ref::<Self>() {
                    self == o
                } else {
                    false
                }
            }
        }

        #[derive(PartialEq)]
        struct A(&'static str);
        #[derive(PartialEq)]
        struct B;

        trait T: ErasedEq {}
        impl T for A {}
        impl T for B {}
        let a1 = A("Hi");
        let a2 = A("Hi");
        let a3 = A("Bye");
        let b = B;

        assert!(a1.erased_eq(&a1));
        assert!(a1.erased_eq(&a2));

        assert!(!a1.erased_eq(&a3));
        assert!(!a1.erased_eq(&b));
        assert!(!b.erased_eq(&a1));

        // FIXME problem! doesn't work once boxed as traits objs
        let t1: Box<ErasedEq> = Box::new(a1);
        let t2: Box<ErasedEq> = Box::new(a2);
        let t3: Box<ErasedEq> = Box::new(b);
        assert!(t1.erased_eq(&*t2));
        assert!(!t1.erased_eq(&*t3));
    }
}
