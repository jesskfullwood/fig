use std::collections::hash_map::DefaultHasher;
use std::fmt::{self, Debug};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use derive_more::Display;
use wasm_bindgen::{closure::Closure, JsCast};
use web_sys::{Element as DomElement, Event as DomEvent, HtmlElement};

use crate::util;
use crate::{App, Cmd, JsResult, Model, Str};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
pub(crate) struct EventId(u64);

pub struct Event<M: Model> {
    id: EventId,
    pub(crate) inner: EventInner<M>,
}

pub(crate) enum EventInner<M: Model> {
    OnClick(Rc<dyn Fn() -> M::Msg>),
    OnInput(Rc<dyn Fn(String) -> M::Msg>),
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

impl<M: Model> Eq for Event<M> {}

impl<M: Model> PartialOrd for Event<M> {
    fn partial_cmp(&self, other: &Event<M>) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl<M: Model> Ord for Event<M> {
    fn cmp(&self, other: &Event<M>) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<M: Model> Event<M> {
    pub(crate) fn id(&self) -> EventId {
        self.id
    }

    fn click<S: Hash + 'static>(s: S, f: fn(s: &S) -> M::Msg) -> Event<M> {
        // Can't hash fn ptr - compiler bug! Do very unsafe workaround
        // https://github.com/rust-lang/rust/issues/46989
        let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
        let hash = hash_closure(&s, ptr);
        Event {
            id: EventId(hash),
            inner: EventInner::OnClick(Rc::new(move || f(&s))),
        }
    }

    pub fn input<S: Hash + 'static>(s: S, f: fn(&S, String) -> M::Msg) -> Event<M> {
        let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
        let hash = hash_closure(&s, ptr);
        Event {
            id: EventId(hash),
            inner: EventInner::OnInput(Rc::new(move |val| f(&s, val))),
        }
    }
}

fn hash_closure<S: Hash, F: Hash>(s: S, f: F) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    f.hash(&mut hasher);
    hasher.finish()
}

pub fn on_click<M: Model, S: Hash + 'static>(s: S, f: fn(s: &S) -> M::Msg) -> Event<M> {
    Event::click(s, f)
}

pub fn on_input<M: Model, S: Hash + 'static>(s: S, f: fn(&S, String) -> M::Msg) -> Event<M> {
    Event::input(s, f)
}

/// Represents a listener attached to the DOM.
/// When it is dropped it will detach the corresponding listener.
pub(crate) struct Listener<M: Model> {
    element: DomElement,
    type_: Str,
    closure: Closure<dyn FnMut(DomEvent)>,
    marker: std::marker::PhantomData<M>,
}

impl<M: Model> Drop for Listener<M> {
    fn drop(&mut self) {
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
    fn new(element: DomElement, type_: Str, closure: Closure<dyn FnMut(DomEvent)>) -> Listener<M> {
        Listener {
            element,
            type_,
            closure,
            marker: std::marker::PhantomData,
        }
    }
}

pub(crate) fn closure0<M: Model, F: FnMut() -> Cmd<M::Msg> + 'static>(
    mut handler: F,
) -> Closure<dyn FnMut()> {
    Closure::wrap(Box::new(move || {
        web_sys::console::time();
        let cmd = handler();
        App::<M>::with(move |app| {
            app.loop_update(cmd).expect("Update error");
        });
        web_sys::console::time_end();
    }) as Box<dyn FnMut()>)
}

pub(crate) fn closure1<M: Model, T, F: FnMut(T) -> Cmd<M::Msg> + 'static>(
    mut handler: F,
) -> Closure<dyn FnMut(T)>
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
    }) as Box<dyn FnMut(T)>)
}

pub(crate) fn event_handler<M: Model, S: Into<Str>, F: Fn(DomEvent) -> Cmd<M::Msg> + 'static>(
    element: DomElement,
    event_name: S,
    handler: F,
) -> JsResult<Listener<M>> {
    let event_name = event_name.into();
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
            util::get_str_prop(target_el, "value").expect("missing value"),
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

pub(crate) fn attach_event_listener<M: Model>(
    event: &Event<M>,
    element: &DomElement,
) -> JsResult<Listener<M>> {
    match &event.inner {
        EventInner::OnClick(cb) => click_handler::<M>(&element, cb.clone()),
        EventInner::OnInput(cb) => input_handler::<M>(&element, cb.clone()),
    }
}
