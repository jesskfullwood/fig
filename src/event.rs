use crate::Model;

use derive_more::Display;

use std::collections::hash_map::DefaultHasher;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Display)]
struct ClosureId(u64);

pub struct Event<M: Model> {
    id: ClosureId,
    pub(crate) inner: EventInner<M>,
}

pub(crate) enum EventInner<M: Model> {
    OnClick(Rc<Fn() -> M::Msg>),
    OnInput(Rc<Fn(String) -> M::Msg>),
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

fn hash_closure<S: Hash, F: Hash>(s: S, f: F) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    f.hash(&mut hasher);
    hasher.finish()
}

pub fn on_click<M: Model, S: Hash + 'static>(s: S, f: fn(s: &S) -> M::Msg) -> Event<M> {
    // Can't hash fn ptr - compiler bug! Do very unsafe workaround
    // https://github.com/rust-lang/rust/issues/46989
    let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
    let hash = hash_closure(&s, ptr);
    Event {
        id: ClosureId(hash),
        inner: EventInner::OnClick(Rc::new(move || f(&s))),
    }
}

pub fn on_input<M: Model, S: Hash + 'static>(s: S, f: fn(&S, String) -> M::Msg) -> Event<M> {
    // Can't hash fn ptr - compiler bug! Do very unsafe workaround
    // https://github.com/rust-lang/rust/issues/46989
    let ptr = unsafe { std::mem::transmute::<_, usize>(f) };
    let hash = hash_closure(&s, ptr);
    Event {
        id: ClosureId(hash),
        inner: EventInner::OnInput(Rc::new(move |val| f(&s, val))),
    }
}
