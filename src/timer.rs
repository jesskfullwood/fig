use std::fmt::{self, Debug};

use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;

use crate::{Cmd, Key, Model, Subscription};

pub struct Timer<M: Model> {
    callback_id: Option<i32>,
    interval_ms: u32,
    // TODO make a proper closure type?
    trigger: fn() -> Cmd<M::Msg>,
    callback: Option<Closure<dyn FnMut()>>,
}

impl<M: Model + Debug> Debug for Timer<M> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Timer {{ interval_ms: {} }}", self.interval_ms)
    }
}

impl<M: Model> Timer<M> {
    pub fn new(interval_ms: u32, trigger: fn() -> Cmd<M::Msg>) -> Timer<M> {
        Timer {
            callback_id: None,
            callback: None,
            interval_ms,
            trigger,
        }
    }
}

impl<M: Model> PartialEq for Timer<M> {
    fn eq(&self, other: &Self) -> bool {
        self.interval_ms == other.interval_ms && self.trigger == other.trigger
    }
}

impl<M: Model> Subscription<M> for Timer<M> {
    fn subscribe(&mut self, key: Key<M>) {
        let cb = key.closure0(self.trigger);
        let jsfunction = cb.as_ref().unchecked_ref();
        let window = web_sys::window().expect("No global `window` exists");
        match window.set_interval_with_callback_and_timeout_and_arguments_0(
            jsfunction,
            self.interval_ms as i32,
        ) {
            Ok(id) => self.callback_id = Some(id),
            Err(e) => panic!("{:?}", e),
        };
        self.callback = Some(cb);
    }
    fn sub_eq(&self, other: &dyn Subscription<M>) -> bool {
        if let Some(o) = other.downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

impl<M: Model> Drop for Timer<M> {
    fn drop(&mut self) {
        // if there is no callback, we assume the timer was never attached
        if let Some(_) = self.callback {
            web_sys::window()
                .map(|window| {
                    window.clear_interval_with_handle(self.callback_id.expect("No timer id"));
                })
                .unwrap_or(())
        }
    }
}
