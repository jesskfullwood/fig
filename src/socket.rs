use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{ErrorEvent, MessageEvent, WebSocket};

use crate::{Cmd, Key, Model, Subscription};

struct Socket<M: Model> {
    uri: String,

    on_open: fn() -> Cmd<M::Msg>,
    on_message: fn(MessageEvent) -> Cmd<M::Msg>,
    on_error: fn(ErrorEvent) -> Cmd<M::Msg>,

    on_open_cb: Option<Closure<dyn FnMut()>>,
    on_message_cb: Option<Closure<dyn FnMut(MessageEvent)>>,
    on_error_cb: Option<Closure<dyn FnMut(ErrorEvent)>>,
}

impl<M: Model> PartialEq for Socket<M> {
    fn eq(&self, other: &Self) -> bool {
        self.uri == other.uri
            && self.on_open == other.on_open
            && self.on_message == other.on_message
            && self.on_error == other.on_error
    }
}

impl<M: Model> Socket<M> {
    pub fn start_websocket(&mut self, key: Key<M>) -> Result<(), JsValue> {
        let ws = WebSocket::new(&self.uri)?;

        // set open handler
        let onopen_callback = key.closure0(self.on_open);
        ws.set_onopen(Some(onopen_callback.as_ref().unchecked_ref()));
        self.on_open_cb = Some(onopen_callback);

        // set event handler
        let onmessage_callback = key.closure1(self.on_message);
        ws.set_onmessage(Some(onmessage_callback.as_ref().unchecked_ref()));
        self.on_message_cb = Some(onmessage_callback);

        // set error handler
        let onerror_callback = key.closure1(self.on_error);
        ws.set_onerror(Some(onerror_callback.as_ref().unchecked_ref()));

        let cloned_ws = ws.clone();
        Ok(())
    }
}

impl<M: Model> Subscription<M> for Socket<M> {
    fn subscribe(&mut self, key: Key<M>) {}
    fn sub_eq(&self, other: &dyn Subscription<M>) -> bool {
        if let Some(o) = other.downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}
