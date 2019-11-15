use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{CloseEvent, ErrorEvent, MessageEvent, WebSocket};

use crate::{Cmd, Key, Model, Subscription};

// TODO how to make duplex??
// Perhaps upon connect we should send a handle to the update
pub struct Socket<M: Model> {
    uri: String,

    on_open: fn(handle: Handle) -> Cmd<M::Msg>,
    on_message: fn(msg: Message) -> Cmd<M::Msg>,
    on_error: fn(err: ConnectionError) -> Cmd<M::Msg>,
    on_close: fn(closed: Closed) -> Cmd<M::Msg>,

    on_open_cb: Option<Closure<dyn FnMut()>>,
    on_message_cb: Option<Closure<dyn FnMut(MessageEvent)>>,
    on_error_cb: Option<Closure<dyn FnMut(ErrorEvent)>>,
    on_close_cb: Option<Closure<dyn FnMut(CloseEvent)>>,

    inner: Option<WebSocket>,
}

impl<M: Model> Socket<M> {
    pub fn new<S: Into<String>>(
        uri: S,
        on_open: fn(Handle) -> Cmd<M::Msg>,
        on_message: fn(Message) -> Cmd<M::Msg>,
        on_error: fn(ConnectionError) -> Cmd<M::Msg>,
        on_close: fn(Closed) -> Cmd<M::Msg>,
    ) -> Socket<M> {
        Socket {
            uri: uri.into(),
            on_open,
            on_message,
            on_error,
            on_close,
            on_open_cb: None,
            on_message_cb: None,
            on_error_cb: None,
            on_close_cb: None,
            inner: None,
        }
    }
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
        let handle = Handle(ws.clone());

        // set open handler
        let on_open = self.on_open.clone();
        let onopen_callback = key.closure0(move || on_open(handle.clone()));
        ws.set_onopen(Some(onopen_callback.as_ref().unchecked_ref()));
        self.on_open_cb = Some(onopen_callback);

        // set message handler
        let on_message = self.on_message.clone();
        let onmessage_callback = key.closure1(move |ev: MessageEvent| {
            // TODO can't assume Text here
            // TODO Deserialize JSON
            let json = ev.data().as_string().unwrap();
            on_message(Message::Text(json))
        });
        ws.set_onmessage(Some(onmessage_callback.as_ref().unchecked_ref()));
        self.on_message_cb = Some(onmessage_callback);

        // set error handler
        let on_error = self.on_error.clone();
        let onerror_callback = key.closure1(move |_| on_error(ConnectionError));
        ws.set_onerror(Some(onerror_callback.as_ref().unchecked_ref()));
        self.on_error_cb = Some(onerror_callback);

        // set close handler
        let on_close = self.on_close.clone();
        let onclose_callback = key.closure1(move |_| on_close(Closed));
        ws.set_onclose(Some(onclose_callback.as_ref().unchecked_ref()));
        self.on_close_cb = Some(onclose_callback);

        self.inner = Some(ws);
        Ok(())
    }
}

impl<M: Model> Subscription<M> for Socket<M> {
    fn subscribe(&mut self, key: Key<M>) {
        self.start_websocket(key).expect("Failed to start")
    }
    fn sub_eq(&self, other: &dyn Subscription<M>) -> bool {
        if let Some(o) = other.downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

impl<M: Model> Drop for Socket<M> {
    fn drop(&mut self) {
        if let Some(ws) = &self.inner {
            ws.close().expect("Socket failed to close")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Message {
    Text(String),
    Blob(Vec<u8>),
}

#[derive(Debug, Clone)]
pub struct ConnectionError;

#[derive(Debug, Clone)]
pub struct Closed;

/// The 'send' half of the socket.
#[derive(Debug, Clone)]
pub struct Handle(WebSocket);
