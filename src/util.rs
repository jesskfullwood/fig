use crate::{UrlRequest, JsValue, JsResult, Cmd};


pub fn get_session<T: serde::de::DeserializeOwned>() -> JsResult<T> {
    let window = web_sys::window()
        .ok_or_else(|| JsValue::from_str("No window"))?;
    let storage = window.session_storage()
                  .map(|sessopt| sessopt.ok_or_else(|| JsValue::from_str("No storage")))
        .and_then(|inner| inner)?;
    let itemstr = storage.get_item("session")
        .map(|itemopt| itemopt
             .ok_or_else(|| JsValue::from_str("session not found")))
        .and_then(|inner| inner)?;
    serde_json::from_str(&itemstr).map_err(|e| JsValue::from_str(&e.to_string()))
}

pub fn put_session<T: serde::Serialize>(item: T) -> JsResult<()> {
    let itemstr = serde_json::to_string(&item).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let window = web_sys::window()
        .ok_or_else(|| JsValue::from_str("No window"))?;
    let storage = window.session_storage()
        .map(|sessopt| sessopt.ok_or_else(|| JsValue::from_str("No storage")))
        .and_then(|inner| inner)?;
    storage.set_item("session", &itemstr)
}

/// Url request handler. Always forces a reload.
pub fn on_url_request_force_load<Msg>(req: UrlRequest) -> Cmd<Msg> {
    match req {
        UrlRequest::Internal(url) => Cmd::load_url(url.to_string()),
        UrlRequest::External(urlstr) => Cmd::load_url(urlstr),
    }
}

/// Default Url request handler. Pushes the url if internal, loads if external.
pub fn on_url_request_intercept<Msg>(req: UrlRequest) -> Cmd<Msg> {
    use UrlRequest::*;
    match req {
        Internal(url) => Cmd::push_url(url.to_string()),
        External(urlstr) => Cmd::load_url(urlstr),
    }
}