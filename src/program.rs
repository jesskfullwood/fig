use crate::{
    intercept_links, App, Cmd, Element, Html, JsResult, JsValue, Key, Model, Tag, UrlRequest, APP,
};

use wasm_bindgen::JsCast;
use web_sys::{Element as DomElement, HtmlDivElement};

use std::collections::HashMap;

/// Run a sandboxed application, ignoring HTTP and routing
pub fn sandbox<M: Model>(
    init: M,
    view: impl Fn(&M) -> Html<M> + 'static,
    update: impl Fn(M::Msg, M) -> M + 'static,
    target: &str,
) -> JsResult<()> {
    application(
        |_, _| (init, Cmd::none()),
        view,
        move |msg, model| (update(msg, model), Cmd::none()),
        // on url change, just force a load
        crate::util::on_url_request_force_load,
        // no way to create Cmd::push_url so no way to
        // trigger the url_changed handler
        |_| unreachable!(),
        target,
    )
}

/// Run a single-page application, including routing and HTTP requests
pub fn application<M: Model>(
    init: impl FnOnce(Key, url::Url) -> (M, Cmd<M::Msg>),
    view: impl Fn(&M) -> Html<M> + 'static,
    update: impl Fn(M::Msg, M) -> (M, Cmd<M::Msg>) + 'static,
    on_url_request: impl Fn(UrlRequest) -> Cmd<M::Msg> + 'static,
    on_url_change: impl Fn(url::Url) -> Cmd<M::Msg> + 'static,
    target: &str,
) -> JsResult<()> {
    console_error_panic_hook::set_once();
    let window = web_sys::window().expect("No global `window` exists");
    let document = window.document().expect("No document");
    let location = document.location().expect("No location");
    let url = url::Url::parse(&location.href()?).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let target = document
        .get_element_by_id(target)
        .expect("Target element not found");
    let target: HtmlDivElement = target.dyn_into()?;
    let downcast_cpy: DomElement = target.clone().dyn_into().unwrap();
    let initial = document.create_element(&Tag::Div.to_string())?;
    target.set_inner_html(""); // blank the target div and create an initial root
    target.append_child(&*initial)?;

    let app = App {
        window,
        target,
        model: None,
        update: Box::new(update),
        view: Box::new(view),
        on_url_change: Box::new(on_url_change),
        current_vdom: Html::from(Element::tag(Tag::Div)), // now the dom and vdom are in sync
        listeners: HashMap::new(),
    };

    // put app on the heap...
    let app = Box::new(app);
    // and leak it so we can put it in a thread-local
    let app_ptr = Box::leak::<'static>(app) as *mut App<M> as *mut u8;
    // super unsafe. We swap our global void pointer to point to our app.
    // We to do it this way because it isn't possible to have
    // generics in globals. That is, the user of the library chooses their own Model
    // so we can't know the full type of App in advance.
    APP.with(move |ptr| {
        let inner = ptr as *const *mut u8 as *mut *mut u8;
        unsafe {
            *inner = app_ptr;
        }
    });

    // Now we prepare to initialize
    let (model, initcmd) = init(Key(()), url);

    // From this point on we only interact with App through App::with.
    // Then it's safe, hopefully.
    App::<M>::with(|app| {
        app.model = Some(model);
        // Run initial command, then rerender just to be sure
        app.loop_update(initcmd)
    })?;

    let link_listener = intercept_links::<M, _>(location, downcast_cpy, on_url_request)?;
    Box::leak(Box::new(link_listener));

    Ok(())
}
