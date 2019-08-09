use crate::{set_link_click_handler, App, Element, Html, JsResult, JsValue, Model, Tag, APP};

use wasm_bindgen::JsCast;
use web_sys::{Element as DomElement, HtmlDivElement};

use std::collections::HashMap;

/// Run a single-page application, including routing and HTTP requests
pub fn run<M: Model>(model: M, target: &str) -> JsResult<()> {
    info!("Launching application");
    // Set the hook to get sensible(ish) error messages upon panic
    console_error_panic_hook::set_once();
    let window = web_sys::window().expect("No global `window` exists");
    let document = window.document().expect("No document");
    let location = document.location().expect("No location");
    let url = url::Url::parse(&location.href()?).map_err(|e| JsValue::from_str(&e.to_string()))?;
    let target = document
        .get_element_by_id(target)
        .expect("Target element not found");
    let target: HtmlDivElement = target.dyn_into()?;
    let root_elem: DomElement = target.clone().dyn_into().unwrap();
    let initial = document.create_element(&Tag::Div.to_string())?;
    target.set_inner_html(""); // blank the target div and create an initial root
    target.append_child(&*initial)?;

    let app = App {
        window: window.clone(),
        target,
        model,
        current_vdom: Html::from(Element::tag(Tag::Div)), // now the dom and vdom are in sync
        listeners: HashMap::new(),
        // TODO this could be a hashmap to reduce On^2 complexity
        subscriptions: Vec::new(),
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

    // From this point on we only interact with App through App::with.
    // Then it's safe, hopefully.
    App::<M>::with(|app| {
        app.set_popstate_handler();

        let initcmd = app.model.init(url);
        // Run initial command, then rerender just to be sure
        app.loop_update(initcmd)?;

        // Set the handler which intercepts all clicks and sees if they were on links, if so - intercepts)
        let link_listener = set_link_click_handler::<M, _>(location, root_elem, M::on_url_request)?;
        // We leak this listener because it must be valid for the life of the program
        Box::leak(Box::new(link_listener));

        JsResult::Ok(())
    })
}
