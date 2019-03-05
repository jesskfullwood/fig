use futures::Future;
use serde::{Deserialize, Serialize};
use tree::fetch;
use tree::html::*;
use tree::select;
use tree::*;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
struct Model {
    select: String,
    check: bool,
    input: String,
    click_ct: u32,
    list_ct: u32,
    server_says: Option<String>,
    route: Route,
}

impl Default for Model {
    fn default() -> Model {
        Model {
            select: String::new(),
            check: false,
            input: "Initial".into(),
            click_ct: 0,
            list_ct: 5,
            server_says: None,
            route: Route::Summary,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Route {
    Items,
    Summary,
}

fn on_url_change(url: url::Url) -> Msg {
    log!("Url change");
    match url.path() {
        "/" | "/items" => Msg::Route(Route::Items),
        "/summary" => Msg::Route(Route::Summary),
        _ => unimplemented!(),
    }
}

#[derive(Clone, Debug)]
enum Msg {
    Select(String),
    ToggleCheck,
    Input(String),
    ButtonClick,
    AddLi,
    RmLi,
    FetchSelected(String),
    FetchedSelected(String),
    Route(Route),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Data {
    data: String,
}

impl tree::Model for Model {
    type Msg = Msg;
}

fn update(msg: Msg, model: Model) -> (Model, Cmd<Msg>) {
    log!("update model with message: {:?}", msg);
    match msg {
        Msg::Select(select) => (Model { select, ..model }, Cmd::None),
        Msg::FetchSelected(val) => (model, Cmd::Fetch(Box::new(fetch_selected(val)))),
        Msg::FetchedSelected(val) => (
            Model {
                server_says: Some(val),
                ..model
            },
            Cmd::None,
        ),
        Msg::ToggleCheck => (
            Model {
                check: !model.check,
                ..model
            },
            Cmd::None,
        ),
        Msg::Input(input) => (
            Model {
                input: input.to_ascii_lowercase(),
                ..model
            },
            Cmd::None,
        ),
        Msg::ButtonClick => (
            Model {
                click_ct: model.click_ct + 1,
                ..model
            },
            Cmd::None,
        ),
        Msg::AddLi => (
            Model {
                list_ct: model.list_ct + 1,
                ..model
            },
            Cmd::None,
        ),
        Msg::RmLi => (
            Model {
                list_ct: if model.list_ct > 0 {
                    model.list_ct - 1
                } else {
                    0
                },
                ..model
            },
            Cmd::None,
        ),
        Msg::Route(route) => (Model { route, ..model }, Cmd::None),
    }
}

fn fetch_selected(val: String) -> impl Future<Item = Msg, Error = JsValue> {
    log!("Fetch: '{}'", val);
    fetch::Request::new("http://localhost:8001")
        .method(fetch::Method::Post)
        .header("Content-Type", "application/json")
        .body_json(&Data { data: val })
        .fetch_json()
        .map(|data: Data| Msg::FetchedSelected(data.data))
}

fn view(model: &Model) -> Html<Model> {
    log!("rendering model: {:?}", model);
    div!(
        id("my-app"),
        h1!("Tree demo"),
        p!("Enter some text!"),
        div!(
            input!(
                value(model.input.clone()),
                on_input((), |(), input| Msg::Input(input)),
                placeholder("placeholder")
            ),
            p!(i!("Boldly repeat: "), b!(model.input.clone()))
        ),
        div!(
            p!(class("bluesy"), "Classy!"),
            button!(
                on_click((), |()| Msg::ButtonClick),
                format!("Clicked: {}", model.click_ct),
            ),
            select!(
                on_input((), |(), sel| Msg::Select(sel)),
                option!(value("this"), "this"),
                option!(value("that"), "that"),
                option!(value("other"), "other"),
            ),
            button!(
                on_click(model.select.clone(), |select| Msg::FetchSelected(
                    select.clone()
                )),
                "Send request"
            ),
            p!({
                let says = if let Some(ref says) = model.server_says {
                    says
                } else {
                    "Nothing!"
                };
                format!("Our server says: {}", says)
            })
        ),
        div!(if Route::Items == model.route {
            div!(
                button!(on_click((), |()| Msg::AddLi), "+ item"),
                button!(on_click((), |()| Msg::RmLi), "- item"),
                ul!((0..model.list_ct)
                    .map(|i| li!(format!("List item {}", i)))
                    .collect::<Vec<_>>()),
            )
        } else {
            div!(
                p!(format!("You have created {} items", model.list_ct)),
                p!(a!(href("/items"), "Click to view"))
            )
        })
    )
}

#[wasm_bindgen]
pub fn render() {
    tree::run(Model::default(), update, view, on_url_change, "app").expect("Failed to run");
}
