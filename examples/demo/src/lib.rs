use fig::fetch;
use fig::html::*;
use fig::select;
use fig::socket::Socket;
use fig::timer::Timer;
use fig::Model as _;
use fig::*;
use futures::Future;
use log::{info, trace};
use serde::{Deserialize, Serialize};
// TODO remove this dependency
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
    ticker: bool,
    socket: bool,
    socket_message: Option<String>,
}

impl Default for Model {
    fn default() -> Model {
        Model {
            select: "this".into(),
            check: false,
            input: "Initial".into(),
            click_ct: 0,
            list_ct: 5,
            server_says: None,
            route: Route::Home,
            ticker: false,
            socket: false,
            socket_message: None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Route {
    Home,
    Items,
    Summary,
}

fn on_url_change(url: Url) -> Cmd<Msg> {
    info!("Url changed: {}", url);
    let route = match url.path() {
        "/" => Route::Home,
        "/items" => Route::Items,
        "/summary" => Route::Summary,
        _other => return Cmd::load_url("/"),
    };
    Cmd::msg(Msg::Route(route))
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
    SocketMessage(String),
    ToggleTicker,
    ToggleSocket,
    Tick,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Data {
    data: String,
}

impl fig::Model for Model {
    type Msg = Msg;
}

fn update(msg: Msg, model: Model) -> (Model, Cmd<Msg>) {
    info!("Update model with message: {:?}", msg);
    match msg {
        Msg::Select(select) => Model { select, ..model }.no_cmd(),
        Msg::FetchSelected(val) => model.with_cmd(Cmd::spawn(fetch_selected(val))),
        Msg::FetchedSelected(val) => Model {
            server_says: Some(val),
            ..model
        }
        .no_cmd(),
        Msg::ToggleCheck => Model {
            check: !model.check,
            ..model
        }
        .no_cmd(),
        Msg::Input(input) => Model {
            input: input.to_ascii_lowercase(),
            ..model
        }
        .no_cmd(),
        Msg::ButtonClick => Model {
            click_ct: model.click_ct + 1,
            ..model
        }
        .no_cmd(),
        Msg::AddLi => Model {
            list_ct: model.list_ct + 1,
            ..model
        }
        .no_cmd(),
        Msg::RmLi => Model {
            list_ct: if model.list_ct > 0 {
                model.list_ct - 1
            } else {
                0
            },
            ..model
        }
        .no_cmd(),
        Msg::Route(route) => Model { route, ..model }.no_cmd(),
        Msg::ToggleSocket => Model {
            socket: !model.socket,
            ..model
        }
        .no_cmd(),
        Msg::SocketMessage(msg) => {
            info!("Socket: {}", msg);
            Model {
                socket_message: Some(msg),
                ..model
            }
        }
        .no_cmd(),
        Msg::ToggleTicker => Model {
            ticker: !model.ticker,
            ..model
        }
        .no_cmd(),
        Msg::Tick => {
            info!("TICK!!!");
            model.no_cmd()
        }
    }
}

fn new_websocket() -> Socket<Model> {
    Socket::new(
        "ws://localhost:8001",
        || Msg::SocketMessage("Connection opened".into()).into(),
        |msg| Msg::SocketMessage(format!("Message: {:?}", msg)).into(),
        |err| Msg::SocketMessage(format!("Error: {:?}", err)).into(),
    )
}

fn subscriptions(model: &Model) -> Sub<Model> {
    let mut subs: Vec<Box<dyn Subscription<_>>> = Vec::new();
    if model.ticker {
        subs.push(Box::new(Timer::new(1000, || Cmd::msg(Msg::Tick))))
    }
    if model.socket {
        subs.push(Box::new(new_websocket()))
    }

    Sub::new(subs)
}

fn fetch_selected(val: String) -> impl Future<Item = Cmd<Msg>, Error = Cmd<Msg>> {
    info!("Fetch: '{}'", val);
    fetch::Request::new("http://localhost:8000/api".to_string())
        .method(fetch::Method::Post)
        .send_json(&Data { data: val })
        .fetch_json_data(|res: Result<Data, _>| {
            if let Ok(data) = res {
                Cmd::msg(Msg::FetchedSelected(data.data))
            } else {
                // TODO display the error somewhere
                Cmd::none()
            }
        })
}

fn view(model: &Model) -> Html<Model> {
    trace!("Rendering model: {:?}", model);
    div!(
        id("my-app"),
        h1!("Fig demo"),
        p!("Enter some ", b!("text"), " if you please!"),
        div!(
            input!(
                value(model.input.clone()),
                on_input((), |(), input| Msg::Input(input)),
                placeholder("placeholder")
            ),
            p!(i!("Boldly repeat: "), b!(model.input.clone()))
        ),
        div!(
            button!(
                if model.ticker {
                    "Ticker: On"
                } else {
                    "Ticker: Off"
                },
                on_click((), |()| Msg::ToggleTicker)
            ),
            button!(
                if model.socket {
                    "Websocket: Disconnect"
                } else {
                    "Websocket: Connect"
                },
                on_click((), |()| Msg::ToggleSocket)
            ),
            p!(class!("bluesy"), "Classy!"),
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
            p!("Our server says:", {
                let says = if let Some(ref says) = model.server_says {
                    says
                } else {
                    "Nothing!"
                };
                b!(says.to_string())
            })
        ),
        div!(
            id("links"),
            p!(a!(href("/"), "Home")),
            match model.route {
                Route::Home => div!(
                    p!(a!(href("/items"), "View items")),
                    p!(a!(href("/summary"), "View summary"))
                ),
                Route::Items => div!(
                    button!(on_click((), |()| Msg::AddLi), "+ item"),
                    button!(on_click((), |()| Msg::RmLi), "- item"),
                    ul!((0..model.list_ct)
                        .map(|i| li!(format!("List item {}", i)))
                        .collect::<Vec<_>>()),
                ),
                Route::Summary => div!(p!(format!("You have created {} items", model.list_ct)),),
            }
        )
    )
}

#[wasm_bindgen]
pub fn render() {
    console_log::init_with_level(log::Level::Debug).unwrap();
    fig::application(
        |_key, url| (Model::default(), on_url_change(url)),
        view,
        update,
        subscriptions,
        fig::util::on_url_request_intercept,
        on_url_change,
        "app",
    )
    .expect("Failed to run");
}
