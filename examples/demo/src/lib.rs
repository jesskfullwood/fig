use fig::fetch;
use fig::html::*;
use fig::select;
use fig::socket::Socket;
use fig::timer::Timer;
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
    tick_on: bool,
    socket: SocketState,
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
            tick_on: false,
            socket: SocketState::Closed,
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
    SocketMessage(SocketMsg),
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

    fn update(&mut self, msg: Msg) -> Cmd<Msg> {
        info!("Update model with message: {:?}", msg);
        match msg {
            Msg::Select(select) => { self.select = select; Cmd::none() }
            Msg::FetchSelected(val) => Cmd::spawn(fetch_selected(val)),
            Msg::FetchedSelected(val) => {
                self.server_says = Some(val);
                Cmd::none()
            }
            Msg::ToggleCheck => {
                self.check = !self.check;
                Cmd::none()
            }
            Msg::Input(input) => {
                self.input = input.to_ascii_lowercase();
                Cmd::none()
            }
            Msg::ButtonClick => {
                self.click_ct += 1;
                Cmd::none()
            }
            Msg::AddLi => {
                self.list_ct += 1;
                Cmd::none()
            }
            Msg::RmLi => {
                if self.list_ct > 0 {
                    self.list_ct -= 1;
                }
                Cmd::none()
            }
            Msg::Route(route) => { self.route = route; Cmd::none() }
            Msg::ToggleSocket => {
                self.socket = match self.socket {
                    SocketState::Closed =>  SocketState::TryOpen,
                    _ => SocketState::Closed
                };
                Cmd::none()
            }
            Msg::SocketMessage(SocketMsg::Opened) => {
                self.socket = SocketState::Open;
                Cmd::none()
            }
            Msg::SocketMessage(SocketMsg::Msg(msg)) | Msg::SocketMessage(SocketMsg::Err(msg)) => {
                self.socket_message = Some(msg);
                Cmd::none()
            }
            Msg::ToggleTicker => {
                self.ticker = !self.ticker;
                Cmd::none()
            }
            Msg::Tick => {
                self.tick_on = !self.tick_on;
                Cmd::none()
            }
        }
    }

    fn view(&self) -> Html<Model> {
        trace!("Rendering model: {:?}", self);
        div!(
            id("my-app"),
            h1!("Fig demo"),
            p!("Enter some ", b!("text"), " if you please!"),
            div!(
                input!(
                    value(self.input.clone()),
                    on_input((), |(), input| Msg::Input(input)),
                    placeholder("placeholder")
                ),
                p!(i!("Boldly repeat: "), b!(self.input.clone()))
            ),
            p!(class!("bluesy"), "Classy!"),
            div!(
                button!(
                    on_click((), |()| Msg::ButtonClick),
                    format!("Clicked: {}", self.click_ct),
                ),
            ),
            div!(
                button!(
                    if self.ticker {
                        "Ticker: On"
                    } else {
                        "Ticker: Off"
                    },
                    on_click((), |()| Msg::ToggleTicker)
                ),
                if self.ticker && self.tick_on {
                    Some(span!("TICK!"))
                } else {
                    None
                },
            ),
            div!(
                button!(
                    if let SocketState::Closed = self.socket {
                        "Websocket: Connect"
                    } else {
                        "Websocket: Disconnect"
                    },
                    on_click((), |()| Msg::ToggleSocket)
                ),
                span!(format!("State: {:?}", self.socket)),
            ),
            div!(
                select!(
                    on_input((), |(), sel| Msg::Select(sel)),
                    option!(value("this"), "this"),
                    option!(value("that"), "that"),
                    option!(value("other"), "other"),
                ),
                button!(
                    on_click(self.select.clone(), |select| Msg::FetchSelected(
                        select.clone()
                    )),
                    "Send request"
                ),
                p!("Our server says:", {
                    let says = if let Some(ref says) = self.server_says {
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
                match self.route {
                    Route::Home => div!(
                        p!(a!(href("/items"), "View items")),
                        p!(a!(href("/summary"), "View summary"))
                    ),
                    Route::Items => div!(
                        button!(on_click((), |()| Msg::AddLi), "+ item"),
                        button!(on_click((), |()| Msg::RmLi), "- item"),
                        ul!((0..self.list_ct)
                            .map(|i| li!(format!("List item {}", i)))
                            .collect::<Vec<_>>()),
                    ),
                    Route::Summary => div!(p!(format!("You have created {} items", self.list_ct)),),
                }
            )
        )
    }

    fn subscribe(&self) -> Sub<Self> {
        let mut subs: Vec<Box<dyn Subscription<_>>> = Vec::new();
        if self.ticker {
            subs.push(Box::new(Timer::new(1000, || Cmd::msg(Msg::Tick))))
        }
        if let SocketState::Closed = self.socket {
            // To close the socket, we just dispose of it
        } else {
            subs.push(Box::new(new_websocket()))
        }
        Sub::new(subs)
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
}

fn new_websocket() -> Socket<Model> {
    Socket::new(
        "ws://localhost:8000/ws",
        || Msg::SocketMessage(SocketMsg::Opened).into(),
        |msg| Msg::SocketMessage(SocketMsg::Msg(format!("Message: {:?}", msg))).into(),
        |err| Msg::SocketMessage(SocketMsg::Err(format!("Error: {:?}", err))).into(),
    )
}

#[derive(Debug, Clone)]
enum SocketState {
    Closed,
    TryOpen,
    Open,
}

#[derive(Debug, Clone)]
enum SocketMsg {
    Opened,
    Msg(String),
    Err(String),
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

#[wasm_bindgen]
pub fn render() {
    console_log::init_with_level(log::Level::Debug).unwrap();
    fig::run(Model::default(), "app").expect("Failed to run");
}
