use tree::log;
use tree::*;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone, Default)]
struct Model {
    select: String,
    check: bool,
    input: String,
}

#[derive(Clone, Debug)]
enum Msg {
    Select(String),
    ToggleCheck,
    Input(String),
    ButtonClick
}

impl tree::Model for Model {
    type Msg = Msg;
}

fn update(msg: Msg, model: Model) -> (Model, Cmd<Msg>) {
    log!("Update: {:?}", msg);
    match msg {
        Msg::Select(select) => (Model { select, ..model }, Cmd::None),
        Msg::ToggleCheck => (
            Model {
                check: !model.check,
                ..model
            },
            Cmd::None,
        ),
        Msg::Input(input) => (Model { input, ..model }, Cmd::None),
        Msg::ButtonClick => {
            log!("Click!");
            (model, Cmd::None)
        }
    }
}

fn view(model: &Model) -> Html<Model> {
    div(
        (),
        (
            p((), text("This is some text")),
            p((), input((on_input(Msg::Input),
                         placeholder("placeholder")),
                        ())),
            p((), button(on_click(|| Msg::ButtonClick), text("Clicky!"))),
            select(
                on_input(Msg::Select),
                (
                    option(value("a"), text("a")),
                    option(value("b"), text("b")),
                    option(value("c"), text("c")),
                ),
            ),
        ),
    )
}

#[wasm_bindgen]
pub fn render() {
    tree::run(Model::default(), update, view);
    log!("Hi")
}
