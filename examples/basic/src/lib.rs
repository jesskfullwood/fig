use tree::log;
use tree::*;
use wasm_bindgen::prelude::*;

#[derive(Clone, Default)]
struct Model {
    select: String,
    check: bool,
    input: String,
}

enum Msg {
    Select(String),
    ToggleCheck,
    Input(String),
}

impl tree::Model for Model {
    type Msg = Msg;
}

fn update(msg: Msg, model: Model) -> (Model, Cmd<Msg>) {
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
    }
}

fn view(model: &Model) -> Html<Model> {
    div(
        (),
        (
            p((), text("This is some text")),
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
