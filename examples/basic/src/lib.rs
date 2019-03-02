use tree::log;
use tree::*;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
struct Model {
    select: String,
    check: bool,
    input: String,
    click_ct: u32,
    list_ct: u32,
}

impl Default for Model {
    fn default() -> Model {
        Model {
            select: String::new(),
            check: false,
            input: String::new(),
            click_ct: 0,
            list_ct: 5,
        }
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
    }
}

fn view(model: &Model) -> Html<Model> {
    log!("view: {:?}", model);
    div(
        id("my-app"),
        (
            p((), (), "This is some text"),
            p(
                (),
                input(
                    (
                        value(&model.input),
                        on_input(Msg::Input),
                        placeholder("placeholder"),
                    ),
                    (),
                ),
                (),
            ),
            p(
                (),
                button(
                    on_click(|| Msg::ButtonClick),
                    format!("Clicked: {}", model.click_ct),
                ),
                (),
            ),
            select(
                on_input(Msg::Select),
                (
                    option(value("a"), "a"),
                    option(value("b"), "b"),
                    option(value("c"), "c"),
                ),
            ),
            button(on_click(|| Msg::AddLi), "+ item"),
            button(on_click(|| Msg::RmLi), "- item"),
            ul(
                (),
                (0..model.list_ct)
                    .map(|i| li((), format!("List item {}", i)))
                    .collect::<Vec<_>>(),
            ),
        ),
    )
}

#[wasm_bindgen]
pub fn render() {
    tree::run(Model::default(), update, view, "app");
    log!("Hi")
}
