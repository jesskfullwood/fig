use fig::*;
use wasm_bindgen::prelude::*;

struct Model {
    click_count: usize,
}

enum Msg {
    ButtonClicked,
}

impl fig::Model for Model {
    type Msg = Msg;

    fn update(&mut self, msg: Self::Msg) -> Cmd<Self::Msg> {
        match msg {
            Msg::ButtonClicked => {
                self.click_count += 1;
                Cmd::none()
            }
        }
    }

    fn view(&self) -> Html<Self> {
        div![
            style!("font-family" => "Arial", "text-align" => "center"),
            h1!("Hello from fig!"),
            button!("Click me!", on_click((), |()| Msg::ButtonClicked)),
            p!(format!("Click count: {}", self.click_count))
        ]
    }
}

#[wasm_bindgen]
pub fn render() {
    fig::run(Model { click_count: 0 }, "app").expect("Failed to start app")
}
