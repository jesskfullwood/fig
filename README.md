# Fig

A Rust/WASM frontend framework inspired by [The Elm Architecture](https://guide.elm-lang.org/architecture/).

**Features:**
* Fast rendering
* Typesafe event handlers
* HTML-as-code
* Push- and PopState support
* `fetch` requests
* Timers
* WebSockets

Built on top of [`wasm_bindgen`](https://github.com/rustwasm/wasm-bindgen).

## Minimal Example

```rust
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
```

Results in:
![hello fig](/examples/quickstart/hello-fig.png)

See the [`quickstart` demo](/examples/quickstart) and the [more fully-featured demo](/examples/demo) for more.
