use std::collections::BTreeMap;
use tree::html::*;
use tree::*;
use wasm_bindgen::prelude::*;

#[derive(Debug, Clone)]
struct Model {
    divs: BTreeMap<u32, String>,
}

impl Default for Model {
    fn default() -> Model {
        Model {
            divs: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
enum Msg {
    Roll(u32, String),
}

impl tree::Model for Model {
    type Msg = Msg;
}

fn update(msg: Msg, mut model: Model) -> Model {
    match msg {
        Msg::Roll(id, text) => {
            model.divs.insert(id, text);
            model
        }
    }
}

fn adiv(id: u32, s: &str) -> Html<Model> {
    div!(format!("{}: {}", id, s))
}

fn view(model: &Model) -> Html<Model> {
    div!(model
        .divs
        .iter()
        .map(|(k, v)| adiv(*k, v))
        .collect::<Vec<_>>())
}

#[wasm_bindgen]
pub fn render() {
    tree::sandbox(Model::default(), view, update, "app").expect("Failed to run");
}
