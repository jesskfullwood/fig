#[macro_use]
extern crate fig;

use rand::seq::SliceRandom;
use rand::{Rng, SeedableRng};

use rand_xorshift::XorShiftRng;

use std::cmp::min;

use fig::event::on_click;
use fig::html::{id, type_};
use fig::{Cmd, Html};
use wasm_bindgen::prelude::wasm_bindgen;

static ADJECTIVES: &[&'static str] = &[
    "pretty",
    "large",
    "big",
    "small",
    "tall",
    "short",
    "long",
    "handsome",
    "plain",
    "quaint",
    "clean",
    "elegant",
    "easy",
    "angry",
    "crazy",
    "helpful",
    "mushy",
    "odd",
    "unsightly",
    "adorable",
    "important",
    "inexpensive",
    "cheap",
    "expensive",
    "fancy",
];

static COLOURS: &[&'static str] = &[
    "red", "yellow", "blue", "green", "pink", "brown", "purple", "brown", "white", "black",
    "orange",
];

static NOUNS: &[&'static str] = &[
    "table", "chair", "house", "bbq", "desk", "car", "pony", "cookie", "sandwich", "burger",
    "pizza", "mouse", "keyboard",
];

struct Row {
    id: usize,
    label: String,
}

impl Row {
    fn new<R>(id: usize, rng: &mut R) -> Row
    where
        R: Rng,
    {
        let mut label = String::new();
        label.push_str(ADJECTIVES.choose(rng).unwrap());
        label.push(' ');
        label.push_str(COLOURS.choose(rng).unwrap());
        label.push(' ');
        label.push_str(NOUNS.choose(rng).unwrap());

        Row { id, label }
    }
}

pub struct Model {
    rows: Vec<Row>,
    next_id: usize,
    selected_id: Option<usize>,
    rng: XorShiftRng,
}

pub enum Msg {
    Run(usize),
    Add(usize),
    Update(usize),
    Clear,
    Swap,
    Remove(usize),
    Select(usize),
}

impl Model {
    fn new() -> Self {
        Model {
            rows: Vec::new(),
            next_id: 1,
            selected_id: None,
            rng: XorShiftRng::from_seed([0; 16]),
        }
    }
}

impl fig::Model for Model {
    type Msg = Msg;

    fn update(&mut self, msg: Self::Msg) -> Cmd<Self::Msg> {
        match msg {
            Msg::Run(amount) => {
                let next_id = self.next_id;
                let rng = &mut self.rng;
                let update_amount = min(amount, self.rows.len());
                for index in 0..update_amount {
                    self.rows[index] = Row::new(next_id + index, rng);
                }
                self.next_id += update_amount;
                self.rows.extend(
                    (update_amount..amount)
                        .map(|index| Row::new(next_id + index, rng))
                        .collect::<Vec<_>>(),
                );
                self.next_id += amount - update_amount;
            }
            Msg::Add(amount) => {
                let next_id = self.next_id;
                let rng = &mut self.rng;
                self.rows.extend(
                    (0..amount)
                        .map(|index| Row::new(next_id + index, rng))
                        .collect::<Vec<_>>(),
                );
                self.next_id += amount;
            }
            Msg::Update(step) => {
                for index in (0..(self.rows.len() / step)).map(|x| x * step) {
                    self.rows[index].label += " !!!";
                }
            }
            Msg::Clear => {
                self.rows.clear();
            }
            Msg::Swap => {
                if self.rows.len() > 998 {
                    self.rows.swap(1, 998);
                }
            }
            Msg::Remove(id) => {
                if let Some((index, _)) = self.rows.iter().enumerate().find(|(_, row)| row.id == id)
                {
                    self.rows.remove(index);
                }
            }
            Msg::Select(id) => {
                if self.selected_id == Some(id) {
                    self.selected_id = None;
                } else {
                    self.selected_id = Some(id);
                }
            }
        }
        Cmd::none()
    }

    fn view(&self) -> Html<Self> {
        div!(
            class!("container"),
            div!(
                class!("jumbotron"),
                div!(
                    class!("row"),
                    div!(class!("col-md-6"), h1!("Fig"),),
                    div!(
                        class!("col-md-6"),
                        div!(
                            class!("row"),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("run"),
                                    on_click((), |_| Msg::Run(1_000)),
                                    "Create 1,000 rows"
                                )
                            ),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("runlots"),
                                    on_click((), |_| Msg::Run(10_000)),
                                    "Create 10,000 rows"
                                )
                            ),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("add"),
                                    on_click((), |_| Msg::Add(1_000)),
                                    "Append 1,000 rows"
                                )
                            ),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("update"),
                                    on_click((), |_| Msg::Update(10)),
                                    "Update every 10th row"
                                )
                            ),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("clear"),
                                    on_click((), |_| Msg::Clear),
                                    "Clear"
                                )
                            ),
                            div!(
                                class!("col-sm-6", "smallpad"),
                                button!(
                                    type_("button"),
                                    class!("btn", "btn-primary", "btn-block"),
                                    id("swaprows"),
                                    on_click((), |_| Msg::Swap),
                                    "Swap Rows"
                                )
                            ),
                        )
                    )
                )
            ),
            table!(
                class!("table", "table-hover", "table-striped", "test-data"),
                tbody!(id("tbody"), {
                    self.rows
                        .iter()
                        .map(|row| {
                            let id = row.id.clone();
                            tr!(
                                class!(if self.selected_id == Some(id) {
                                    "danger"
                                } else {
                                    ""
                                }),
                                td!(class!("col-md-1"), id.to_string()),
                                td!(
                                    class!("col-md-4"),
                                    on_click(id, |id| Msg::Select(*id)),
                                    a!(class!("lbl"), row.label.clone()),
                                ),
                                td!(
                                    class!("col-md-1"),
                                    a!(
                                        class!("remove"),
                                        on_click(id, |id| Msg::Remove(*id)),
                                        span!(class!("glyphicon", "glyphicon-remove", "remove"))
                                    )
                                ),
                                td!(class!("col-md-6"))
                            )
                        })
                        .collect::<Vec<_>>()
                })
            ),
            span!(class!("preloadicon", "glyphicon", "glyphicon-remove"))
        )
    }
}

#[wasm_bindgen]
pub fn render() {
    fig::run(Model::new(), "app").expect("Failed to start app")
}
