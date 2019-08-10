use fig::html::*;
use fig::*;
use wasm_bindgen::prelude::*;

#[derive(Default)]
struct Model {
    todos: Vec<Todo>,
}

fn test() -> Model {
    Model {
        // todos: vec![
        //     Todo { text: "This".into(), state: State::Active },
        //     Todo { text: "That".into(), state: State::Completed },
        // ]
        todos: vec![],
    }
}

struct Todo {
    text: String,
    state: State,
}

pub enum State {
    Active,
    Completed,
}

impl Todo {
    fn view(&self) -> Html<Model> {
        li!(
            div!(
                class!("view"),
                input!(
                    class!("toggle"),
                    type_("checkbox"),
                    label!(self.text.clone()),
                    button!(class!("destroy"))
                )
            ),
            input!(class!("edit"), value(self.text.clone()))
        )
    }
}

enum Msg {
    ButtonClicked,
}

impl fig::Model for Model {
    type Msg = Msg;

    fn update(&mut self, msg: Self::Msg) -> Cmd<Self::Msg> {
        match msg {
            Msg::ButtonClicked => Cmd::none(),
        }
    }

    fn view(&self) -> Html<Self> {
        let todos: Vec<_> = self.todos.iter().map(|t| t.view()).collect();
        let header = header![
            class!("header"),
            h1!("todos"),
            input![class!("new-todo"), placeholder("What needs to be done?")]
        ];
        let main = section![
            class!("main"),
            input!(id("toggle-all"), class!("toggle-all"), type_("checkbox")),
            label!(for_("toggle-all")),
            ul!(class!("todo-list"), todos)
        ];
        let footer = footer![
            class!("footer"),
            span!(
                class!("todo-count"),
                format!("{} items left", self.todos.len())
            ),
            ul!(
                class!("filters"),
                vec![
                    li!(a!(class!("selected"), "All"),),
                    span!(" "),
                    li!("Active"),
                    span!(" "),
                    li!("Completed")
                ]
            )
        ];
        section![
            class!("todoapp"),
            div!(
                header,
                if self.todos.len() > 0 {
                    Some((main, footer))
                } else {
                    None
                }
            ),
            footer![
                class!("info"),
                p!("Double-click to edit a todo"),
                p!("Created by Alex Whitney"),
                p!("Design by ", a!("TodoMVC", href("http://todomvc.com")))
            ]
        ]
    }
}

#[wasm_bindgen]
pub fn render() {
    fig::run(test(), "app").expect("Failed to start app")
}
