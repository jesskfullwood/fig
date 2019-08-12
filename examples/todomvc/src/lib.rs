use fig::html::*;
use fig::*;
use wasm_bindgen::prelude::*;

struct Model {
    wip: String,
    todos: Vec<Todo>,
    route: Route,
}

fn test() -> Model {
    Model {
        wip: String::new(),
        todos: vec![
            Todo {
                text: "This".into(),
                completed: false,
                editing: false,
            },
            Todo {
                text: "That".into(),
                completed: false,
                editing: false,
            },
        ],
        route: Route::All,
    }
}

#[derive(Debug, Clone, Hash)]
struct Todo {
    text: String,
    completed: bool,
    editing: bool,
}

impl Todo {
    fn new(text: String) -> Todo {
        Todo {
            text,
            completed: false,
            editing: false,
        }
    }
}

#[derive(Debug, Clone, Hash)]
pub enum Route {
    All,
    Active,
    Completed,
}

impl Todo {
    fn view(&self, ix: usize) -> Html<Model> {
        let mut c = if self.completed {
            vec!["completed".into()]
        } else {
            vec![]
        };
        if self.editing {
            c.push("editing".into());
        }
        li!(
            Attribute::class(c),
            div!(
                class!("view"),
                input!(
                    class!("toggle"),
                    type_("checkbox"),
                    on_click((ix, self.clone()), |(ix, t)| {
                        let mut t = t.clone();
                        t.completed = !t.completed;
                        Msg::TodoUpdate {
                            index: *ix,
                            todo: t,
                        }
                    })
                ),
                label!(
                    self.text.clone(),
                    on_dbl_click((ix, self.clone()), |(ix, t)| {
                        let mut t = t.clone();
                        t.editing = true;
                        Msg::TodoUpdate {
                            index: *ix,
                            todo: t,
                        }
                    })
                ),
                button!(class!("destroy"), on_click(ix, |ix| Msg::RemoveTodo(*ix)))
            ),
            input!(
                class!("edit"),
                value(self.text.clone()),
                // TODO I don't like this. It was confusing. Opaque error message
                on_input((ix, self.clone()), |(ix, t), s| {
                    let mut t = t.clone();
                    t.text = s;
                    Msg::TodoUpdate {
                        index: *ix,
                        todo: t,
                    }
                }),
                on_keydown("Enter", (ix, self.clone()), |(ix, t)| {
                    let mut t = t.clone();
                    t.editing = false;
                    Msg::TodoUpdate {
                        index: *ix,
                        todo: t,
                    }
                }),
            )
        )
    }
}

enum Msg {
    NewTodoUpdate(String),
    NewTodo(String),
    TodoUpdate { index: usize, todo: Todo },
    RemoveTodo(usize),
    Route(Route),
}

impl fig::Model for Model {
    type Msg = Msg;

    fn update(&mut self, msg: Self::Msg) -> Cmd<Self::Msg> {
        match msg {
            Msg::NewTodoUpdate(text) => {
                self.wip = text;
            }
            Msg::NewTodo(text) => {
                self.todos.push(Todo::new(text));
                self.wip = String::new();
            }
            Msg::TodoUpdate { index, todo } => {
                self.todos[index] = todo;
            }
            Msg::RemoveTodo(ix) => {
                self.todos.remove(ix);
            }
            Msg::Route(route) => self.route = route,
        };
        Cmd::none()
    }

    fn view(&self) -> Html<Self> {
        let todos: Vec<_> = self
            .todos
            .iter()
            .filter_map(|t| match self.route {
                Route::All => Some(t),
                Route::Active => {
                    if t.completed {
                        None
                    } else {
                        Some(t)
                    }
                }
                Route::Completed => {
                    if t.completed {
                        Some(t)
                    } else {
                        None
                    }
                }
            })
            .enumerate()
            .map(|(ix, t)| t.view(ix))
            .collect();
        let header = header![
            class!("header"),
            h1!("todos"),
            input![
                class!("new-todo"),
                placeholder("What needs to be done?"),
                on_input((), |(), s| Msg::NewTodoUpdate(s.clone())),
                on_keydown("Enter", self.wip.clone(), |wip| Msg::NewTodo(wip.clone())),
                value(self.wip.clone()),
                self.wip.clone()
            ]
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
                    li!(
                        a!(
                            if let Route::All = self.route {
                                Some(class!("selected"))
                            } else {
                                None
                            },
                            href("/"),
                            "All",
                        ),
                        on_click((), |()| Msg::Route(Route::All))
                    ),
                    span!(" "),
                    li!(
                        a!(
                            if let Route::Active = self.route {
                                Some(class!("selected"))
                            } else {
                                None
                            },
                            href("/active"),
                            "Active"
                        ),
                        on_click((), |()| Msg::Route(Route::Active))
                    ),
                    span!(" "),
                    li!(
                        a!(
                            if let Route::Completed = self.route {
                                Some(class!("selected"))
                            } else {
                                None
                            },
                            href("/completed"),
                            "Completed"
                        ),
                        on_click((), |()| Msg::Route(Route::Completed))
                    )
                ]
            )
        ];
        div![
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
            ],
            footer![
                class!("info"),
                p!("Double-click to edit a todo"),
                p!("Created by Alex Whitney"),
                p!("Design by ", a!("TodoMVC", href("http://todomvc.com")))
            ]
        ]
    }

    fn on_url_change(url: Url) -> Cmd<Msg> {
        let route = match url.path() {
            "/" => Route::All,
            "/active" => Route::Active,
            "/completed" => Route::Completed,
            _other => return Cmd::load_url("/"), // redirect
        };
        Cmd::msg(Msg::Route(route))
    }
}

#[wasm_bindgen]
pub fn run() {
    fig::run(test(), "app").expect("Failed to start app")
}
