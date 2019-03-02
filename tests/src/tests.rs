use tree::html::{class, on_click, value};
use tree::{button, div, option, p, select};
use tree::{Html, Model};

struct M(i32);

impl Model for M {
    type Msg = i32;
}

#[test]
fn nested_div() {
    let m = M(123);

    let div: Html<M> = div!(
        "some text",
        " more text",
        p!(class("bluesy"), "Classy!"),
        button!(on_click(|| 10), format!("Clicked: {}", m.0)),
        select!(
            option!(value("a"), "a"),
            option!(value("b"), "b"),
            option!(value("c"), "c"),
        ),
    );
    println!("{}", div);
    // panic!();
}
