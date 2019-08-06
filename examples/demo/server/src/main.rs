use serde::{Deserialize, Serialize};

use actix_web::{web, web::Json, App, HttpServer, Responder};

use actix_files::NamedFile;

fn index() -> impl Responder {
    NamedFile::open("static/index.html")
}

#[derive(Serialize, Deserialize)]
struct MyData {
    data: String,
}

fn fetch(Json(d): Json<MyData>) -> impl Responder {
    let resp = match d.data.as_str() {
        "this" => "What's all this?",
        "that" => "That's OK",
        "other" => "The other other",
        _ => "Um, what??",
    };
    Json(MyData { data: resp.into() })
}

fn main() -> std::io::Result<()> {
    HttpServer::new(|| {
        App::new()
            .route("/api", web::post().to(fetch))
            .service(actix_files::Files::new("/static", "static"))
            .route("/{tail:.*}", web::get().to(index))
    })
    .bind("127.0.0.1:8000")?
    .run()
}
