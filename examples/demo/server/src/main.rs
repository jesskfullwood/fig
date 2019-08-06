use serde::{Deserialize, Serialize};

use actix_files::NamedFile;
use actix_web::{web, web::Json, App, HttpRequest, HttpResponse, HttpServer, Responder};
use actix_web_actors::ws;
use log::info;

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

/// Entry point for web socket
fn socket(req: HttpRequest, stream: web::Payload) -> Result<HttpResponse, actix_web::Error> {
    ws::start(MyActor, &req, stream)
}

struct MyActor;

impl actix::Actor for MyActor {
    type Context = ws::WebsocketContext<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        info!("Websocket actor started");
    }

    fn stopped(&mut self, ctx: &mut Self::Context) {
        info!("Websocket actor stopped");
    }
}

impl actix::StreamHandler<ws::Message, ws::ProtocolError> for MyActor {
    fn handle(&mut self, msg: ws::Message, ctx: &mut Self::Context) {
        info!("Socket message: {:?}", msg);
    }
}

fn main() -> std::io::Result<()> {
    std::env::set_var("RUST_LOG", "info");
    env_logger::init();
    HttpServer::new(|| {
        App::new()
            .wrap(actix_web::middleware::Logger::default())
            .route("/api", web::post().to(fetch))
            .route("/ws", web::get().to(socket))
            .service(actix_files::Files::new("/static", "static"))
            .route("/{tail:.*}", web::get().to(index))
    })
    .bind("127.0.0.1:8000")?
    .run()
}
