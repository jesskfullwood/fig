#!/usr/bin/python3

from flask import (Flask, request, jsonify)
from flask_cors import CORS
from flask_socketio import SocketIO

app = Flask(__name__)
socketio = SocketIO(app)
CORS(app)

LOOKUP = {
    "this": "What's all this?",
    "that": "That's OK",
    "other": "The other other",
}


@app.route("/", methods=['POST'])
def index():
    try:
        data = request.get_json()['data']
        reply = LOOKUP.get(data) or "Um, what?"
        return jsonify({"data": reply})
    except:
        return "Bad data", 400


if __name__ == "__main__":
    socketio.run(app, port=8001)
