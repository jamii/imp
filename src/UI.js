ws = new WebSocket("ws://localhost:8080");

ws.onerror = function(error) {
    console.log(error);
}

handlers = {}

ws.onmessage = function(event) {
    console.log(event.data);
    msg = JSON.parse(event.data);
    try {
        handlers[msg.kind].apply(this, msg.args)
    } catch (error) {
        console.error(error);
    }
}

handlers.eval = function(code) {
    eval(code);
}

handlers.render = function(html) {
    diff.innerHTML(document.body, html);
}

function message(kind, args) {
    ws.send(JSON.stringify({ kind: kind, args: args }));
}