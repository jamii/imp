ws = new WebSocket("ws://localhost:8080");

ws.onerror = function(error) {
    console.log(error);
}

handlers = {}

ws.onmessage = function(event) {
    console.log(event.data);
    msg = JSON.parse(event.data);
    try {
        if (msg.Render) {
            render(msg.Render);
        }
    } catch (error) {
        console.error(error);
    }
}

render = function(html) {
    diff.innerHTML(document.body, html);
}

send = function(event) {
    ws.send(JSON.stringify(event));
}