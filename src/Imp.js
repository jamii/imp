ws = new WebSocket("ws://localhost:8080");

function imp_event(table) {
    return function () {
        ws.send(JSON.stringify({"table": table, "values": Array.from(arguments)}));
        return false;
    }
}

ws.onerror = function (error) {
    console.log(error);
}

ws.onmessage = function (event) {
    msg = JSON.parse(event.data);
    if (msg.session) {
        window.session = msg.session;
    }
    if (msg.events) {
        for (var i = 0; i < msg.events.length; i++) {
            window[msg.events[i]] = imp_event(msg.events[i]);
        }
    }
    if (msg.render) {
        render.apply(this, msg.render);
    }
}

nodes = {}
queue = []

function onrender(callback) {
    queue.push(callback);
}

function render(node_delete_childs, html_create_parents, html_create_siblings, html_create_childs, html_create_tags, text_create_parents, text_create_siblings, text_create_childs, text_create_contents, attribute_delete_childs, attribute_delete_keys, attribute_create_childs, attribute_create_keys, attribute_create_vals) {
    try {
        for (i = 0; i < node_delete_childs.length; i++) {
            nodes[node_delete_childs[i]].remove();
            delete nodes[node_delete_childs[i]];
        }
        for (i = 0; i < html_create_parents.length; i++) {
            // super hacky way to root things
            if (html_create_tags[i] == "head") {
                nodes[html_create_childs[i]] = document.head;
            } else if (html_create_tags[i] == "body") {
                nodes[html_create_childs[i]] = document.body;
            } else {
                parent = nodes[html_create_parents[i]];
                child = document.createElement(html_create_tags[i]);
                nodes[html_create_childs[i]] = child;
                parent.insertBefore(child, nodes[html_create_siblings[i]]);
            }
        }
        for (i = 0; i < text_create_parents.length; i++) {
            parent = nodes[text_create_parents[i]];
            child = document.createTextNode(text_create_contents[i]);
            nodes[text_create_childs[i]] = child;
            parent.insertBefore(child, nodes[text_create_siblings[i]]);
        }
        for (i = 0; i < attribute_delete_childs.length; i++) {
            key = attribute_delete_keys[i];
            if (key == "class") {
                key = "className"; 
            }
            if (key.startsWith("on")) {
                nodes[attribute_delete_childs[i]].removeAttribute(key);
            } else {
                nodes[attribute_delete_childs[i]][key] = undefined;
            }
        }
        for (i = 0; i < attribute_create_childs.length; i++) {
            key = attribute_create_keys[i];
            if (key == "class") {
                key = "className"; // as far as I know this is the only property whose name differs from the matching attribute
            }
            if (key.startsWith("on")) {
                // events have to be handled by setting attributes, because the string needs to be evaled
                nodes[attribute_create_childs[i]].setAttribute(key, attribute_create_vals[i]);
            } else {
                // everything else is better off as a property
                nodes[attribute_create_childs[i]][key] = attribute_create_vals[i];
            }
        }
        while (queue.length > 0) {
            queue.pop().call();
        }
    } catch (error) {
        console.error(error);
    }
}
