ws = new WebSocket("ws://localhost:8080");

function imp_event(table) {
    return function () {
        console.time("roundtrip");
        ws.send(JSON.stringify({"table": table, "values": Array.from(arguments)}));
        return false;
    }
}

ws.onerror = function (error) {
    console.log(error);
}

ws.onmessage = function (event) {
    msg = JSON.parse(event.data)
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

function render(node_delete_parents, node_delete_ixes, html_create_parents, html_create_ixes, html_create_childs, html_create_tags, text_create_parents, text_create_ixes, text_create_contents, attribute_delete_childs, attribute_delete_keys, attribute_create_childs, attribute_create_keys, attribute_create_vals) {
    console.time("render")
    try {
        for (i = 0; i < node_delete_parents.length; i++) {
            document.getElementById(node_delete_parents[i]).childNodes[node_delete_ixes[i]].remove();
        }
        for (i = 0; i < html_create_parents.length; i++) {
            // super hacky way to root things
            if (html_create_tags[i] == "head") {
                document.head.id = html_create_childs[i];
            } else if (html_create_tags[i] == "body") {
                document.body.id = html_create_childs[i];
            } else {
                parent = document.getElementById(html_create_parents[i]);
                child = document.createElement(html_create_tags[i]);
                child.id = html_create_childs[i];
                parent.insertBefore(child, parent.childNodes[html_create_ixes[i]]);
            }
        }
        for (i = 0; i < text_create_parents.length; i++) {
            parent = document.getElementById(text_create_parents[i]);
            child = document.createTextNode(text_create_contents[i]);
            parent.insertBefore(child, parent.childNodes[text_create_ixes[i]]);
        }
        for (i = 0; i < attribute_create_childs.length; i++) {
            key = attribute_create_keys[i];
            if (key == "class") {
                key = "className"; // as far as I know this is the only property whose name differs from the matching attribute
            }
            if (key.startsWith("on")) {
                // events have to be handled by setting attributes, because the string needs to be evaled
                document.getElementById(attribute_create_childs[i]).setAttribute(key, attribute_create_vals[i]);
            } else {
                // everything else is better off as a property
                document.getElementById(attribute_create_childs[i])[key] = attribute_create_vals[i];
            }
        }
        for (i = 0; i < attribute_delete_childs.length; i++) {
            key = attribute_delete_keys[i];
            if (key == "class") {
                key = "className"; 
            }
            if (key.startsWith("on")) {
                document.getElementById(attribute_delete_childs[i]).removeAttribute(key);
            } else {
                document.getElementById(attribute_delete_childs[i])[key] = undefined;
            }
        }
    } catch (error) {
        console.error(error)
    }
    console.timeEnd("render")
    console.timeEnd("roundtrip")
}
