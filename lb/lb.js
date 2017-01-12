function sendEvent(event) {
    var request = new XMLHttpRequest();
    request.onload = requestHandler;
    request.onerror = errorHandler;
    request.open("POST", "/ui");
    request.setRequestHeader('Content-Type', 'application/json');
    request.send(JSON.stringify(event));
}

function requestHandler() {
    data = JSON.parse(this.responseText);
    console.log(data);
    for (var key in data) {
        data[key] = data[key] || [];
    }
    render(
        data.delete_node_node || [],
        data.change_tag_node || [],
        data.change_tag_tag || [],
        data.change_text_node || [],
        data.change_text_text || [],
        data.create_node_node || [],
        data.create_node_tag || [],
        data.move_node_parent || [],
        data.move_node_position || [],
        data.move_node_child || [],
        data.change_attribute_node || [],
        data.change_attribute_key || [],
        data.change_attribute_val || [],
        data.change_style_node || [],
        data.change_style_key || [],
        data.change_style_val || [],
        data.start_listening_to_node || [],
        data.start_listening_to_event || [],
        data.stop_listening_to_node || [],
        data.stop_listening_to_event || []
    );
}

function errorHandler(event) {
    console.log(this, event);
}

function render(
    delete_node_node,
    change_tag_node,
    change_tag_tag,
    change_text_node,
    change_text_text,
    create_node_node,
    create_node_tag,
    move_node_parent,
    move_node_position,
    move_node_child,
    change_attribute_node,
    change_attribute_key,
    change_attribute_val,
    change_style_node,
    change_style_key,
    change_style_val,
    start_listening_to_node,
    start_listening_to_event,
    stop_listening_to_node,
    stop_listening_to_event
) {
    for (var i = 0; i < delete_node_node.length; i++) {
        document.getElementById(delete_node_node[i]).remove();
    }
    
    for (var i = 0; i < change_tag_node.length; i++) {
        oldNode = document.getElementById(change_tag_node[i]);
        node = document.createElement(change_tag_tag[i]);
        node.style = oldNode.style.cssText;
        while (oldNode.hasChildNodes()) {
            node.appendChild(oldNode.firstChild);
        }
        node.onclick = oldNode.onclick;
        node.onkeydown = oldNode.onkeydown;
        node.onchange = oldNode.onchange;
        oldNode.replaceWith(node);
    }
    
    nursery = document.createElement("div");
    document.getElementById("root").appendChild(nursery);
    
    for (var i = 0; i < create_node_node.length; i++) {
        node = document.createElement(create_node_tag[i]);
        node.id = create_node_node[i];
        nursery.appendChild(node);
    }
    
    for (var i = 0; i < change_text_node.length; i++) {
        document.getElementById(change_text_node[i]).textContent = change_text_text[i];
    }
    
    for (var i = 0; i < move_node_parent.length; i++) {
        child = document.getElementById(move_node_child[i]);
        nursery.appendChild(child);
    }
    
    for (var i = 0; i < move_node_parent.length; i++) {
        child = document.getElementById(move_node_child[i]);
        parent = document.getElementById(move_node_parent[i]);
        parent.insertBefore(child, parent.children[move_node_position[i]]);
    }
    
    nursery.remove();
    
    for (var i = 0; i < change_attribute_node.length; i++) {
        node = document.getElementById(change_attribute_node[i]);
        node[change_attribute_key[i]] = change_attribute_val[i];
    }
    
    for (var i = 0; i < change_style_node.length; i++) {
        node = document.getElementById(change_style_node[i]);
        node.style[change_style_key[i]] = change_style_val[i];
    }
    
    for (var i = 0; i < start_listening_to_node.length; i++) {
        node = document.getElementById(start_listening_to_node[i]);
        switch (start_listening_to_event[i]) {
            case "click":
              node.onclick = onclickHandler;
              break;
              
            case "key_down":
              node.onkeydown = onkeydownHandler;
              break;
              
            case "change":
              node.onchange = onchangeHandler;
              break;
        }
    }
    
    for (var i = 0; i < stop_listening_to_node.length; i++) {
        node = document.getElementById(stop_listening_to_node[i]);
        switch (stop_listening_to_event[i]) {
            case "click":
              node.onclick = null;
              break;
              
            case "key_down":
              node.onkeydown = null;
              break;
              
            case "change":
              node.onchange = null;
              break;
        }
    }
}

function onclickHandler(event) {
    sendEvent({"click": {"node": this.id}});
}

function onkeydownHandler(event) {
    sendEvent({
        "key_down": {"node": this.id, "key": event.which},
        "change": {"node": this.id, "text": this.value}
    });
}

function onchangeHandler(event) {
    sendEvent({"change": {"node": this.id, "text": this.value}});
}

sendEvent({"first_render": true});
