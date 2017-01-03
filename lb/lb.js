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
    // console.log(data);
    render(data.removed || [], data.parent || [], data.ix || [], data.id || [], data.tag || [], data.style_id || [], data.style_key || [], data.style_val || [], data.text_id || [], data.text || [], data.on_click || [], data.on_key_down || []);
}

function errorHandler(event) {
    console.log(this, event);
}

function render(removed, parent, ix, id, tag, styleId, styleKey, styleVal, textId, text, onclick, onkeydown) {
    // console.log(arguments)
    
    trash = document.createElement("div");
    document.getElementById("root").appendChild(trash);
    
    for (var i = removed.length - 1; i >= 0; i--) {
        node = document.getElementById(removed[i]);
        trash.appendChild(node);
    }
    
    for (var i = 0; i < parent.length; i++) {
        node = document.getElementById(id[i]);
        if (node == null) {
            node = document.createElement(tag[i]);
        } else if (node.tagName != tag[i].toUpperCase()) {
            oldNode = node
            node = document.createElement(tag[i]);
            node.style = oldNode.style.cssText;
            while (oldNode.hasChildNodes()) {
                node.appendChild(oldNode.firstChild);
            }
            node.onclick = oldNode.onclick;
            node.onkeydown = oldNode.onkeydown;
        }
        node.id = id[i];
        parentNode = document.getElementById(parent[i])
        parentNode.insertBefore(node, parentNode.children[ix[i]-1]);
    }
    
    for (var i = 0; i < styleId.length; i++) {
        node = document.getElementById(styleId[i]);
        node.style[styleKey[i]] = styleVal[i];
    }
    
    for (var i = 0; i < textId.length; i++) {
        node = document.getElementById(textId[i]);
        node.textContent = text[i];
    }
    
    for (var i = 0; i < onclick.length; i++) {
        node = document.getElementById(onclick[i]);
        node.onclick = onclickHandler;
    }
    
    for (var i = 0; i < onkeydown.length; i++) {
        node = document.getElementById(onkeydown[i]);
        node.onkeydown = onkeydownHandler;
    }
    
    trash.remove();
}

function onclickHandler(event) {
    sendEvent({"click": {"id": this.id}});
    return false;
}

function onkeydownHandler(event) {
    sendEvent({"key_down": {"id": this.id, "key": event.which, "text": this.value}});
}

sendEvent({"first_render": true});
