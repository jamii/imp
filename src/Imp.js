function onclickHandler(event) {
    Blink.msg("event", {"table": "click", "values": [this.id]});
    return false;
}

function onkeydownHandler(event) {
    Blink.msg("event", {"table": "keydown", "values": [this.id, event.which, this.value]});
}

function render(removed, parent, ix, id, tagName, styleId, styleKey, styleVal, textContentId, textContent, onclick, onkeydown) {
    console.log(arguments)
    
    trash = document.createElement("div");
    document.getElementById("root").appendChild(trash);
    
    for (var i = removed.length - 1; i >= 0; i--) {
        node = document.getElementById(removed[i]);
        trash.appendChild(node);
    }
    
    for (var i = 0; i < parent.length; i++) {
        node = document.getElementById(id[i]);
        if (node == null) {
            node = document.createElement(tagName[i]);
        } else if (node.tagName != tagName[i].toUpperCase()) {
            oldNode = node
            node = document.createElement(tagName[i]);
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
    
    for (var i = 0; i < textContentId.length; i++) {
        node = document.getElementById(textContentId[i]);
        node.textContent = textContent[i];
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
