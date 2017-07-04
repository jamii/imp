document.body.innerHTML = ""; // get rid off the Blink.jl spinner

function imp_event(table) {
    return function () {
        Blink.msg("event", {"table": table, "values": Array.from(arguments)});
        return false;
    }
}

function render(node_delete_parents, node_delete_ixes, html_create_parents, html_create_ixes, html_create_childs, html_create_tags, text_create_parents, text_create_ixes, text_create_contents, attribute_delete_childs, attribute_delete_keys, attribute_create_childs, attribute_create_keys, attribute_create_vals) {
    console.time("render")
    for (i = 0; i < node_delete_parents.length; i++) {
        document.getElementById(node_delete_parents[i]).children[node_delete_ixes[i]].remove();
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
            parent.insertBefore(child, parent.children[html_create_ixes[i]]);
        }
    }
    for (i = 0; i < text_create_parents.length; i++) {
        parent = document.getElementById(text_create_parents[i]);
        child = document.createTextNode(text_create_contents[i]);
        parent.insertBefore(child, parent.children[text_create_ixes[i]]);
    }
    for (i = 0; i < attribute_delete_childs.length; i++) {
        document.getElementById(attribute_delete_childs[i]).removeAttribute(attribute_delete_keys[i]);
    }
    for (i = 0; i < attribute_create_childs.length; i++) {
        document.getElementById(attribute_create_childs[i]).setAttribute(attribute_create_keys[i], attribute_create_vals[i]);
    }
    console.timeEnd("render")
}
