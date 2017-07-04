document.body.innerHTML = ""; // get rid off the Blink.jl spinner

function imp_event(table) {
    return function () {
        Blink.msg("event", {"table": table, "values": Array.from(arguments)});
        return false;
    }
}

function render(deletes, creates) {
    console.time("render")
    for (i = 0; i < deletes.length; i++) {
        document.getElementById(deletes[i]).remove();
    }
    for (i = 0; i < creates.length; i++) {
        create = creates[i]
        // super hacky way to root things
        if (create[3] == "head") {
            document.head.id = create[2]
        } else if (create[3] == "body") {
            document.body.id = create[2]
        } else {
            parent = document.getElementById(create[0]);
            sibling = (create[1] == null) ? null : document.getElementById(create[1]);
            child = document.createElement(create[3]);
            child.id = create[2];
            parent.insertBefore(child, sibling);
        }
    }
    console.timeEnd("render")
}
