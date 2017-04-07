function imp_event(table) {
    return function () {
        Blink.msg("event", {"table": table, "values": Array.from(arguments)});
        return false;
    }
}
