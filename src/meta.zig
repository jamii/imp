pub fn deepEqual(a: var, b: @TypeOf(a)) bool {
    const T = @TypeOf(a);
    const ti = @typeInfo(T);
    switch (ti) {
        .Struct, .Enum, .Union => {
            if (@hasDecl(T, "deepEqual")) {
                return T.deepEqual(a,b);
            }
        },
        else => {},
    }
    switch (ti) {
        .Int, .Float, .Bool, .Enum => {
            return a == b;
        },
        .Pointer => |pti| {
            switch (pti.size) {
                .One => {
                    return deepEqual(a.*, b.*);
                },
                .Slice => {
                    if (a.len != b.len) {
                        return false;
                    }
                    for (a) |a_elem, a_ix| {
                        if (!deepEqual(a_elem, b[a_ix])) {
                            return false;
                        }
                    }
                    return true;
                },
                .Many, .C => @compileError("cannot deepEqual " ++ @typeName(T)),
            }
        },
        .Optional => {
            if (a) |a_val| {
                if (b) |b_val| {
                    return deepEqual(a_val, b_val);
                } else {
                    return false;
                }
            } else {
                return (b == null);
            }
        },
        .Array => |ati| {
            for (a) |a_elem, a_ix| {
                if (!deepEqual(a_elem, b[a_ix])) {
                    return false;
                }
            }
            return true;
        },
        .Struct => |sti| {
            inline for (sti.fields) |fti| {
                if (!deepEqual(@field(a, fti.name), @field(b, fti.name))) {
                    return false;
                }
            }
            return true;
        },
        .Union => |uti| {
            if (uti.tag_type) |tag_type| {
                inline for (uti.fields) |fti| {
                    if (@enumToInt(@as(tag_type, a)) == fti.enum_field.?.value) {
                        if (@enumToInt(@as(tag_type, b)) == fti.enum_field.?.value) {
                            return deepEqual(
                                @field(a, fti.name),
                                @field(b, fti.name),
                            );
                        } else {
                            return false;
                        }
                    }
                }
                unreachable;
            } else {
                @compileError("cannot deepEqual " ++ @typeName(T));
            }
        },
        else => @compileError("cannot deepEqual " ++ @typeName(T)),
    }
}

pub fn deepHash(key: var) u64 {
    var hasher = std.hash.Wyhash.init(0);
    deepHashInto(hasher, key);
    return hasher.final();
}

pub fn deepHashInto(hasher: var, key: var) void {
    const T = @TypeOf(key);
    const ti = @typeInfo(T);
    switch (ti) {
        .Struct, .Enum, .Union => {
            if (@hasDecl(T, "deepHashInto")) {
                if (!@hasDecl(T, "deepEqual")) {
                    @compileError(@typeName(T) ++ " has deepHashInto but no deepEqual");
                }
                return T.deepHashInto(hasher, key);
            }
        },
        else => {},
    }
    switch (ti) {
        .Int => @call(.{ .modifier = .always_inline }, hasher.update, .{std.mem.asBytes(&key)}),
        .Float => |info| deepHashInto(hasher, @bitCast(std.meta.IntType(false, info.bits), key), strat),
        .Bool => deepHashInto(hasher, @boolToInt(key)),
        .Enum => deepHashInto(hasher, @enumToInt(key)),
        .Pointer => |pti| {
            switch (pti.size) {
                .One => deepHashInto(hasher, key.*),
                .Slice => {
                    for (key) |element| {
                        deepHashInto(hasher, element);
                    }
                },
                .Many, .C => @compileError("cannot deepHash " ++ @typeName(T)),
            }
        },
        .Optional => if (key) |k| deepHashInto(hasher, k),
        .Array => {
            for (key) |element| {
                deepHashInto(hasher, element);
            }
        },
        .Struct => |info| {
            inline for (info.fields) |field| {
                deepHashInto(hasher, @field(key, field.name));
            }
        },
        .Union => |info| {
            if (info.tag_type) |tag_type| {
                const tag = meta.activeTag(key);
                deepHashInto(hasher, tag);
                inline for (info.fields) |field| {
                    const enum_field = field.enum_field.?;
                    if (enum_field.value == @enumToInt(tag)) {
                        deepHashInto(hasher, @field(key, enum_field.name));
                        return;
                    }
                }
                unreachable;
            } else @compileError("cannot deepHash " ++ @typeName(T));
        },
        else => @compileError("cannot deepHash " ++ @typeName(T)),
    }
}
