const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

pub const CodeGeneratorOptions = struct {
    include_paths: []const []const u8,
    header_paths: []const []const u8,
    zigft_path: []const u8 = "",
    translater: []const u8 = "c_to_zig",
    error_set: []const u8 = "Error",
    c_error_type: ?[]const u8 = null,
    c_import: []const u8 = "c",
    c_root_struct: ?[]const u8 = null,
    add_simple_test: bool = true,

    // callback determining which declarations to include
    filter_fn: fn (name: []const u8) bool,

    // callback determining to const pointer to struct should become by-value
    type_is_by_value_fn: fn (type_name: []const u8) bool = neverByValue,

    // callback determining which enum items represent errors
    enum_is_error_fn: fn (item_name: []const u8, value: i128) bool = isNonZero,
    // callback determining if an enum type is a packed struct
    enum_is_packed_struct_fn: fn (enum_name: []const u8) bool = neverPackedStruct,

    // callbacks determining particular pointer attributes
    ptr_is_many_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = isTargetChar,
    ptr_is_null_terminated_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = isTargetChar,
    ptr_is_optional_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = neverOptional,

    // callback determining whether ptr param is optional
    param_is_optional_fn: fn (fn_name: []const u8, param_name: ?[]const u8, param_index: usize, param_type: []const u8) ?bool = notFunctionSpecific,
    // callback distinguishing in/out pointers from output pointers
    param_is_input_fn: fn (fn_name: []const u8, param_name: ?[]const u8, param_index: usize, param_type: []const u8) bool = neverInput,
    // callback returning the index of the corresponding pointer argument if it should be treated as the length of a slice pointer
    param_is_slice_len_fn: fn (fn_name: []const u8, param_name: ?[]const u8, param_index: usize, param_type: []const u8) ?usize = neverSliceLength,

    // calling determining whether positive status is needed
    status_is_returned_fn: fn (fn_name: []const u8) bool = neverReturned,
    // calling determining whether a fucntion should return error union is needed
    error_union_is_returned_fn: fn (fn_name: []const u8) bool = alwaysReturned,

    // callbacks adjusting naming convention
    fn_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,
    type_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,
    const_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,
    param_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = removeArgPrefix,
    field_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,
    enum_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,
    error_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = makeNoChange,

    // callback returning doc comment
    doc_comment_fn: fn (std.mem.Allocator, old_name: []const u8, new_name: []const u8) std.mem.Allocator.Error!?[]const u8 = provideNoComment,
};

pub const inout = TypeWithAttributes.inout;

pub const TypeWithAttributes = struct {
    type: type,
    is_inout: bool = false,

    fn inout(comptime T: type) @This() {
        return .{ .type = T, .is_inout = true };
    }

    fn get(comptime arg: anytype) @This() {
        return switch (@TypeOf(arg)) {
            type => .{ .type = arg },
            @This() => arg,
            else => @compileError("Type expected, found  '" ++ @typeName(@TypeOf(arg)) ++ "'"),
        };
    }
};
pub const TypeSubstitution = struct {
    old: type,
    new: type,
};
pub const TranslatorOptions = struct {
    substitutions: []const TypeSubstitution = &.{},
    c_import_ns: type,
    late_bind_fn: ?fn ([]const u8) *const anyopaque = null,
    error_scheme: type,
};
pub fn BasicErrorScheme(
    old_enum_type: type,
    new_error_set: type,
    default_error: new_error_set,
) type {
    @setEvalBranchQuota(2000000);
    const es_info = @typeInfo(new_error_set);
    if (es_info != .error_set) @compileError("Error set expected, found '" ++ @typeName(new_error_set) ++ "'");
    const en_info = @typeInfo(old_enum_type);
    const en_count = switch (en_info) {
        .@"enum" => |en| en.fields.len,
        .int, .bool => 1,
        else => @compileError("Enum, int, or bool expected, found '" ++ @typeName(old_enum_type) ++ "'"),
    };
    const error_set = es_info.error_set orelse &.{};
    var error_enum_buffer: [error_set.len]struct {
        status: old_enum_type,
        err: new_error_set,
    } = undefined;
    var signatures: [error_set.len]comptime_int = undefined;
    for (error_set, 0..) |e, index| {
        error_enum_buffer[index].err = @field(new_error_set, e.name);
        signatures[index] = asComptimeInt(e.name);
    }
    var non_error_status_buffer: [en_count]old_enum_type = undefined;
    var non_error_status_count = 0;
    switch (en_info) {
        .@"enum" => |en| {
            for (en.fields) |field| {
                const status = @field(old_enum_type, field.name);
                const sig = asComptimeInt(field.name);
                if (std.mem.indexOfScalar(comptime_int, &signatures, sig)) |i| {
                    error_enum_buffer[i].status = status;
                } else {
                    non_error_status_buffer[non_error_status_count] = status;
                    non_error_status_count += 1;
                }
            }
            if (non_error_status_count == 0) @compileError("No success status");
        },
        .int => {
            error_enum_buffer[0].status = 1;
            non_error_status_buffer[0] = 0;
            non_error_status_count += 1;
        },
        .bool => {
            error_enum_buffer[0].status = false;
            non_error_status_buffer[0] = true;
            non_error_status_count += 1;
        },
        else => unreachable,
    }
    const non_error_statuses = init: {
        var list: [non_error_status_count]old_enum_type = undefined;
        @memcpy(&list, non_error_status_buffer[0..non_error_status_count]);
        break :init list;
    };
    const error_enum_table = error_enum_buffer;
    return struct {
        pub const Status = old_enum_type;
        pub const ErrorSet = new_error_set;

        pub fn RetvalSubstitute(comptime T: type) type {
            return switch (@typeInfo(Status)) {
                .@"enum" => |en| if (en.tag_type == T) Status else T,
                else => T,
            };
        }

        pub fn OutputType(comptime T: type) type {
            return if (T == Status and non_error_statuses.len <= 1) void else T;
        }

        pub fn check(retval: anytype) ErrorSet!OutputType(@TypeOf(retval)) {
            const T = @TypeOf(retval);
            return switch (T) {
                Status => if (std.mem.indexOfScalar(Status, &non_error_statuses, retval)) |_|
                    if (OutputType(T) == void) {} else retval
                else for (error_enum_table) |entry| {
                    if (entry.status == retval) break entry.err;
                } else default_error,
                else => retval,
            };
        }
    };
}
pub const NullErrorScheme = struct {};

pub fn Translator(comptime options: TranslatorOptions) type {
    return struct {
        pub fn Translated(
            comptime OldFn: type,
            comptime return_error_union: bool,
            comptime ignore_non_error_return_value: bool,
            comptime local_subs: anytype,
        ) type {
            @setEvalBranchQuota(2000000);
            const old_fn = @typeInfo(OldFn).@"fn";
            const OldRT = old_fn.return_type.?;
            const NewRT = RetvalSubstitute(OldRT, local_subs);
            const extra = switch (ignore_non_error_return_value) {
                true => 0,
                false => switch (return_error_union) {
                    // room for an extra type when there're multiple status codes indicating success
                    true => if (options.error_scheme.OutputType(NewRT) != void) 1 else 0,
                    // room for an extra type when the return value isn't an error code or void
                    false => if (NewRT != void) 1 else 0,
                },
            };
            // look for non-const pointers, scanning backward
            const OutputTypes = init: {
                var types: [old_fn.params.len + extra]type = undefined;
                if (extra == 1) types[old_fn.params.len] = NewRT;
                const start_index = inline for (0..old_fn.params.len) |j| {
                    const i = old_fn.params.len - j - 1;
                    const Target = WritableTarget(old_fn.params[i].type.?) orelse break i + 1;
                    // see if the pointer is attributed as in/out
                    if (getTypeWithAttributes(local_subs, i, old_fn.params.len)) |type_wa| {
                        if (type_wa.is_inout) break i + 1;
                    }
                    types[i] = Substitute(Target, local_subs, i, old_fn.params.len);
                } else 0;
                break :init types[start_index..];
            };
            const param_count = old_fn.params.len + extra - OutputTypes.len;
            var params: [param_count]std.builtin.Type.Fn.Param = undefined;
            inline for (old_fn.params, 0..) |param, index| {
                if (index < param_count) {
                    params[index] = .{
                        .type = Substitute(param.type.?, local_subs, index, old_fn.params.len),
                        .is_generic = false,
                        .is_noalias = false,
                    };
                }
            }
            // determine the payload of the return type
            const Payload = switch (OutputTypes.len) {
                0 => void,
                1 => OutputTypes[0],
                else => std.meta.Tuple(OutputTypes),
            };
            return @Type(.{
                .@"fn" = .{
                    .calling_convention = .auto,
                    .is_generic = false,
                    .is_var_args = false,
                    .return_type = switch (return_error_union) {
                        true => options.error_scheme.ErrorSet!Payload,
                        else => Payload,
                    },
                    .params = &params,
                },
            });
        }

        pub fn translate(
            comptime fn_name: []const u8,
            comptime return_error_union: bool,
            comptime ignore_non_error_return_value: bool,
            comptime local_subs: anytype,
        ) Translated(
            @TypeOf(@field(options.c_import_ns, fn_name)),
            return_error_union,
            ignore_non_error_return_value,
            local_subs,
        ) {
            @setEvalBranchQuota(2000000);
            const OldFn = @TypeOf(@field(options.c_import_ns, fn_name));
            const NewFn = Translated(
                OldFn,
                return_error_union,
                ignore_non_error_return_value,
                local_subs,
            );
            const OldRT = @typeInfo(OldFn).@"fn".return_type.?;
            // NewRT is OldRT in the new namespace (usually an enum)
            const NewRT = RetvalSubstitute(OldRT, local_subs);
            // ReturnType is what the new function actually returns
            const ReturnType = @typeInfo(NewFn).@"fn".return_type.?;
            const Payload = switch (@typeInfo(ReturnType)) {
                .error_union => |eu| eu.payload,
                else => ReturnType,
            };
            const ns = struct {
                inline fn call(new_args: std.meta.ArgsTuple(NewFn)) ReturnType {
                    var old_args: std.meta.ArgsTuple(OldFn) = undefined;
                    // copy arguments
                    inline for (new_args, 0..) |new_arg, i| {
                        const ArgT = @TypeOf(old_args[i]);
                        old_args[i] = convert(ArgT, new_arg);
                    }
                    var payload: Payload = if (Payload == void) {} else undefined;
                    // see how many pointers and tuple fields there're
                    const pointer_count = old_args.len - new_args.len;
                    const output_count = switch (@typeInfo(Payload)) {
                        .@"struct" => |st| if (st.is_tuple) payload.len else 1,
                        .void => 0,
                        else => 1,
                    };
                    const extra = output_count - pointer_count;
                    // add pointers to result
                    switch (pointer_count) {
                        1 => old_args[new_args.len] = @ptrCast(&payload),
                        else => inline for (new_args.len..old_args.len) |i| {
                            const ArgT = @TypeOf(old_args[i]);
                            old_args[i] = convert(ArgT, &payload[i - new_args.len]);
                        },
                    }
                    // get original function
                    const func = if (options.late_bind_fn) |get| bind: {
                        const bind_ns = struct {
                            var func_ptr: ?*const OldFn = null;
                        };
                        if (bind_ns.func_ptr) |ptr| {
                            break :bind ptr;
                        } else {
                            const ptr: *const OldFn = @ptrCast(get(fn_name));
                            bind_ns.func_ptr = ptr;
                            break :bind ptr;
                        }
                    } else @field(options.c_import_ns, fn_name);
                    // call original function
                    const old_rv = @call(.auto, func, old_args);
                    const new_rv = convert(NewRT, old_rv);
                    // check outcome (if returning error union)
                    const output = switch (return_error_union) {
                        true => try options.error_scheme.check(new_rv),
                        false => new_rv,
                    };
                    if (extra > 0) {
                        // add positive status to result
                        if (output_count > 1)
                            payload[payload.len - 1] = output
                        else
                            payload = output;
                    }
                    return payload;
                }
            };
            return fn_transform.spreadArgs(ns.call, .auto);
        }

        pub fn SliceType(comptime T: type) type {
            return switch (@typeInfo(T)) {
                .pointer => |pt| define: {
                    var new_pt = pt;
                    new_pt.size = .slice;
                    new_pt.sentinel_ptr = null;
                    if (@typeInfo(new_pt.child) == .@"opaque") new_pt.child = u8;
                    break :define @Type(.{ .pointer = new_pt });
                },
                .optional => |op| ?SliceType(op.child),
                else => @compileError("Argument is not a pointer"),
            };
        }

        pub fn SliceMerged(
            comptime OldFn: type,
            comptime pairs: []const SplitSlice,
        ) type {
            @setEvalBranchQuota(2000000);
            const old_fn = switch (@typeInfo(OldFn)) {
                .@"fn" => |f| f,
                else => @compileError("Function type expected, received '" ++ @typeName(OldFn) ++ "'"),
            };
            const param_count = old_fn.params.len - pairs.len;
            var new_params: [param_count]std.builtin.Type.Fn.Param = undefined;
            var j: usize = 0;
            inline for (old_fn.params, 0..) |param, i| {
                const PT = param.type orelse @compileError("Cannot merge generic argument");
                const is_index = inline for (pairs) |pair| {
                    if (pair.len_index == i) break true;
                } else false;
                if (!is_index) {
                    new_params[j] = param;
                    const is_ptr = inline for (pairs) |pair| {
                        if (pair.ptr_index == i) break true;
                    } else false;
                    if (is_ptr) new_params[j].type = SliceType(PT);
                    j += 1;
                } else {
                    switch (@typeInfo(PT)) {
                        .int => {},
                        else => @compileError("Argument is not an integer"),
                    }
                }
            }
            var new_fn = old_fn;
            new_fn.params = &new_params;
            return @Type(.{ .@"fn" = new_fn });
        }

        pub fn mergeSlice(
            comptime func: anytype,
            comptime pairs: []const SplitSlice,
        ) SliceMerged(@TypeOf(func), pairs) {
            @setEvalBranchQuota(2000000);
            const OldFn = @TypeOf(func);
            const NewFn = SliceMerged(OldFn, pairs);
            const NewRT = @typeInfo(NewFn).@"fn".return_type.?;
            const cc = @typeInfo(NewFn).@"fn".calling_convention;
            const ns = struct {
                fn call(new_args: std.meta.ArgsTuple(NewFn)) NewRT {
                    var old_args: std.meta.ArgsTuple(OldFn) = undefined;
                    // copy arguments
                    comptime var j: usize = 0;
                    inline for (0..old_args.len) |i| {
                        const is_index = inline for (pairs) |pair| {
                            if (pair.len_index == i) break true;
                        } else false;
                        if (!is_index) {
                            // see if it's a slice pointer
                            const len_index: ?usize = inline for (pairs) |pair| {
                                if (pair.ptr_index == i) break pair.len_index;
                            } else null;
                            if (len_index) |k| {
                                // copy len and pointer
                                const new_arg = new_args[j];
                                const AT = @TypeOf(new_arg);
                                switch (@typeInfo(AT)) {
                                    .pointer => {
                                        old_args[i] = @ptrCast(new_arg.ptr);
                                        old_args[k] = @intCast(new_arg.len);
                                    },
                                    .optional => {
                                        if (new_arg) |s| {
                                            old_args[i] = @ptrCast(s.ptr);
                                            old_args[k] = @intCast(s.len);
                                        } else {
                                            old_args[i] = null;
                                            old_args[k] = 0;
                                        }
                                    },
                                    else => unreachable,
                                }
                            } else {
                                old_args[i] = new_args[j];
                            }
                            j += 1;
                        }
                    }
                    // call original function
                    return @call(.always_inline, func, old_args);
                }
            };
            return fn_transform.spreadArgs(ns.call, cc);
        }

        pub fn translateMerge(
            comptime fn_name: []const u8,
            comptime return_error_union: bool,
            comptime ignore_non_error_return_value: bool,
            comptime local_subs: anytype,
            comptime pairs: []const SplitSlice,
        ) SliceMerged(Translated(
            @TypeOf(@field(options.c_import_ns, fn_name)),
            return_error_union,
            ignore_non_error_return_value,
            local_subs,
        ), pairs) {
            const f = translate(fn_name, return_error_union, ignore_non_error_return_value, local_subs);
            return mergeSlice(f, pairs);
        }

        inline fn convert(comptime T: type, arg: anytype) T {
            const AT = @TypeOf(arg);
            return switch (@typeInfo(T)) {
                .int => switch (@typeInfo(AT)) {
                    .@"enum" => @intFromEnum(arg),
                    .bool => if (arg) 1 else 0,
                    else => @bitCast(arg),
                },
                .pointer => switch (@typeInfo(AT)) {
                    .pointer => @ptrCast(arg),
                    // converting "pass-by-value" to "pass-by-pointer"
                    else => convert(T, &arg),
                },
                inline .@"struct", .@"union" => switch (@typeInfo(AT)) {
                    .pointer => convert(T, arg.*),
                    else => @bitCast(arg),
                },
                .optional => |op| switch (@typeInfo(AT)) {
                    .optional => if (arg) |a| convert(op.child, a) else null,
                    else => convert(op.child, arg),
                },
                .@"enum" => @enumFromInt(arg),
                else => @bitCast(arg),
            };
        }

        fn SwapType(comptime T: type, comptime dir: enum { old_to_new, new_to_old }) type {
            return inline for (options.substitutions) |sub| {
                if (dir == .old_to_new) {
                    if (T == sub.old) break sub.new;
                } else {
                    if (T == sub.new) break sub.old;
                }
            } else T;
        }

        fn getTypeWithAttributes(tuple: anytype, arg_index: ?usize, arg_count: usize) ?TypeWithAttributes {
            const keys = if (arg_index) |index| .{
                std.fmt.comptimePrint("{d}", .{index}),
                std.fmt.comptimePrint("{d}", .{-@as(isize, @intCast(arg_count - index))}),
            } else .{"retval"};
            return inline for (keys) |key| {
                if (@hasField(@TypeOf(tuple), key)) {
                    break TypeWithAttributes.get(@field(tuple, key));
                }
            } else null;
        }

        fn Substitute(comptime T: type, tuple: anytype, arg_index: ?usize, arg_count: usize) type {
            // look for type in function-specific tuple first
            return if (getTypeWithAttributes(tuple, arg_index, arg_count)) |type_wa|
                type_wa.type
            else
                SwapType(T, .old_to_new);
        }

        fn RetvalSubstitute(comptime T: type, tuple: anytype) type {
            return if (@hasDecl(options.error_scheme, "RetvalSubstitute"))
                options.error_scheme.RetvalSubstitute(T)
            else
                Substitute(T, tuple, null, 0);
        }

        fn WritableTarget(comptime T: type) ?type {
            const info = @typeInfo(T);
            if (info == .pointer and !info.pointer.is_const) {
                const Target = info.pointer.child;
                if (@typeInfo(Target) != .@"opaque" and @sizeOf(Target) != 0) return Target;
            }
            return null;
        }
    };
}

pub const Type = union(enum) {
    container: struct {
        layout: ?[]const u8 = null,
        backing_type: ?[]const u8 = null,
        kind: []const u8,
        fields: []Field = &.{},
        decls: []Declaration = &.{},
    },
    pointer: struct {
        child_type: *Type,
        alignment: ?[]const u8,
        sentinel: ?[]const u8,
        size: std.builtin.Type.Pointer.Size,
        is_const: bool,
        is_volatile: bool,
        is_optional: bool,
        allows_zero: bool,
    },
    enumeration: struct {
        items: []EnumItem = &.{},
        is_signed: bool,
        is_exhaustive: bool = false,
    },
    function: struct {
        parameters: []Parameter = &.{},
        return_type: *Type,
        alignment: ?[]const u8,
        call_convention: ?[]const u8,
    },
    error_union: struct {
        payload_type: *Type,
        error_set: *Type,
    },
    error_set: [][]const u8,
    optional: *Type,
    type_tuple: []*Type,
    expression: Expression,
};
pub const Field = struct {
    name: []const u8,
    type: *Type,
    alignment: ?[]const u8 = null,
    default_value: ?[]const u8 = null,
};
pub const Declaration = struct {
    name: []const u8,
    type: ?*Type = null,
    alignment: ?[]const u8 = null,
    mutable: bool = false,
    expr: Expression = .{ .unknown = "" },
    doc_comment: ?[]const u8 = null,
};
pub const Parameter = struct {
    type: *Type,
    name: ?[]const u8 = null,
    is_inout: bool = false,
};
pub const EnumItem = struct {
    name: []const u8,
    value: i128,
};
pub const Expression = union(enum) {
    type: *Type,
    identifier: []const u8,
    unknown: []const u8,
};
pub const NamespaceType = enum { old, new };
pub const Namespace = struct {
    to_type: std.StringHashMap(*Type),
    to_name: std.AutoHashMap(*Type, []const u8),

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .to_type = .init(allocator),
            .to_name = .init(allocator),
        };
    }

    pub fn getType(self: *const @This(), name: []const u8) ?*Type {
        return self.to_type.get(name);
    }

    pub fn getName(self: *const @This(), t: *Type) ?[]const u8 {
        return self.to_name.get(t);
    }

    pub fn addType(self: *@This(), name: []const u8, t: *Type) !void {
        try self.to_type.put(name, t);
        try self.to_name.put(t, name);
    }

    pub fn removeType(self: *@This(), t: *Type) void {
        if (self.to_name.get(t)) |name| {
            _ = self.to_name.remove(t);
            _ = self.to_type.remove(name);
        }
    }

    pub fn removeName(self: *@This(), name: []const u8) void {
        if (self.to_type.get(name)) |t| {
            _ = self.to_type.remove(name);
            _ = self.to_name.remove(t);
        }
    }
};
const SplitSlice = struct {
    len_index: usize,
    ptr_index: usize,
};
const LocalSubstitution = struct {
    index: ?usize,
    type_name: []const u8,
};

pub fn CodeGenerator(comptime options: CodeGeneratorOptions) type {
    return struct {
        const Ast = std.zig.Ast;
        const NameContext = enum {
            @"fn",
            type,
            @"const",
            param,
            field,
            @"enum",
            @"error",
        };

        arena: std.heap.ArenaAllocator,
        allocator: std.mem.Allocator,
        cwd: []const u8,
        indent_level: usize,
        indented: bool,
        old_root: *Type,
        old_namespace: Namespace,
        old_to_new_type_map: std.AutoHashMap(*Type, *Type),
        old_to_new_param_map: std.AutoHashMap(*Type, *Type),
        new_root: *Type,
        new_namespace: Namespace,
        new_error_set: *Type,
        error_enums: []const []const u8,
        non_error_enums: []const []const u8,
        void_type: *Type,
        type_lookup: ?*Type,
        write_to_byte_array: bool,
        byte_array: std.ArrayList(u8),
        output_writer: std.io.AnyWriter,
        need_inout_import: bool,

        pub fn init(allocator: std.mem.Allocator) !*@This() {
            var arena: std.heap.ArenaAllocator = .init(allocator);
            var self = try arena.allocator().create(@This());
            self.arena = arena;
            self.allocator = self.arena.allocator();
            self.cwd = try std.process.getCwdAlloc(self.allocator);
            self.indent_level = 0;
            self.indented = false;
            self.old_root = try self.createType(.{ .container = .{ .kind = "struct" } });
            self.old_namespace = .init(self.allocator);
            self.old_to_new_type_map = .init(self.allocator);
            self.old_to_new_param_map = .init(self.allocator);
            self.new_root = try self.createType(.{ .container = .{ .kind = "struct" } });
            self.new_namespace = .init(self.allocator);
            self.new_error_set = try self.createType(.{ .error_set = &.{} });
            self.error_enums = &.{};
            self.non_error_enums = &.{};
            self.void_type = try self.createType(.{ .expression = .{ .identifier = "void" } });
            self.type_lookup = null;
            self.write_to_byte_array = false;
            self.byte_array = .init(self.allocator);
            self.need_inout_import = false;
            return self;
        }

        pub fn deinit(self: *@This()) void {
            var arena = self.arena;
            arena.deinit();
        }

        pub fn analyze(self: *@This()) !void {
            try self.processHeaderFiles();
            try self.translateDeclarations();
        }

        pub fn print(self: *@This(), writer: anytype) anyerror!void {
            self.output_writer = writer.any();
            try self.printImports();
            try self.printTypeDef(self.new_root, .new);
            try self.printTrainslatorSetup();
            if (options.add_simple_test) try self.printSimpleTest();
        }

        fn processHeaderFiles(self: *@This()) !void {
            for (options.header_paths) |path| {
                const full_path = try self.findSourceFile(path);
                const output = try self.translateHeaderFile(full_path);
                const source = try self.allocator.dupeZ(u8, output);
                const tree = try Ast.parse(self.allocator, source, .zig);
                for (tree.rootDecls()) |node| {
                    var buffer1: [1]Ast.Node.Index = undefined;
                    if (tree.fullFnProto(&buffer1, node)) |proto| {
                        try self.processFnProto(tree, proto);
                    } else if (tree.fullVarDecl(node)) |decl| {
                        try self.processVarDecl(tree, decl);
                    }
                }
            }
        }

        fn processFnProto(self: *@This(), tree: Ast, proto: Ast.full.FnProto) !void {
            if (proto.visib_token == null or proto.name_token == null) return;
            try self.addDeclaration(.{
                .name = tree.tokenSlice(proto.name_token.?),
                .type = try self.obtainFunctionType(tree, proto),
            });
        }

        fn processVarDecl(self: *@This(), tree: Ast, decl: Ast.full.VarDecl) !void {
            if (decl.visib_token == null) return;
            try self.addDeclaration(.{
                .name = tree.tokenSlice(decl.ast.mut_token + 1),
                .mutable = std.mem.eql(u8, "var", tree.tokenSlice(decl.ast.mut_token)),
                .type = switch (decl.ast.type_node) {
                    0 => null,
                    else => try self.obtainType(tree, decl.ast.type_node),
                },
                .alignment = nodeSlice(tree, decl.ast.align_node),
                .expr = try self.obtainExpression(tree, decl.ast.init_node, true),
            });
        }

        fn obtainExpression(self: *@This(), tree: Ast, node: Ast.Node.Index, is_rhs: bool) !Expression {
            var buffer1: [1]Ast.Node.Index = undefined;
            var buffer2: [2]Ast.Node.Index = undefined;
            if (node == 0) return .{ .unknown = "" };
            return if (tree.fullFnProto(&buffer1, node)) |fn_proto| .{
                .type = try self.obtainFunctionType(tree, fn_proto),
            } else if (tree.fullContainerDecl(&buffer2, node)) |decl| .{
                .type = try self.obtainContainerType(tree, decl),
            } else if (self.detectPointerType(tree, node)) |p| .{
                .type = try self.obtainPointerType(tree, p.ptr_type, p.is_optional),
            } else if (self.detectEnumType(tree, node, is_rhs)) |e| .{
                .type = try self.obtainEnumType(tree, e.item_count, e.is_signed),
            } else if (tree.nodes.items(.tag)[node] == .identifier) .{
                .identifier = nodeSlice(tree, node).?,
            } else .{
                .unknown = nodeSlice(tree, node).?,
            };
        }

        fn obtainType(self: *@This(), tree: Ast, node: Ast.Node.Index) error{OutOfMemory}!*Type {
            const expr = try self.obtainExpression(tree, node, false);
            return switch (expr) {
                .type => |t| t,
                .identifier => |i| self.old_namespace.getType(i) orelse add: {
                    const t = try self.createType(.{ .expression = expr });
                    try self.old_namespace.addType(i, t);
                    break :add t;
                },
                .unknown => try self.createType(.{ .expression = expr }),
            };
        }

        fn obtainFunctionType(self: *@This(), tree: Ast, proto: Ast.full.FnProto) !*Type {
            var params: []Parameter = &.{};
            for (proto.ast.params) |param| {
                // see if there's a colon in front of the param type
                const before = tree.tokenSlice(tree.firstToken(param) - 1);
                try self.append(&params, .{
                    .name = switch (std.mem.eql(u8, before, ":")) {
                        true => tree.tokenSlice(tree.firstToken(param) - 2),
                        false => null,
                    },
                    .type = try self.obtainType(tree, param),
                });
            }

            return try self.createType(.{
                .function = .{
                    .parameters = params,
                    .return_type = try self.obtainType(tree, proto.ast.return_type),
                    .alignment = nodeSlice(tree, proto.ast.align_expr),
                    .call_convention = nodeSlice(tree, proto.ast.callconv_expr),
                },
            });
        }

        fn obtainContainerType(self: *@This(), tree: Ast, decl: Ast.full.ContainerDecl) !*Type {
            var fields: []Field = &.{};
            for (decl.ast.members) |member| {
                if (tree.fullContainerField(member)) |field| {
                    try self.append(&fields, .{
                        .name = tree.tokenSlice(field.ast.main_token),
                        .type = try self.obtainType(tree, field.ast.type_expr),
                        .alignment = nodeSlice(tree, field.ast.align_expr),
                    });
                }
            }
            return try self.createType(.{
                .container = .{
                    .layout = if (decl.layout_token) |t| tree.tokenSlice(t) else null,
                    .kind = tree.tokenSlice(decl.ast.main_token),
                    .fields = fields,
                },
            });
        }

        fn obtainPointerType(self: *@This(), tree: Ast, ptr_type: Ast.full.PtrType, is_optional: bool) !*Type {
            return try self.createType(.{
                .pointer = .{
                    .child_type = try self.obtainType(tree, ptr_type.ast.child_type),
                    .sentinel = nodeSlice(tree, ptr_type.ast.sentinel),
                    .size = ptr_type.size,
                    .is_const = ptr_type.const_token != null,
                    .is_volatile = ptr_type.volatile_token != null,
                    .is_optional = is_optional,
                    .allows_zero = ptr_type.allowzero_token != null,
                    .alignment = nodeSlice(tree, ptr_type.ast.align_node),
                },
            });
        }

        fn obtainEnumType(self: *@This(), _: Ast, count: usize, is_signed: bool) !*Type {
            // remove decls containing integers and use them as the enum values
            const index: usize = self.old_root.container.decls.len - count;
            var items: []EnumItem = &.{};
            for (0..count) |_| {
                const decl = self.old_root.container.decls[index];
                const value = std.fmt.parseInt(i128, decl.expr.unknown, 10) catch unreachable;
                try self.append(&items, .{ .name = decl.name, .value = value });
                self.remove(&self.old_root.container.decls, index);
            }
            return try self.createType(.{
                .enumeration = .{
                    .items = items,
                    .is_signed = is_signed,
                },
            });
        }

        fn detectPointerType(_: *@This(), tree: Ast, node: Ast.Node.Index) ?struct {
            ptr_type: Ast.full.PtrType,
            is_optional: bool,
        } {
            if (tree.fullPtrType(node)) |pt| return .{
                .ptr_type = pt,
                .is_optional = false,
            } else {
                const tag = tree.nodes.items(.tag)[node];
                if (tag == .optional_type) {
                    const data = tree.nodes.items(.data)[node];
                    if (tree.fullPtrType(data.lhs)) |pt| return .{
                        .ptr_type = pt,
                        .is_optional = true,
                    };
                }
            }
            return null;
        }

        fn detectEnumType(self: *@This(), tree: Ast, node: Ast.Node.Index, is_rhs: bool) ?struct {
            item_count: usize,
            is_signed: bool,
        } {
            if (!is_rhs) return null;
            // C enums get translated as either c_uint or c_int
            const rhs = nodeSlice(tree, node).?;
            const is_signed_int = std.mem.eql(u8, rhs, "c_int");
            const is_unsigned_int = std.mem.eql(u8, rhs, "c_uint");
            if (is_signed_int or is_unsigned_int) {
                // enum items are declared ahead of the type;
                // scan backward looking for int values
                const decls = self.old_root.container.decls;
                var count: usize = 0;
                while (count + 1 < decls.len) : (count += 1) {
                    const decl = decls[decls.len - count - 1];
                    if (!decl.mutable and decl.expr == .unknown) {
                        _ = std.fmt.parseInt(i128, decl.expr.unknown, 10) catch break;
                    } else break;
                }
                if (count > 0) return .{
                    .item_count = count,
                    .is_signed = is_signed_int,
                };
            }
            return null;
        }

        fn addDeclaration(self: *@This(), decl: Declaration) !void {
            var copy = decl;
            switch (decl.expr) {
                .type => |t| {
                    if (self.old_namespace.getType(decl.name)) |existing_t| {
                        // copy type info into the Type object that's there because
                        // of forward declaration
                        existing_t.* = decl.expr.type.*;
                        copy.expr.type = existing_t;
                    } else {
                        // add the type under the decl name
                        try self.old_namespace.addType(decl.name, t);
                    }
                },
                .identifier => {
                    // assume that it's referring to a type
                    const t = try self.createType(.{ .expression = decl.expr });
                    try self.old_namespace.addType(decl.name, t);
                },
                else => {},
            }
            try self.append(&self.old_root.container.decls, copy);
        }

        fn createType(self: *@This(), info: Type) !*Type {
            const t = try self.allocator.create(Type);
            t.* = info;
            return t;
        }

        fn nodeSlice(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
            if (node == 0) return null;
            const span = tree.nodeToSpan(node);
            return tree.source[span.start..span.end];
        }

        fn transformName(self: *@This(), name: []const u8, context: NameContext) ![]const u8 {
            switch (context) {
                inline else => |tag| {
                    const f = @field(options, @tagName(tag) ++ "_name_fn");
                    const new_name = try f(self.allocator, name);
                    if (new_name.len > 0 and new_name[0] == '@') return new_name;
                    const is_valid = std.zig.isValidId(new_name) and switch (context) {
                        .@"const", .@"fn", .param, .type => !std.zig.primitives.isPrimitive(new_name),
                        else => true,
                    };
                    return if (is_valid) new_name else try self.allocPrint("@\"{s}\"", .{new_name});
                },
            }
        }

        fn translateDeclarations(self: *@This()) !void {
            // add error set first
            try self.deriveErrorSet();
            if (getErrorType() != null) {
                try self.append(&self.new_root.container.decls, .{
                    .name = options.error_set,
                    .expr = .{ .type = self.new_error_set },
                });
            }
            // translate all declarations
            for (self.old_root.container.decls) |decl| {
                if (options.filter_fn(decl.name)) {
                    // get name in target namespace
                    const transform: NameContext = if (isFunctionDecl(decl))
                        .@"fn"
                    else switch (decl.expr) {
                        .type, .identifier => .type,
                        .unknown => .@"const",
                    };
                    const new_name = try self.transformName(decl.name, transform);
                    const new_decl_t = if (decl.type) |t| try self.translateType(t, false) else null;
                    const expr = try self.translateExpression(decl.expr);
                    if (expr == .type) {
                        // don't add declarations for opaques
                        if (expr.type.* == .container and std.mem.eql(u8, expr.type.container.kind, "opaque"))
                            continue;
                        try self.new_namespace.addType(new_name, expr.type);
                    }
                    const doc_comment = try options.doc_comment_fn(self.allocator, decl.name, new_name);
                    try self.append(&self.new_root.container.decls, .{
                        .name = new_name,
                        .type = new_decl_t,
                        .alignment = decl.alignment,
                        .mutable = false,
                        .expr = expr,
                        .doc_comment = doc_comment,
                    });
                }
            }
            if (options.c_root_struct) |name| {
                // use specified type as root (i.e. the namespace of the source file)
                const t = self.old_namespace.getType(name) orelse {
                    std.debug.print("Unable to find container type '{s}'\n", .{name});
                    return error.Unexpected;
                };
                const new_t = self.old_to_new_type_map.get(t) orelse return error.Unexpected;
                const new_root = switch (new_t.*) {
                    .container => new_t,
                    .pointer => |p| p.child_type,
                    else => {
                        std.debug.print("'{s}' is not a container type\n", .{name});
                        return error.Unexpected;
                    },
                };
                // transfer decls into specified type
                new_root.container.decls = self.new_root.container.decls;
                self.new_root = new_root;
                // remove declaration of pointer type, if used to specified the struct
                if (new_t.* == .pointer) {
                    for (new_root.container.decls, 0..) |decl, i| {
                        if (decl.expr == .type and decl.expr.type == new_t) {
                            self.remove(&new_root.container.decls, i);
                            self.new_namespace.removeType(new_t);
                            break;
                        }
                    }
                    // can't be optional
                    new_t.pointer.is_optional = false;
                }
            }
            // add translate calls to function declarations
            for (self.new_root.container.decls) |*new_decl| {
                if (isFunctionDecl(new_decl.*)) {
                    const new_t = new_decl.type.?;
                    const t, const fn_name = for (self.old_root.container.decls) |decl| {
                        if (isFunctionDecl(decl)) {
                            if (self.old_to_new_type_map.get(decl.type.?) == new_t) {
                                break .{ decl.type.?, decl.name };
                            }
                        }
                    } else unreachable;
                    const return_error_union = self.shouldReturnErrorUnion(fn_name, t);
                    const ignore_non_error_return_value = self.shouldIgnoreNonErrorRetval(fn_name, t);
                    const local_subs = try self.findLocalSubstitutions(t, new_t);
                    const split_slices = try self.findSplitSlices(t, new_t);
                    // get translate() or translateMerge() call
                    new_decl.expr = try self.obtainTranslateCall(
                        fn_name,
                        return_error_union,
                        ignore_non_error_return_value,
                        local_subs,
                        split_slices,
                    );
                    if (split_slices.len > 0) {
                        // change the declaration, removing slice len and changing pointer type
                        for (0..split_slices.len) |i| {
                            const pair = split_slices[split_slices.len - i - 1];
                            const ptr_param = &new_t.function.parameters[pair.ptr_index];
                            ptr_param.type = try self.translateSlicePointer(ptr_param.type);
                            self.remove(&new_t.function.parameters, pair.len_index);
                        }
                    }
                }
            }
        }

        fn translateExpression(self: *@This(), expr: Expression) !Expression {
            return switch (expr) {
                .type => |t| .{ .type = try self.translateType(t, false) },
                .identifier => |i| if (self.old_namespace.getType(i)) |t| .{
                    .type = translate: {
                        // if the attempt to translate the type comes back here, then
                        // just use the old name, which should refer to a builtin type
                        if (self.type_lookup == t) break :translate t else {
                            self.type_lookup = t;
                            defer self.type_lookup = null;
                            break :translate try self.translateType(t, false);
                        }
                    },
                } else expr,
                .unknown => expr,
            };
        }

        fn translateField(self: *@This(), field: Field) !Field {
            const new_name = try self.transformName(field.name, .field);
            return .{
                .name = new_name,
                .type = try self.translateType(field.type, false),
                .alignment = field.alignment,
            };
        }

        fn translateParameter(self: *@This(), param: Parameter, is_pointer_target: bool, is_inout: bool) !Parameter {
            const new_name = if (param.name) |n| try self.transformName(n, .param) else null;
            const param_type = swap: {
                switch (param.type.*) {
                    .pointer => |p| if (p.is_const and !isOpaque(p.child_type)) {
                        // const pointer to struct and union become by-value argument
                        switch (p.child_type.*) {
                            .container => {
                                const type_name = try self.obtainTypeName(p.child_type, .old);
                                if (!is_pointer_target and options.type_is_by_value_fn(type_name)) {
                                    break :swap p.child_type;
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
                break :swap param.type;
            };
            const new_type = try self.translateType(param_type, false);
            return .{
                .name = new_name,
                .type = new_type,
                .is_inout = is_inout,
            };
        }

        fn translateEnumItem(self: *@This(), item: EnumItem) !EnumItem {
            const new_name = try self.transformName(item.name, .@"enum");
            return .{
                .name = new_name,
                .value = item.value,
            };
        }

        fn translateType(self: *@This(), t: *Type, is_pointer_target: bool) error{OutOfMemory}!*Type {
            if (self.old_to_new_type_map.get(t)) |new_t| return new_t;
            const new_t = switch (t.*) {
                .container => try self.translateContainer(t),
                .pointer => try self.translatePointer(t),
                .enumeration => try self.translateEnumeration(t),
                .function => try self.translateFunction(t, is_pointer_target),
                .expression => |e| create: {
                    const new_t = switch (try self.translateExpression(e)) {
                        .type => |new_t| new_t,
                        else => t,
                    };
                    if (new_t == t and t.expression == .identifier) {
                        // add name to namespace
                        try self.new_namespace.addType(t.expression.identifier, t);
                    }
                    break :create new_t;
                },
                else => unreachable,
            };
            try self.old_to_new_type_map.put(t, new_t);
            return new_t;
        }

        fn translateContainer(self: *@This(), t: *Type) !*Type {
            const c = t.container;
            if (std.mem.eql(u8, c.kind, "opaque")) return t;
            var new_fields: []Field = &.{};
            for (c.fields) |field| {
                const new_field = try self.translateField(field);
                try self.append(&new_fields, new_field);
            }
            // don't use extern when resulting struct contains function
            const layout = for (new_fields) |field| {
                if (field.type.* == .function) break null;
            } else c.layout;
            return try self.createType(.{
                .container = .{
                    .layout = layout,
                    .kind = c.kind,
                    .backing_type = c.backing_type,
                    .fields = new_fields,
                },
            });
        }

        fn translatePointer(self: *@This(), t: *Type) !*Type {
            const p = t.pointer;
            const child_t = try self.translateType(p.child_type, true);
            const ptr_name = try self.obtainTypeName(t, .old);
            const target_name = try self.obtainTypeName(p.child_type, .old);
            const is_null_terminated = options.ptr_is_null_terminated_fn(ptr_name, target_name);
            const is_many = options.ptr_is_many_fn(ptr_name, target_name);
            const is_optional = options.ptr_is_optional_fn(ptr_name, target_name);
            return try self.createType(.{
                .pointer = .{
                    .child_type = child_t,
                    .alignment = p.alignment,
                    .sentinel = if (is_null_terminated) "0" else null,
                    .size = if (is_many) .many else .one,
                    .is_const = p.is_const,
                    .is_volatile = p.is_volatile,
                    .is_optional = is_optional,
                    .allows_zero = p.allows_zero,
                },
            });
        }

        fn translateEnumeration(self: *@This(), t: *Type) !*Type {
            const e = t.enumeration;
            const enum_name = try self.obtainTypeName(t, .old);
            if (options.enum_is_packed_struct_fn(enum_name)) {
                var pow2_items: []EnumItem = &.{};
                for (e.items) |item| {
                    if (item.value != 0 and std.math.isPowerOfTwo(item.value)) {
                        try self.append(&pow2_items, item);
                    }
                }
                std.mem.sort(EnumItem, pow2_items, {}, struct {
                    fn compare(_: void, lhs: EnumItem, rhs: EnumItem) bool {
                        return lhs.value < rhs.value;
                    }
                }.compare);
                var blank_field_name: []const u8 = "_";
                var bits_used: isize = 0;
                var bit_fields: []Field = &.{};
                var new_fields: []Field = &.{};
                for (pow2_items) |item| {
                    const pos = @ctz(item.value);
                    if (bits_used != pos) {
                        // insert filler
                        const blank_field = try self.createBlankField(blank_field_name, pos - bits_used);
                        try self.append(&new_fields, blank_field);
                        blank_field_name = try self.allocPrint("{s}{s}", .{ blank_field_name, "_" });
                        bits_used = pos;
                    }
                    const new_field = try self.translateBitField(item);
                    try self.append(&new_fields, new_field);
                    try self.append(&bit_fields, new_field);
                    bits_used += 1;
                }
                // insert final filler
                const blank_field = try self.createBlankField(blank_field_name, -bits_used);
                try self.append(&new_fields, blank_field);
                var new_decls: []Declaration = &.{};
                for (e.items) |item| {
                    if (item.value == 0 or !std.math.isPowerOfTwo(item.value)) {
                        var remaining = item.value;
                        for (pow2_items) |other_item| remaining &= ~other_item.value;
                        if (remaining == 0) {
                            // it can be represented as a combination of field items
                            var set_fields: []Field = &.{};
                            for (pow2_items, 0..) |pow2_item, i| {
                                if (item.value & pow2_item.value != 0) {
                                    try self.append(&set_fields, bit_fields[i]);
                                }
                            }
                            const decl = try self.createBitFieldDeclaration(item.name, set_fields);
                            try self.append(&new_decls, decl);
                        } else {
                            const decl = try self.createIntDeclaration(item.name, item.value);
                            try self.append(&new_decls, decl);
                        }
                    }
                }
                return try self.createType(.{
                    .container = .{
                        .layout = "packed",
                        .kind = "struct",
                        .backing_type = "c_uint",
                        .fields = new_fields,
                        .decls = new_decls,
                    },
                });
            } else {
                var new_items: []EnumItem = &.{};
                for (e.items) |item| {
                    const new_field = try self.translateEnumItem(item);
                    try self.append(&new_items, new_field);
                }
                return try self.createType(.{
                    .enumeration = .{
                        .items = new_items,
                        .is_signed = e.is_signed,
                    },
                });
            }
        }

        fn translateFunction(self: *@This(), t: *Type, is_pointer_target: bool) !*Type {
            const f = t.function;
            var new_params: []Parameter = &.{};
            // look for writable pointer
            var output_types: []*Type = &.{};
            var inout_index: ?usize = null;
            const fn_name = try self.obtainFunctionName(t);
            const output_start = for (0..f.parameters.len) |offset| {
                const index = f.parameters.len - offset - 1;
                const param = f.parameters[index];
                if (is_pointer_target) break index + 1;
                if (!self.isWriteTarget(param.type)) break index + 1;
                // maybe it's a in/out pointer--need to ask the callback function
                const type_name = try self.obtainTypeName(param.type.pointer.child_type, .old);
                if (options.param_is_input_fn(fn_name, param.name, index, type_name)) {
                    inout_index = index;
                    self.need_inout_import = true;
                    break index + 1;
                }
            } else 0;
            for (f.parameters[output_start..]) |param| {
                const target_type = param.type.pointer.child_type;
                const output_type = try self.translateType(target_type, true);
                try self.append(&output_types, output_type);
            }
            // see if the translated function should return an error union
            const return_error_union = !is_pointer_target and self.shouldReturnErrorUnion(fn_name, t);
            const status_type = try self.translateType(f.return_type, false);
            const extra: usize = switch (return_error_union) {
                true => switch (self.non_error_enums.len > 1) {
                    true => if (self.shouldIgnoreNonErrorRetval(fn_name, t)) 0 else 1,
                    false => 0,
                },
                false => if (isVoid(f.return_type)) 0 else 1,
            };
            if (extra == 1) {
                try self.append(&output_types, status_type);
            }
            const arg_count = f.parameters.len + extra - output_types.len;
            for (f.parameters[0..arg_count], 0..) |param, index| {
                const new_param = try self.translateParameter(param, is_pointer_target, inout_index == index);
                try self.append(&new_params, new_param);
            }
            const payload_type = switch (output_types.len) {
                0 => self.void_type,
                1 => output_types[0],
                else => try self.createType(.{ .type_tuple = output_types }),
            };
            const return_type = switch (return_error_union) {
                true => try self.createType(.{
                    .error_union = .{
                        .payload_type = payload_type,
                        .error_set = self.new_error_set,
                    },
                }),
                false => payload_type,
            };
            return try self.createType(.{
                .function = .{
                    .parameters = new_params,
                    .return_type = return_type,
                    .alignment = f.alignment,
                    .call_convention = if (is_pointer_target) f.call_convention else null,
                },
            });
        }

        fn findLocalSubstitutions(self: *@This(), t: *Type, new_t: *Type) ![]LocalSubstitution {
            var local_subs: []LocalSubstitution = &.{};
            for (new_t.function.parameters, 0..) |new_param, index| {
                // we need to do local substitution when the new type cannot be derived through
                // the global substitution table
                const param = t.function.parameters[index];
                if (!try self.addGlobalSubstitute(param.type, new_param.type)) {
                    var type_name = try self.obtainTypeName(new_param.type, .new);
                    if (param.is_inout) {
                        type_name = try self.allocPrint("inout({s})", .{type_name});
                    }
                    try self.append(&local_subs, .{
                        .index = index,
                        .type_name = type_name,
                    });
                }
            }
            // add substitutes of return types
            const return_type = new_t.function.return_type;
            const output_types: []const *Type = switch (return_type.*) {
                .error_union => |e| switch (e.payload_type.*) {
                    .type_tuple => |tt| tt,
                    else => &.{e.payload_type},
                },
                else => &.{return_type},
            };
            const offset = new_t.function.parameters.len;
            for (output_types, 0..) |new_ot, i| {
                const ot, const index = switch (offset + i < t.function.parameters.len) {
                    true => .{ t.function.parameters[offset + i].type.pointer.child_type, offset + i },
                    false => switch (return_type.*) {
                        // if an error union is return, then the extra return is the status
                        // which doesn't require substitution
                        .error_union => continue,
                        else => .{ t.function.return_type, null },
                    },
                };
                if (!try self.addGlobalSubstitute(ot, new_ot)) {
                    const type_name = try self.obtainTypeName(new_ot, .new);
                    try self.append(&local_subs, .{
                        .index = index,
                        .type_name = type_name,
                    });
                }
            }
            return local_subs;
        }

        fn findSplitSlices(self: *@This(), t: *Type, new_t: *Type) ![]SplitSlice {
            var pairs: []SplitSlice = &.{};
            const has_pointers = for (new_t.function.parameters) |p| {
                if (p.type.* == .pointer) break true;
            } else false;
            if (has_pointers) {
                const fn_name = try self.obtainFunctionName(t);
                for (0..new_t.function.parameters.len) |i| {
                    const is_ptr = for (pairs) |pair| {
                        if (pair.ptr_index == i) break true;
                    } else false;
                    if (!is_ptr) {
                        const param = t.function.parameters[i];
                        const type_name = try self.obtainTypeName(param.type, .old);
                        if (options.param_is_slice_len_fn(fn_name, param.name, i, type_name)) |ptr_index| {
                            try self.append(&pairs, .{ .ptr_index = ptr_index, .len_index = i });
                        }
                    }
                }
            }
            return pairs;
        }

        fn translateBitField(self: *@This(), item: EnumItem) !Field {
            const new_name = try self.transformName(item.name, .@"enum");
            const t = self.new_namespace.getType("bool") orelse add: {
                const new_t = try self.createType(.{ .expression = .{ .identifier = "bool" } });
                try self.new_namespace.addType("bool", new_t);
                break :add new_t;
            };
            return .{
                .name = new_name,
                .type = t,
                .default_value = "false",
            };
        }

        fn translateSlicePointer(self: *@This(), t: *Type) !*Type {
            var new_t = t.*;
            new_t.pointer.size = .slice;
            new_t.pointer.sentinel = null;
            if (isOpaque(new_t.pointer.child_type)) {
                new_t.pointer.child_type = try self.createType(.{
                    .expression = .{ .identifier = "u8" },
                });
            }
            return try self.createType(new_t);
        }

        fn createBlankField(self: *@This(), name: []const u8, width: isize) !Field {
            const t = get: {
                if (width > 0) {
                    const type_name = try self.allocPrint("u{d}", .{width});
                    break :get self.new_namespace.getType(type_name) orelse add: {
                        const new_t = try self.createType(.{ .expression = .{ .identifier = type_name } });
                        try self.new_namespace.addType(type_name, new_t);
                        break :add new_t;
                    };
                } else {
                    const expr = try self.allocPrint("std.meta.Int(.unsigned, @bitSizeOf(c_uint) - {d})", .{-width});
                    break :get try self.createType(.{ .expression = .{ .unknown = expr } });
                }
            };
            return .{
                .name = name,
                .type = t,
                .default_value = "0",
            };
        }

        fn createBitFieldDeclaration(self: *@This(), name: []const u8, fields: []Field) !Declaration {
            const new_name = try self.transformName(name, .@"enum");
            var pairs: [][]const u8 = &.{};
            for (fields) |field| {
                const assign = try self.allocPrint(".{s} = true", .{field.name});
                try self.append(&pairs, assign);
            }
            const set = switch (pairs.len) {
                0 => ".{}",
                else => try self.allocPrint(".{s}", .{pairs}),
            };
            const t = self.new_namespace.getType("@This()") orelse add: {
                const new_t = try self.createType(.{ .expression = .{ .identifier = "@This()" } });
                try self.new_namespace.addType("@This()", new_t);
                break :add new_t;
            };
            return .{
                .name = new_name,
                .type = t,
                .expr = .{ .unknown = set },
            };
        }

        fn createIntDeclaration(self: *@This(), name: []const u8, value: i128) !Declaration {
            const new_name = try self.transformName(name, .@"enum");
            const number = try self.allocPrint("{d}", .{value});
            return .{
                .name = new_name,
                .expr = .{ .unknown = number },
            };
        }

        fn addGlobalSubstitute(self: *@This(), old_type: *Type, new_type: *Type) !bool {
            if (new_type == old_type) return true;
            if (new_type.* == .expression and new_type.expression == .identifier) {
                if (std.zig.isPrimitive(new_type.expression.identifier)) return true;
            }
            const is_unique_type = switch (old_type.*) {
                .enumeration, .expression => false,
                else => true,
            };
            if (is_unique_type) {
                const result = try self.old_to_new_param_map.getOrPut(old_type);
                if (!result.found_existing) {
                    result.value_ptr.* = new_type;
                    return true;
                }
            }
            return false;
        }

        fn isFunctionDecl(decl: Declaration) bool {
            return decl.type != null and decl.type.?.* == .function;
        }

        fn isWriteTarget(_: *@This(), t: *const Type) bool {
            return switch (t.*) {
                .pointer => |p| !p.is_const and !isOpaque(p.child_type),
                else => false,
            };
        }

        fn isOpaque(t: *const Type) bool {
            return switch (t.*) {
                .container => |c| c.fields.len == 0,
                .expression => |e| switch (e) {
                    .type => |ref_t| isOpaque(ref_t),
                    .identifier => |i| std.mem.eql(u8, i, "anyopaque"),
                    .unknown => false,
                },
                else => false,
            };
        }

        fn isVoid(t: *const Type) bool {
            return switch (t.*) {
                .expression => |e| switch (e) {
                    .type => |ref_t| isVoid(ref_t),
                    .identifier => |i| std.mem.eql(u8, i, "void"),
                    .unknown => false,
                },
                else => false,
            };
        }

        fn getErrorType() ?[]const u8 {
            const name = options.c_error_type orelse return null;
            return if (std.mem.eql(u8, name, "int"))
                "c_int"
            else if (std.mem.eql(u8, name, "unsigned int"))
                "c_uint"
            else
                name;
        }

        fn isReturningStatus(self: *@This(), t: *Type) bool {
            const err_type = getErrorType() orelse return false;
            const name = self.obtainTypeName(t.function.return_type, .old) catch return false;
            return std.mem.eql(u8, err_type, name);
        }

        fn shouldReturnErrorUnion(self: *@This(), fn_name: []const u8, t: *Type) bool {
            return self.isReturningStatus(t) and options.error_union_is_returned_fn(fn_name);
        }

        fn shouldIgnoreNonErrorRetval(self: *@This(), fn_name: []const u8, t: *Type) bool {
            return if (self.non_error_enums.len > 1 and self.isReturningStatus(t))
                !options.status_is_returned_fn(fn_name)
            else
                false;
        }

        fn deriveErrorSet(self: *@This()) !void {
            var names: [][]const u8 = &.{};
            var errors: [][]const u8 = &.{};
            var non_errors: [][]const u8 = &.{};
            const error_type = getErrorType() orelse return;
            if (self.old_namespace.getType(error_type)) |t| {
                if (t.* == .enumeration) {
                    for (t.enumeration.items) |item| {
                        if (options.enum_is_error_fn(item.name, item.value)) {
                            const err_name = try self.transformName(item.name, .@"error");
                            try self.append(&names, err_name);
                            const en_name = try self.transformName(item.name, .@"enum");
                            try self.append(&errors, en_name);
                        } else {
                            const en_name = try self.transformName(item.name, .@"enum");
                            try self.append(&non_errors, en_name);
                        }
                    }
                }
            }
            const has_unexpected = for (names) |n| {
                if (std.mem.eql(u8, n, "Unexpected")) break true;
            } else false;
            if (!has_unexpected) try self.append(&names, "Unexpected");
            self.new_error_set.error_set = names;
            try self.new_namespace.addType(options.error_set, self.new_error_set);
            self.error_enums = errors;
            self.non_error_enums = non_errors;
        }

        fn obtainTranslateCall(
            self: *@This(),
            old_fn_name: []const u8,
            return_error_union: bool,
            ignore_non_error_return_value: bool,
            local_subs: []LocalSubstitution,
            split_slices: []SplitSlice,
        ) !Expression {
            const local_sub_list = if (local_subs.len > 0) format: {
                var sub_items: [][]const u8 = &.{};
                for (local_subs) |sub| {
                    const sub_item = if (sub.index) |i|
                        try self.allocPrint(".@\"{d}\" = {s}", .{ i, sub.type_name })
                    else
                        try self.allocPrint(".retval = {s}", .{sub.type_name});
                    try self.append(&sub_items, sub_item);
                }
                const sub_items_joined = try std.mem.join(self.allocator, ", ", sub_items);
                break :format try self.allocPrint(" {s} ", .{sub_items_joined});
            } else "";
            // print code for translateMerge() to merge slice pointers
            const slice_list = if (split_slices.len > 0) format: {
                var slice_lines: [][]const u8 = &.{};
                for (split_slices) |pair| {
                    const slice_line = try self.allocPrint("    .{{ .ptr_index = {d}, .len_index = {d} }},", .{
                        pair.ptr_index,
                        pair.len_index,
                    });
                    try self.append(&slice_lines, slice_line);
                }
                break :format try std.mem.join(self.allocator, "\n", slice_lines);
            } else "";
            const code = if (slice_list.len > 0)
                try self.allocPrint("{s}.translateMerge(\"{s}\", {}, {}, .{{{s}}}, &.{{\n{s}\n}})", .{
                    options.translater,
                    old_fn_name,
                    return_error_union,
                    ignore_non_error_return_value,
                    local_sub_list,
                    slice_list,
                })
            else
                try self.allocPrint("{s}.translate(\"{s}\", {}, {}, .{{{s}}})", .{
                    options.translater,
                    old_fn_name,
                    return_error_union,
                    ignore_non_error_return_value,
                    local_sub_list,
                });
            return .{ .unknown = code };
        }

        fn obtainTypeName(self: *@This(), t: *Type, ns: NamespaceType) ![]const u8 {
            const indent_before = self.indent_level;
            defer self.indent_level = indent_before;
            self.indent_level = 0;
            self.write_to_byte_array = true;
            defer self.write_to_byte_array = false;
            self.byte_array.clearRetainingCapacity();
            self.printTypeRef(t, ns) catch {};
            return try self.allocator.dupe(u8, self.byte_array.items);
        }

        fn obtainFunctionName(self: *@This(), t: *Type) ![]const u8 {
            return for (self.old_root.container.decls) |decl| {
                // function prototype
                if (decl.type == t) break decl.name;
                if (decl.expr == .type) {
                    // function definition
                    if (decl.expr.type == t) break decl.name;
                    if (decl.expr.type.* == .pointer) {
                        // function pointer
                        if (decl.expr.type == t) break decl.name;
                    }
                }
            } else try self.obtainTypeName(t, .old);
        }

        fn printImports(self: *@This()) anyerror!void {
            try self.printTxt("const std = @import(\"std\");\n");
            try self.printFmt("const api_translator = @import(\"{s}api-translator.zig\");\n", .{
                options.zigft_path,
            });
            if (self.need_inout_import) {
                try self.printTxt("const inout = api_translator.inout;\n");
            }
            try self.printFmt("const {s} = @cImport({{\n", .{options.c_import});
            for (options.header_paths) |path| {
                try self.printFmt("@cInclude(\"{s}\");\n", .{path});
            }
            try self.printTxt("}});\n\n");
        }

        fn printTypeRef(self: *@This(), t: *Type, ns: NamespaceType) anyerror!void {
            if (ns == .old) {
                if (t == self.old_root) {
                    try self.printTxt("@This()");
                } else if (self.old_namespace.getName(t)) |name| {
                    try self.printFmt("{s}", .{name});
                } else {
                    try self.printTypeDef(t, ns);
                }
            } else if (ns == .new) {
                if (t == self.new_root) {
                    try self.printTxt("@This()");
                } else if (t == self.old_root) {
                    try self.printTxt(options.c_import);
                } else if (self.new_namespace.getName(t)) |name| {
                    try self.printFmt("{s}", .{name});
                } else if (self.old_namespace.getName(t)) |name| {
                    try self.printFmt("{s}.{s}", .{ options.c_import, name });
                } else {
                    try self.printTypeDef(t, ns);
                }
            }
        }

        fn printTypeDef(self: *@This(), t: *Type, ns: NamespaceType) anyerror!void {
            switch (t.*) {
                .container => |c| {
                    if (t != self.new_root) {
                        if (c.layout) |l| try self.printFmt("{s} ", .{l});
                        try self.printFmt("{s}", .{c.kind});
                        if (c.backing_type) |bt| try self.printFmt("({s})", .{bt});
                        if (c.fields.len == 0) {
                            try self.printTxt(" {{}}");
                        } else {
                            try self.printTxt(" {{\n");
                        }
                    }
                    for (c.fields) |field| try self.printField(field, ns);
                    if (c.fields.len > 0 and c.decls.len > 0) try self.printTxt("\n");
                    for (c.decls, 0..) |decl, i| {
                        if (i > 0) {
                            if (isFunctionDecl(decl) or isFunctionDecl(c.decls[i - 1])) {
                                try self.printTxt("\n");
                            }
                        }
                        try self.printDeclaration(decl, ns);
                    }
                    if (t != self.new_root and c.fields.len != 0) {
                        try self.printTxt("}}");
                    }
                },
                .pointer => |p| {
                    if (p.is_optional) try self.printTxt("?");
                    switch (p.size) {
                        .one => try self.printTxt("*"),
                        .many => try self.printTxt("[*"),
                        .slice => try self.printTxt("["),
                        .c => try self.printTxt("[*c"),
                    }
                    if (p.sentinel) |s| try self.printFmt(":{s}", .{s});
                    if (p.size != .one) try self.printTxt("]");
                    if (p.allows_zero) try self.printTxt("allows_zero ");
                    if (p.is_const) try self.printTxt("const ");
                    if (p.alignment) |a| try self.printFmt("align({s}) ", .{a});
                    if (p.is_volatile) try self.printTxt("volatile ");
                    try self.printTypeRef(p.child_type, ns);
                },
                .enumeration => |e| {
                    try self.printFmt("enum({s}) {{\n", .{if (e.is_signed) "c_int" else "c_uint"});
                    const is_sequential = for (e.items, 0..) |item, index| {
                        if (index > 0 and item.value != e.items[index - 1].value + 1) break false;
                    } else true;
                    const is_hexidecimal = !is_sequential and for (e.items) |item| {
                        if (item.value < 0) break false;
                    } else true;
                    const max_width = find: {
                        var width: usize = 0;
                        for (e.items) |item| {
                            width = @max(width, @bitSizeOf(@TypeOf(item.value)) - @clz(item.value) + 1);
                        }
                        break :find width;
                    };
                    for (e.items, 0..) |item, index| {
                        try self.printFmt("{s}", .{item.name});
                        if (!is_sequential or (index == 0 and item.value != 0)) {
                            if (is_hexidecimal) {
                                inline for (.{ 64, 32, 16, 8 }) |bits| {
                                    if (max_width > (bits / 2) or bits == 8) {
                                        const width = std.fmt.comptimePrint("{d}", .{bits / 4});
                                        try self.printFmt(" = 0x{x:0" ++ width ++ "}", .{@as(u128, @bitCast(item.value))});
                                        break;
                                    }
                                }
                            } else {
                                try self.printFmt(" = {d}", .{item.value});
                            }
                        }
                        try self.printTxt(",\n");
                    }
                    if (!e.is_exhaustive) {
                        try self.printTxt("_,\n");
                    }
                    try self.printTxt("}}");
                },
                .error_set => |names| {
                    try self.printTxt("error{{\n");
                    for (names) |n| try self.printFmt("{s},\n", .{n});
                    try self.printTxt("}}");
                },
                .error_union => |e| {
                    try self.printTypeRef(e.error_set, ns);
                    try self.printTxt("!");
                    try self.printTypeRef(e.payload_type, ns);
                },
                .function => |f| {
                    if (f.parameters.len > 0) {
                        try self.printTxt("fn (\n");
                        for (f.parameters) |param| {
                            if (param.name) |n| try self.printFmt("{s}: ", .{n});
                            try self.printTypeRef(param.type, ns);
                            try self.printTxt(",\n");
                        }
                        try self.printTxt(") ");
                    } else {
                        try self.printTxt("fn () ");
                    }
                    if (f.alignment) |a| try self.printFmt("align({s}) ", .{a});
                    if (f.call_convention) |c| try self.printFmt("callconv({s}) ", .{c});
                    try self.printTypeRef(f.return_type, ns);
                },
                .optional => |ot| {
                    try self.printTxt("?");
                    try self.printTypeRef(ot, ns);
                },
                .type_tuple => |types| {
                    try self.printTxt("std.meta.Tuple(&.{{ ");
                    for (types, 0..) |tt, i| {
                        try self.printTypeRef(tt, ns);
                        if (i != types.len - 1) try self.printTxt(", ");
                    }
                    try self.printTxt(" }})");
                },
                .expression => |e| {
                    switch (e) {
                        .type => |et| try self.printTypeRef(et, ns),
                        .identifier, .unknown => |s| try self.printFmt("{s}", .{s}),
                    }
                },
            }
        }

        fn printField(self: *@This(), field: Field, ns: NamespaceType) anyerror!void {
            try self.printFmt("{s}: ", .{field.name});
            try self.printTypeRef(field.type, ns);
            if (field.alignment) |a| try self.printFmt(" align({s})", .{a});
            if (field.default_value) |v| try self.printFmt(" = {s}", .{v});
            try self.printTxt(",\n");
        }

        fn printDocComment(self: *@This(), text: []const u8) anyerror!void {
            var iterator = std.mem.splitScalar(u8, text, '\n');
            while (iterator.next()) |line| {
                try self.printFmt("/// {s}\n", .{line});
            }
        }

        fn printDeclaration(self: *@This(), decl: Declaration, ns: NamespaceType) anyerror!void {
            if (decl.doc_comment) |c| try self.printDocComment(c);
            const mut = if (decl.mutable) "var" else "const";
            try self.printFmt("pub {s} {s}", .{ mut, decl.name });
            if (decl.type) |t| {
                try self.printTxt(": ");
                try self.printTypeRef(t, ns);
            }
            try self.printTxt(" = ");
            switch (decl.expr) {
                .type => |t| try self.printTypeDef(t, ns),
                .identifier, .unknown => |s| try self.printFmt("{s}", .{s}),
            }
            try self.printTxt(";\n");
        }

        fn printTrainslatorSetup(self: *@This()) !void {
            try self.printTxt("\n");
            try self.printFmt("const {s} = api_translator.Translator(.{{\n", .{options.translater});
            try self.printFmt(".c_import_ns = {s},\n", .{options.c_import});
            try self.printTypeSubstitutions();
            try self.printErrorScheme();
            try self.printTxt("}});\n");
        }

        fn printTypeSubstitutions(self: *@This()) !void {
            const Sub = struct { old_name: []const u8, new_name: []const u8 };
            var subs: []Sub = &.{};
            var added: std.StringHashMap(bool) = .init(self.allocator);
            var iterator = self.old_to_new_param_map.iterator();
            while (iterator.next()) |entry| {
                const old_t = entry.key_ptr.*;
                const new_t = entry.value_ptr.*;
                const old_name = try self.obtainTypeName(old_t, .new);
                if (added.get(old_name) == null) {
                    const new_name = try self.obtainTypeName(new_t, .new);
                    if (!std.mem.eql(u8, old_name, new_name)) {
                        try self.append(&subs, .{ .old_name = old_name, .new_name = new_name });
                        try added.put(old_name, true);
                    }
                }
            }
            std.mem.sort(Sub, subs, {}, struct {
                fn compare(_: void, lhs: Sub, rhs: Sub) bool {
                    return switch (std.mem.order(u8, lhs.old_name, rhs.old_name)) {
                        .gt => false,
                        .lt => true,
                        .eq => switch (std.mem.order(u8, lhs.new_name, rhs.new_name)) {
                            .lt => true,
                            else => false,
                        },
                    };
                }
            }.compare);
            if (subs.len == 0) return;
            try self.printTxt(".substitutions = &.{{\n");
            for (subs) |sub| {
                try self.printFmt(".{{ .old = {s}, .new = {s} }},\n", .{ sub.old_name, sub.new_name });
            }
            try self.printTxt("}},\n");
        }

        fn printErrorScheme(self: *@This()) !void {
            const error_type = getErrorType() orelse {
                try self.printTxt(".error_scheme = api_translator.NullErrorScheme,\n");
                return;
            };
            const old_enum_t = self.old_namespace.getType(error_type) orelse {
                std.debug.print("Unable to find enum type '{s}'\n", .{error_type});
                return error.Unexpected;
            };
            const new_enum_t = try self.translateType(old_enum_t, false);
            const enum_name = try self.obtainTypeName(new_enum_t, .new);
            try self.printFmt(".error_scheme = api_translator.BasicErrorScheme({s}, {s}, {s}.Unexpected),\n", .{
                enum_name,
                options.error_set,
                options.error_set,
            });
        }

        fn printSimpleTest(self: *@This()) !void {
            try self.printTxt("\ntest {{\n");
            try self.printTxt("inline for (comptime std.meta.declarations(@This())) |decl| {{\n");
            try self.printTxt("_ = @field(@This(), decl.name);\n");
            try self.printTxt("}}\n");
            try self.printTxt("}}\n");
        }

        fn printFmt(self: *@This(), comptime fmt: []const u8, args: anytype) anyerror!void {
            if (std.mem.startsWith(u8, fmt, "}") or std.mem.startsWith(u8, fmt, ")")) {
                self.indent_level -= 1;
            }
            if (self.indent_level > 0 and !self.indented) {
                for (0..self.indent_level) |_| {
                    try self.write("    ", .{});
                }
                self.indented = true;
            }
            try self.write(fmt, args);
            if (std.mem.endsWith(u8, fmt, "{\n") or std.mem.endsWith(u8, fmt, "(\n")) {
                self.indent_level += 1;
            }
            if (std.mem.endsWith(u8, fmt, "\n")) {
                self.indented = false;
            }
        }

        fn printTxt(self: *@This(), comptime txt: []const u8) anyerror!void {
            return self.printFmt(txt, .{});
        }

        fn write(self: *@This(), comptime fmt: []const u8, args: anytype) anyerror!void {
            const writer = if (self.write_to_byte_array)
                self.byte_array.writer().any()
            else
                self.output_writer;
            return writer.print(fmt, args);
        }

        fn translateHeaderFile(self: *@This(), full_path: []const u8) ![]const u8 {
            var argv: [][]const u8 = &.{};
            try self.append(&argv, "zig");
            try self.append(&argv, "translate-c");
            try self.append(&argv, full_path);
            try self.append(&argv, "-lc");
            for (options.include_paths) |include_path| {
                const arg = try self.allocPrint("-I{s}", .{include_path});
                try self.append(&argv, arg);
            }
            const result = try std.process.Child.run(.{
                .allocator = self.allocator,
                .argv = argv,
                .max_output_bytes = 1024 * 1024 * 128,
            });
            if (result.stderr.len != 0) {
                std.debug.print("{s}\n", .{result.stderr});
                return error.Failure;
            }
            return result.stdout;
        }

        fn findSourceFile(self: *@This(), path: []const u8) ![]const u8 {
            for (options.include_paths) |include_path| {
                const full_path = try std.fs.path.resolve(self.allocator, &.{
                    self.cwd,
                    include_path,
                    path,
                });
                if (std.fs.accessAbsolute(full_path, .{})) |_| {
                    return full_path;
                } else |_| self.allocator.free(full_path);
            }
            return error.FileNotFound;
        }

        fn allocPrint(self: *@This(), comptime fmt: []const u8, args: anytype) ![]const u8 {
            return std.fmt.allocPrint(self.allocator, fmt, args);
        }

        fn append(self: *@This(), slice_ptr: anytype, value: GrandchildOf(@TypeOf(slice_ptr))) !void {
            const len = slice_ptr.*.len;
            const capacity = calcCapacity(len);
            const new_len = len + 1;
            const new_capacity = calcCapacity(new_len);
            if (new_capacity != capacity) {
                const slice_before = slice_ptr.*.ptr[0..capacity];
                slice_ptr.* = try self.allocator.realloc(slice_before, new_capacity);
            }
            slice_ptr.*.len = new_len;
            slice_ptr.*[len] = value;
        }

        fn remove(self: *@This(), slice_ptr: anytype, index: usize) void {
            _ = GrandchildOf(@TypeOf(slice_ptr));
            const len = slice_ptr.*.len;
            var i: usize = index;
            while (i + 1 < len) : (i += 1) {
                slice_ptr.*[i] = slice_ptr.*[i + 1];
            }
            const capacity = calcCapacity(len);
            const new_len = len - 1;
            const new_capacity = calcCapacity(new_len);
            if (new_capacity != capacity) {
                const slice_before = slice_ptr.*.ptr[0..capacity];
                slice_ptr.* = self.allocator.realloc(slice_before, new_capacity) catch unreachable;
            }
            slice_ptr.*.len = new_len;
        }

        fn calcCapacity(len: usize) usize {
            return if (len > 0) std.math.ceilPowerOfTwo(usize, len) catch unreachable else 0;
        }

        fn GrandchildOf(comptime T: type) type {
            return switch (@typeInfo(T)) {
                .pointer => |pt| check: {
                    if (pt.is_const) @compileError("Cannot make modification through a const pointer");
                    break :check switch (@typeInfo(pt.child)) {
                        .pointer => |pt2| check2: {
                            if (pt2.is_const) @compileError("Slice is const");
                            break :check2 pt2.child;
                        },
                        else => @compileError("Not a pointer to a slice"),
                    };
                },
                else => @compileError("Not a pointer"),
            };
        }
    };
}

pub fn camelize(allocator: std.mem.Allocator, name: []const u8, start_index: usize, capitalize: bool) ![]const u8 {
    const underscore_count = std.mem.count(u8, name[start_index..], "_");
    const len = name.len - start_index - underscore_count;
    const buffer = try allocator.alloc(u8, len);
    var need_upper = capitalize;
    var i: usize = start_index;
    var j: usize = 0;
    while (i < name.len) : (i += 1) {
        if (name[i] == '_') {
            if (j > 0) need_upper = true;
        } else {
            if (need_upper) {
                buffer[j] = std.ascii.toUpper(name[i]);
                need_upper = false;
            } else {
                buffer[j] = std.ascii.toLower(name[i]);
            }
            j += 1;
        }
    }
    return buffer;
}

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

test "camelize" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var arena: std.heap.ArenaAllocator = .init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();
    const name1 = try camelize(allocator, "animal_green_dragon", 7, false);
    try expectEqualSlices(u8, "greenDragon", name1);
    const name2 = try camelize(allocator, "animal_green_dragon", 7, true);
    try expectEqualSlices(u8, "GreenDragon", name2);
    const name3 = try camelize(allocator, "ANIMAL_GREEN_DRAGON", 0, true);
    try expectEqualSlices(u8, "AnimalGreenDragon", name3);
}

pub fn snakify(allocator: std.mem.Allocator, name: []const u8, start_index: usize) ![]const u8 {
    const underscore_count = scan: {
        var count: usize = 0;
        var i: usize = start_index;
        while (i < name.len) : (i += 1) {
            if (std.ascii.isUpper(name[i]) and i > start_index and std.ascii.isLower(name[i - 1])) {
                count += 1;
            }
        }
        break :scan count;
    };
    const len = name.len - start_index + underscore_count;
    const buffer = try allocator.alloc(u8, len);
    var i: usize = start_index;
    var j: usize = 0;
    while (i < name.len) : (i += 1) {
        if (std.ascii.isUpper(name[i])) {
            if (i > start_index and std.ascii.isLower(name[i - 1])) {
                buffer[j] = '_';
                j += 1;
            }
            buffer[j] = std.ascii.toLower(name[i]);
            j += 1;
        } else {
            buffer[j] = name[i];
            j += 1;
        }
    }
    return buffer;
}

test "snakify" {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var arena: std.heap.ArenaAllocator = .init(gpa.allocator());
    const allocator = arena.allocator();
    defer arena.deinit();
    const name1 = try snakify(allocator, "AnimalGreenDragon", 0);
    try expectEqualSlices(u8, "animal_green_dragon", name1);
    const name2 = try snakify(allocator, "AnimalGreenDragon", 6);
    try expectEqualSlices(u8, "green_dragon", name2);
}

pub fn makeNoChange(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
    return arg;
}

pub fn provideNoComment(_: std.mem.Allocator, _: []const u8, _: []const u8) std.mem.Allocator.Error!?[]const u8 {
    return null;
}

pub fn removeArgPrefix(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
    return if (std.mem.startsWith(u8, arg, "arg_")) arg[4..] else arg;
}

pub fn isNonZero(_: []const u8, value: i128) bool {
    return value != 0;
}

pub fn neverByValue(_: []const u8) bool {
    return false;
}

pub fn neverPackedStruct(_: []const u8) bool {
    return false;
}

pub fn neverOptional(_: []const u8, _: []const u8) bool {
    return false;
}

pub fn alwaysTrue(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) bool {
    return true;
}

pub fn notFunctionSpecific(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) ?bool {
    return null;
}

pub fn neverInput(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) bool {
    return false;
}

pub fn neverSliceLength(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) ?usize {
    return null;
}

pub fn neverReturned(_: []const u8) bool {
    return false;
}

pub fn alwaysReturned(_: []const u8) bool {
    return true;
}

pub fn isTargetChar(_: []const u8, target_type: []const u8) bool {
    const char_types: []const []const u8 = &.{ "u8", "wchar_t", "char16_t" };
    return for (char_types) |char_type| {
        if (std.mem.eql(u8, target_type, char_type)) break true;
    } else false;
}

fn asComptimeInt(comptime s: []const u8) comptime_int {
    return comptime calc: {
        var value = 0;
        for (s) |c| {
            const a: ?comptime_int = switch (c) {
                '0'...'9', 'A'...'Z' => c,
                'a'...'z' => std.ascii.toUpper(c),
                else => null,
            };
            if (a) |v| value = value * 128 + v;
        }
        break :calc value;
    };
}

test "asComptimeInt" {
    try expectEqual(asComptimeInt("HelloWorld"), asComptimeInt("hello world"));
    try expectEqual(asComptimeInt("hello_world"), asComptimeInt("helloworld"));
}

test "Translator.SwapType" {
    const OldStruct = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct = extern struct {
        a: i32,
        b: i32,
    };
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
            .{ .old = *OldStruct, .new = *NewStruct },
            .{ .old = [*]OldStruct, .new = [*]NewStruct },
        },
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, error.Unexpected),
    });
    const SwapType = c_to_zig.SwapType;
    const T1 = SwapType(OldStruct, .old_to_new);
    try expectEqual(T1, NewStruct);
    const T2 = SwapType(*OldStruct, .old_to_new);
    try expectEqual(T2, *NewStruct);
    const T3 = SwapType([*]OldStruct, .old_to_new);
    try expectEqual(T3, [*]NewStruct);
    const T4 = SwapType(NewStruct, .new_to_old);
    try expectEqual(T4, OldStruct);
}

test "Translator.Substitute" {
    const OldStruct = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct = extern struct {
        a: i32,
        b: i32,
    };
    const NewEnum = enum(c_uint) { a, b, c };
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
            .{ .old = *OldStruct, .new = *NewStruct },
            .{ .old = [*]OldStruct, .new = [*]NewStruct },
        },
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, error.Unexpected),
    });
    const Substitute = c_to_zig.Substitute;
    const T1 = Substitute(OldStruct, .{}, 0, 1);
    try expectEqual(T1, NewStruct);
    const T2 = Substitute(*OldStruct, .{}, 0, 1);
    try expectEqual(T2, *NewStruct);
    const T3 = Substitute([*]OldStruct, .{}, 0, 1);
    try expectEqual(T3, [*]NewStruct);
    const T4 = Substitute(c_uint, .{ .@"3" = NewEnum }, 3, 5);
    try expectEqual(T4, NewEnum);
    const T5 = Substitute(c_uint, .{ .@"-2" = NewEnum }, 3, 5);
    try expectEqual(T5, NewEnum);
    const T6 = Substitute(c_uint, .{ .@"-2" = NewEnum }, 2, 5);
    try expectEqual(T6, c_uint);
}

test "Translator.WritableTarget" {
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, error.Unexpected),
    });
    const WritableTarget = c_to_zig.WritableTarget;
    const Null = @TypeOf(null);
    const T1 = WritableTarget(*usize) orelse Null;
    try expectEqual(T1, usize);
    const T2 = WritableTarget(*const usize) orelse Null;
    try expectEqual(T2, Null);
    const T3 = WritableTarget(*void) orelse Null;
    try expectEqual(T3, Null);
    const T4 = WritableTarget(*anyopaque) orelse Null;
    try expectEqual(T4, Null);
    const T5 = WritableTarget(*opaque {}) orelse Null;
    try expectEqual(T5, Null);
    const T6 = WritableTarget(*type) orelse Null;
    try expectEqual(T6, Null);
}

test "Translator.convert (basic)" {
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, error.Unexpected),
    });
    const convert = c_to_zig.convert;
    const OldStruct1 = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct1 = extern struct {
        a: i32,
        b: i32,
    };
    const NewStruct2 = packed struct(u32) {
        flag1: bool = true,
        flag2: bool = false,
        flag3: bool = true,
        _: u29 = 0,
    };
    const StatusEnum = enum(c_uint) {
        ok,
        failure,
        unexpected,
    };
    const new1: NewStruct1 = .{ .a = 123, .b = 456 };
    const old1: OldStruct1 = convert(OldStruct1, new1);
    try expectEqual(old1.number2, 456);
    const old_ptr1 = convert(*const OldStruct1, &new1);
    try expectEqual(old_ptr1.number2, 456);
    const enum1: StatusEnum = .failure;
    const old_enum1 = convert(c_uint, enum1);
    try expectEqual(old_enum1, 1);
    const new2: NewStruct2 = .{};
    const old_enum2 = convert(c_uint, new2);
    try expectEqual(old_enum2, 0b101);
    const old_enum3: c_uint = 0b110;
    const new3 = convert(NewStruct2, old_enum3);
    try expectEqual(new3, NewStruct2{ .flag1 = false, .flag2 = true, .flag3 = true });
}

test "Translator.convert (function pointer)" {
    const OldStruct1 = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct1 = extern struct {
        a: i32,
        b: i32,
    };
    const OldStruct2 = extern struct {
        callback1: *const fn () callconv(.c) void,
        callback2: *const fn (*const OldStruct1) callconv(.c) c_uint,
    };
    const ErrorSet = error{ Failure, Unexpected };
    const StatusEnum = enum(c_uint) {
        ok,
        failure,
        unexpected,
    };
    const NewStruct2 = extern struct {
        cb1: *const fn () callconv(.c) void,
        cb2: *const fn (*const NewStruct1) callconv(.c) StatusEnum,
    };
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = *const OldStruct1, .new = *const NewStruct1 },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, error.Unexpected),
    });
    const convert = c_to_zig.convert;
    const ns = struct {
        var called1 = false;
        var called2 = false;
        var result2: ?NewStruct1 = null;

        fn func1() callconv(.c) void {
            called1 = true;
        }

        fn func2(ptr: *const NewStruct1) callconv(.c) StatusEnum {
            called2 = true;
            result2 = ptr.*;
            return .ok;
        }
    };
    const old1 = convert(OldStruct2, NewStruct2{ .cb1 = ns.func1, .cb2 = ns.func2 });
    old1.callback1();
    try expectEqual(ns.called1, true);
    const input: OldStruct1 = .{ .number1 = 123, .number2 = 456 };
    const res1 = old1.callback2(&input);
    try expectEqual(0, res1);
    try expectEqual(true, ns.called2);
    try expectEqual(NewStruct1{ .a = 123, .b = 456 }, ns.result2);
}

test "Translator.Translated" {
    const OldStruct = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct = extern struct {
        a: i32,
        b: i32,
    };
    const StatusEnum = enum(c_uint) {
        ok,
        failure,
        unexpected,
    };
    const ErrorSet = error{
        Failure,
        Unexpected,
    };
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
            .{ .old = *OldStruct, .new = *NewStruct },
            .{ .old = []const OldStruct, .new = []const NewStruct },
            .{ .old = ?*const OldStruct, .new = *const NewStruct },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, error.Unexpected),
    });
    const Fn1 = c_to_zig.Translated(fn (i32, OldStruct) StatusEnum, true, false, .{});
    try expectEqual(fn (i32, NewStruct) ErrorSet!void, Fn1);
    const Fn2 = c_to_zig.Translated(fn (i32, []const OldStruct) StatusEnum, true, false, .{});
    try expectEqual(fn (i32, []const NewStruct) ErrorSet!void, Fn2);
    const Fn3 = c_to_zig.Translated(fn (i32, *OldStruct) StatusEnum, true, false, .{});
    try expectEqual(fn (i32) ErrorSet!NewStruct, Fn3);
    const Fn4 = c_to_zig.Translated(fn (i32, *bool, *OldStruct) StatusEnum, true, false, .{});
    try expectEqual(fn (i32) ErrorSet!std.meta.Tuple(&.{ bool, NewStruct }), Fn4);
    const Fn5 = c_to_zig.Translated(fn (i32, OldStruct) bool, false, false, .{});
    try expectEqual(fn (i32, NewStruct) bool, Fn5);
    const Fn6 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, false, .{});
    try expectEqual(fn (i32, NewStruct) c_int, Fn6);
    const Fn7 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, true, .{});
    try expectEqual(fn (i32, NewStruct) void, Fn7);
    const Fn8 = c_to_zig.Translated(fn (i32, *bool, ?*const OldStruct) StatusEnum, true, false, .{});
    try expectEqual(fn (i32, *bool, *const NewStruct) ErrorSet!void, Fn8);
}

test "Translator.translate" {
    const OldStruct = extern struct {
        number1: i32,
        number2: i32,
    };
    const NewStruct = extern struct {
        a: i32 = 1,
        b: i32 = 2,
    };
    const StatusEnum = enum(c_uint) {
        ok,
        failure,
        unexpected,
    };
    const ErrorSet = error{
        Failure,
        Unexpected,
    };
    const ActionEnum = enum(c_uint) {
        eat,
        leave,
        shoot,
    };
    const c = struct {
        fn hello(_: OldStruct, _: i32) callconv(.c) c_uint {
            return 0;
        }

        fn world(_: i32, ptr: *OldStruct) callconv(.c) c_uint {
            ptr.* = .{ .number1 = 123, .number2 = 456 };
            return 0;
        }
    };
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, error.Unexpected),
    });
    _ = ActionEnum;
    const func1 = c_to_zig.translate("hello", true, false, .{});
    try expectEqual(@TypeOf(func1), fn (NewStruct, i32) ErrorSet!void);
    try func1(.{}, 123);
}

test "Translator.SliceMerged" {
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .error_scheme = NullErrorScheme,
    });
    const SliceMerged = c_to_zig.SliceMerged;
    const Fn1 = SliceMerged(fn (usize, *const u8) usize, &.{
        .{ .len_index = 0, .ptr_index = 1 },
    });
    try expectEqual(fn ([]const u8) usize, Fn1);
    const Fn2 = SliceMerged(fn (*anyopaque, *const u8, usize, *const u8, usize) void, &.{
        .{ .len_index = 2, .ptr_index = 1 },
        .{ .len_index = 4, .ptr_index = 3 },
    });
    try expectEqual(fn (*anyopaque, []const u8, []const u8) void, Fn2);
}

test "Translator.mergeSlice" {
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .error_scheme = NullErrorScheme,
    });
    const mergeSlice = c_to_zig.mergeSlice;
    const ns = struct {
        var ptr_received: ?[*]const u8 = null;
        var len_received: ?u32 = null;

        fn call1(len: u32, ptr: [*]const u8) void {
            len_received = len;
            ptr_received = ptr;
        }

        fn call2(len: u32, ptr_maybe: ?*const anyopaque) void {
            if (ptr_maybe) |ptr| {
                len_received = len;
                ptr_received = @ptrCast(ptr);
            } else {
                len_received = 0;
                ptr_received = null;
            }
        }
    };
    const f1 = mergeSlice(ns.call1, &.{
        .{ .len_index = 0, .ptr_index = 1 },
    });
    f1("Hello world");
    try expectEqual(11, ns.len_received);
    try expectEqualSlices(u8, "Hello world", ns.ptr_received.?[0..11]);
    const f2 = mergeSlice(ns.call2, &.{
        .{ .len_index = 0, .ptr_index = 1 },
    });
    f2(null);
    try expectEqual(0, ns.len_received);
    f2("Hello world");
    try expectEqual(11, ns.len_received);
    try expectEqualSlices(u8, "Hello world", ns.ptr_received.?[0..11]);
}
