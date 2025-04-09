const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

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
    callbacks_return_default_success_status: bool = true,
};
pub fn BasicErrorScheme(
    old_enum_type: type,
    new_error_set: type,
    options: struct {
        default_success_status: old_enum_type,
        default_failure_status: old_enum_type,
        default_error: new_error_set,
    },
) type {
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
        error_enum_buffer[index] = .{
            .status = options.default_failure_status,
            .err = @field(new_error_set, e.name),
        };
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
            non_error_status_buffer[0] = 0;
            non_error_status_count += 1;
        },
        .bool => {
            non_error_status_buffer[0] = true;
            non_error_status_count += 1;
        },
        .void => {
            non_error_status_buffer[0] = {};
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
        pub const PositiveStatus = if (non_error_statuses.len > 1) Status else void;
        pub const Result = union(enum) {
            status: PositiveStatus,
            err: anyerror,

            const default: PositiveStatus = if (PositiveStatus == void) {} else options.default_success_status;
        };

        pub fn fromEnum(arg: Status) Result {
            return if (std.mem.indexOfScalar(Status, &non_error_statuses, arg)) |_| .{
                .status = if (PositiveStatus == void) {} else arg,
            } else for (error_enum_table) |entry| {
                if (entry.status == arg) break .{ .err = entry.err };
            } else .{
                .err = options.default_error,
            };
        }

        pub fn toEnum(arg: Result) Status {
            return switch (arg) {
                .status => |s| if (PositiveStatus == void) options.default_success_status else s,
                .err => |e| for (error_enum_table) |entry| {
                    if (entry.err == e) break entry.status;
                } else options.default_failure_status,
            };
        }
    };
}
pub const CodeGeneratorOptions = struct {
    include_paths: []const []const u8,
    header_paths: []const []const u8,
    translater: []const u8 = "c_to_zig",
    error_set: []const u8 = "Error",
    c_error_type: []const u8,
    c_import: []const u8 = "c",
    c_root_struct: ?[]const u8 = null,

    always_return_error_union: bool = false,
    ignore_non_default_success_status: bool = true,
    callbacks_return_default_success_status: bool = true,
    add_simple_test: bool = true,

    // callback determining which declarations to include
    filter_fn: fn (name: []const u8) bool,

    // callback determining which enum items represent errors
    enum_is_error_fn: fn (item_name: []const u8, value: i128) bool = nonZeroValue,
    // callback determining if an enum type should be a bitflags packed struct
    enum_is_packed_struct_fn: fn (enum_name: []const u8) bool = neverPackedStruct,

    // callbacks setting pointer attributes
    ptr_is_many_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = ifCharType,
    ptr_is_null_terminated_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = ifCharType,
    ptr_is_optional_fn: fn (ptr_type: []const u8, child_type: []const u8) bool = neverOptional,

    // callback distinguishing in/out pointers from output pointers
    param_is_input_fn: fn (fn_name: []const u8, param_name: ?[]const u8, param_index: usize, param_type: []const u8) bool = alwaysOutput,

    // callbacks adjusting naming convention
    fn_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    type_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    const_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    param_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = removeArgPrefix,
    field_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    enum_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    error_name_fn: fn (std.mem.Allocator, name: []const u8) std.mem.Allocator.Error![]const u8 = noChange,

    pub fn noChange(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
        return arg;
    }

    pub fn removeArgPrefix(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
        return if (std.mem.startsWith(u8, arg, "arg_")) arg[4..] else arg;
    }

    pub fn nonZeroValue(_: []const u8, value: i128) bool {
        return value != 0;
    }

    pub fn neverPackedStruct(_: []const u8) bool {
        return false;
    }

    pub fn neverOptional(_: []const u8, _: []const u8) bool {
        return false;
    }

    pub fn alwaysInput(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) bool {
        return true;
    }

    pub fn alwaysOutput(_: []const u8, _: ?[]const u8, _: usize, _: []const u8) bool {
        return false;
    }

    pub fn ifCharType(_: []const u8, target_type: []const u8) bool {
        const char_types: []const []const u8 = &.{ "u8", "wchar_t", "char16_t" };
        return for (char_types) |char_type| {
            if (std.mem.eql(u8, target_type, char_type)) break true;
        } else false;
    }
};

pub fn Translator(comptime options: TranslatorOptions) type {
    return struct {
        pub fn Translated(
            comptime OldFn: anytype,
            comptime return_error_union: bool,
            comptime ignore_non_error_return_value: bool,
            comptime local_subs: anytype,
            comptime is_callback: bool,
        ) type {
            const old_fn = @typeInfo(OldFn).@"fn";
            const OldRT = old_fn.return_type.?;
            const extra = switch (ignore_non_error_return_value) {
                true => 0,
                false => switch (return_error_union) {
                    // room for an extra type when there're multiple status codes indicating success
                    true => if (options.error_scheme.PositiveStatus != void) 1 else 0,
                    // room for an extra type when the return value isn't an error code or void
                    false => if (OldRT != void) 1 else 0,
                },
            };
            // look for non-const pointers, scanning backward
            const OutputTypes = init: {
                var types: [old_fn.params.len + extra]type = undefined;
                if (extra == 1) {
                    types[old_fn.params.len] = switch (return_error_union) {
                        true => options.error_scheme.PositiveStatus,
                        false => Substitute(OldRT, local_subs, null, old_fn.params.len),
                    };
                }
                const start_index = inline for (0..old_fn.params.len) |j| {
                    const i = old_fn.params.len - j - 1;
                    const Target = WritableTarget(old_fn.params[i].type.?) orelse break i + 1;
                    // see if the pointer is attributed as in/out
                    if (getTypeWithAttributes(local_subs, null, 0)) |type_wa| {
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
            const Error = if (is_callback) anyerror else options.error_scheme.ErrorSet;
            const NewRT = if (return_error_union) Error!Payload else Payload;
            return @Type(.{
                .@"fn" = .{
                    .calling_convention = .auto,
                    .is_generic = false,
                    .is_var_args = false,
                    .return_type = NewRT,
                    .params = &params,
                },
            });
        }

        pub fn translate(
            comptime fn_name: []const u8,
            comptime return_error_union: bool,
            comptime ignore_non_error_return_value: bool,
            local_subs: anytype,
        ) Translated(
            @TypeOf(@field(options.c_import_ns, fn_name)),
            return_error_union,
            ignore_non_error_return_value,
            local_subs,
            false,
        ) {
            const OldFn = @TypeOf(@field(options.c_import_ns, fn_name));
            const NewFn = Translated(
                OldFn,
                return_error_union,
                ignore_non_error_return_value,
                local_subs,
                false,
            );
            const NewRT = @typeInfo(NewFn).@"fn".return_type.?;
            const Payload = switch (@typeInfo(NewRT)) {
                .error_union => |eu| eu.payload,
                else => NewRT,
            };
            const ns = struct {
                inline fn call(new_args: std.meta.ArgsTuple(NewFn)) NewRT {
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
                    const last = if (output_count > 1) payload.len - 1 else 0;
                    // add pointers to result
                    switch (pointer_count) {
                        1 => old_args[new_args.len] = @ptrCast(&payload),
                        else => inline for (new_args.len..old_args.len) |i| {
                            const ArgT = @TypeOf(old_args[i]);
                            old_args[i] = convert(ArgT, &payload[i - new_args.len]);
                        },
                    }
                    // get function
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
                    if (return_error_union) {
                        // see if the call encountered an error
                        const status = convert(options.error_scheme.Status, old_rv);
                        switch (options.error_scheme.fromEnum(status)) {
                            .err => |e| return @errorCast(e),
                            .status => |s| if (extra > 0) {
                                // add positive status to result
                                if (last > 0) payload[last] = s else payload = s;
                            },
                        }
                    } else if (extra > 0) {
                        if (last > 0) {
                            payload[last] = convert(@TypeOf(payload[last]), old_rv);
                        } else {
                            payload = convert(@TypeOf(payload), old_rv);
                        }
                    }
                    return payload;
                }
            };
            return fn_transform.spreadArgs(ns.call, .auto);
        }

        fn ignoreReturnValue(comptime FT: type) bool {
            return if (@typeInfo(FT).@"fn".return_type == options.error_scheme.Status)
                options.callbacks_return_default_success_status
            else
                false;
        }

        pub fn translateCallback(
            comptime FT: type,
            comptime func: Translated(FT, true, ignoreReturnValue(FT), .{}, true),
        ) *const FT {
            const NewCallbackFn = @TypeOf(func);
            const OldCallbackFn = FT;
            const NewRT = @typeInfo(NewCallbackFn).@"fn".return_type.?;
            const OldRT = @typeInfo(OldCallbackFn).@"fn".return_type.?;
            const Payload = @typeInfo(NewRT).error_union.payload;
            const ns = struct {
                inline fn call(old_args: std.meta.ArgsTuple(OldCallbackFn)) OldRT {
                    var new_args: std.meta.ArgsTuple(NewCallbackFn) = undefined;
                    // copy arguments
                    inline for (0..new_args.len) |i| {
                        new_args[i] = convert(@TypeOf(new_args[i]), old_args[i]);
                    }
                    // call new callback
                    var result: options.error_scheme.Result = undefined;
                    if (@call(.auto, func, new_args)) |payload| {
                        // set pointers
                        const pointer_count = old_args.len - new_args.len;
                        const output_count = switch (@typeInfo(Payload)) {
                            .@"struct" => |st| if (st.is_tuple) payload.len else 1,
                            .void => 0,
                            else => 1,
                        };
                        const last = if (output_count > 1) payload.len - 1 else 0;
                        const extra = output_count - pointer_count;
                        inline for (new_args.len..old_args.len) |i| {
                            const j = i - new_args.len;
                            const ptr = old_args[i].*;
                            if (ptr != null) ptr.* = payload[j];
                        }
                        result = .{
                            .status = switch (extra > 0) {
                                true => if (last > 0) payload[last] else payload,
                                false => options.error_scheme.Result.default,
                            },
                        };
                    } else |err| {
                        result = .{ .err = err };
                    }
                    if (OldRT != void) {
                        const status = options.error_scheme.toEnum(result);
                        return convert(OldRT, status);
                    }
                }
            };
            return fn_transform.spreadArgs(ns.call, .c);
        }

        inline fn convert(comptime T: type, arg: anytype) T {
            const AT = @TypeOf(arg);
            const a = @typeInfo(AT);
            return switch (@typeInfo(T)) {
                .int => switch (@typeInfo(AT)) {
                    .@"enum" => @intFromEnum(arg),
                    .bool => if (arg) 1 else 0,
                    else => @bitCast(arg),
                },
                .pointer => |pt| switch (@typeInfo(pt.child)) {
                    .@"fn" => translateCallback(pt.child, arg),
                    inline .@"struct", .@"union" => switch (@typeInfo(a.pointer.child)) {
                        inline .@"struct", .@"union" => |st| switch (st.layout) {
                            .@"extern" => @ptrCast(arg),
                            else => &copyContainer(pt.child, arg.*),
                        },
                        else => @ptrCast(arg),
                    },
                    else => @ptrCast(arg),
                },
                inline .@"struct", .@"union" => switch (a) {
                    inline .@"struct", .@"union" => |st| switch (st.layout) {
                        .@"extern" => @bitCast(arg),
                        else => copyContainer(T, arg),
                    },
                    else => @bitCast(arg),
                },
                .@"enum" => @enumFromInt(arg),
                else => @bitCast(arg),
            };
        }

        inline fn copyContainer(comptime T: type, src: anytype) T {
            var dst: T = undefined;
            switch (@typeInfo(@TypeOf(src))) {
                .@"struct" => |src_st| {
                    const dst_st = @typeInfo(T).@"struct";
                    inline for (src_st.fields, 0..) |src_field, i| {
                        const dst_field = dst_st.fields[i];
                        const src_value = @field(src, src_field.name);
                        @field(dst, dst_field.name) = convert(dst_field.type, src_value);
                    }
                },
                .@"union" => |src_un| {
                    const dst_un = @typeInfo(T).@"union";
                    if (src_un.tag_type) |TT| {
                        const tag: TT = src;
                        inline for (src_un.fields, 0..) |src_field, i| {
                            if (tag == @field(TT, src_field.name)) {
                                const dst_field = dst_un.fields[i];
                                const src_value = @field(src, src_field.name);
                                @field(dst, dst_field.name) = convert(dst_field.type, src_value);
                                break;
                            }
                        }
                    } else {
                        var sel_field = undefined;
                        var dst_field = undefined;
                        inline for (src_un.fields, 0..) |src_field, i| {
                            if (i == 0 or @sizeOf(src_field.type) > @sizeOf(sel_field.type)) {
                                sel_field = src_field;
                                dst_field = dst_un.fields[i];
                            }
                        }
                        const src_value = @field(src, sel_field.name);
                        @field(dst, dst_field.name) = convert(dst_field.type, src_value);
                    }
                },
                else => @compileError("Unexpected type"),
            }
            return dst;
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

pub fn CodeGenerator(comptime options: CodeGeneratorOptions) type {
    return struct {
        const Ast = std.zig.Ast;

        arena: std.heap.ArenaAllocator,
        allocator: std.mem.Allocator,
        cwd: []const u8,
        indent_level: usize,
        indented: bool,
        current_root: *Type,
        old_root: *Type,
        old_namespace: Namespace,
        old_to_new_type_map: std.AutoHashMap(*Type, *Type),
        new_to_old_param_map: std.AutoHashMap(*Type, *Type),
        new_root: *Type,
        new_namespace: Namespace,
        new_error_set: *Type,
        error_enums: []const []const u8,
        non_error_enums: []const []const u8,
        void_type: *Type,
        full_error_set: *Type,
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
            self.new_to_old_param_map = .init(self.allocator);
            self.new_root = try self.createType(.{ .container = .{ .kind = "struct" } });
            self.new_namespace = .init(self.allocator);
            self.new_error_set = try self.createType(.{ .error_set = &.{} });
            self.error_enums = &.{};
            self.non_error_enums = &.{};
            self.void_type = try self.createType(.{ .expression = .{ .identifier = "void" } });
            self.full_error_set = try self.createType(.{ .expression = .{ .identifier = "anyerror" } });
            self.type_lookup = null;
            self.write_to_byte_array = false;
            self.byte_array = .init(self.allocator);
            self.current_root = self.old_root;
            self.need_inout_import = false;
            try self.new_namespace.addType("void", self.void_type);
            try self.new_namespace.addType("anyerror", self.full_error_set);
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
            self.current_root = self.new_root;
            try self.printImports();
            try self.printTypeDef(self.new_root);
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

        fn translateDeclarations(self: *@This()) !void {
            // add error set
            try self.deriveErrorSet();
            try self.append(&self.new_root.container.decls, .{
                .name = options.error_set,
                .expr = .{ .type = self.new_error_set },
            });
            // add remaining
            for (self.old_root.container.decls) |decl| {
                if (options.filter_fn(decl.name)) {
                    // get name in target namespace
                    const new_name = if (isFunctionDecl(decl))
                        try options.fn_name_fn(self.allocator, decl.name)
                    else switch (decl.expr) {
                        .type, .identifier => try options.type_name_fn(self.allocator, decl.name),
                        .unknown => try options.const_name_fn(self.allocator, decl.name),
                    };
                    const new_decl_t = if (decl.type) |t| try self.translateType(t, false) else null;
                    const expr = if (isFunctionDecl(decl))
                        try self.obtainTranslateCall(decl, new_decl_t.?)
                    else
                        try self.translateExpression(decl.expr);
                    if (expr == .type) {
                        // don't add declarations for opaques
                        if (expr.type.* == .container and std.mem.eql(u8, expr.type.container.kind, "opaque")) {
                            continue;
                        }
                        try self.new_namespace.addType(new_name, expr.type);
                    }
                    try self.append(&self.new_root.container.decls, .{
                        .name = new_name,
                        .type = new_decl_t,
                        .alignment = decl.alignment,
                        .mutable = false,
                        .expr = expr,
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
            const new_name = try options.field_name_fn(self.allocator, field.name);
            return .{
                .name = new_name,
                .type = try self.translateType(field.type, false),
                .alignment = field.alignment,
            };
        }

        fn createBlankField(self: *@This(), name: []const u8, width: isize) !Field {
            const t = get: {
                if (width > 0) {
                    const type_name = try std.fmt.allocPrint(self.allocator, "u{d}", .{width});
                    break :get self.new_namespace.getType(type_name) orelse add: {
                        const new_t = try self.createType(.{ .expression = .{ .identifier = type_name } });
                        try self.new_namespace.addType(type_name, new_t);
                        break :add new_t;
                    };
                } else {
                    const expr = try std.fmt.allocPrint(self.allocator, "std.meta.Int(.unsigned, @bitSizeOf(c_uint) - {d})", .{-width});
                    break :get try self.createType(.{ .expression = .{ .unknown = expr } });
                }
            };
            return .{
                .name = name,
                .type = t,
            };
        }

        fn translateParameter(self: *@This(), param: Parameter, is_inout: bool) !Parameter {
            const new_name = if (param.name) |n| try options.param_name_fn(self.allocator, n) else null;
            const new_type = try self.translateType(param.type, false);
            return .{
                .name = new_name,
                .type = new_type,
                .is_inout = is_inout,
            };
        }

        fn translateEnumItem(self: *@This(), item: EnumItem) !EnumItem {
            const new_name = try options.enum_name_fn(self.allocator, item.name);
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
            if (p.child_type.* == .function) {
                return if (p.is_optional) try self.createType(.{
                    .optional = child_t,
                }) else child_t;
            }
            const ptr_name = try self.obtainTypeName(t);
            const target_name = try self.obtainTypeName(p.child_type);
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
            const enum_name = try self.obtainTypeName(t);
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
                        blank_field_name = try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ blank_field_name, "_" });
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
            const output_start = for (0..f.parameters.len) |offset| {
                const index = f.parameters.len - offset - 1;
                const param = f.parameters[index];
                if (!self.isWriteTarget(param.type)) break index + 1;
                // maybe it's a in/out pointer--need to ask the callback function;
                // first, we need a name
                const fn_name = for (self.old_root.container.decls) |decl| {
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
                } else try self.obtainTypeName(t);
                const type_name = try self.obtainTypeName(param.type.pointer.child_type);
                if (options.param_is_input_fn(fn_name, param.name, index, type_name)) {
                    inout_index = index;
                    self.need_inout_import = true;
                    break index + 1;
                }
            } else 0;
            for (f.parameters, 0..) |param, index| {
                if (index >= output_start) {
                    const target_type = param.type.pointer.child_type;
                    const output_type = try self.translateType(target_type, true);
                    try self.append(&output_types, output_type);
                    try self.addSubstitution(target_type, output_type);
                }
            }
            // see if the translated function should return an error union
            const return_error_union = is_pointer_target or self.shouldReturnErrorUnion(t);
            const status_type = try self.translateType(f.return_type, false);
            const extra: usize = switch (self.isReturningStatus(t)) {
                false => if (isVoid(f.return_type)) 0 else 1,
                true => switch (self.non_error_enums.len) {
                    0, 1 => 0,
                    else => switch (options.ignore_non_default_success_status) {
                        true => 0,
                        false => 1,
                    },
                },
            };
            if (extra == 1) try self.append(&output_types, status_type);
            const arg_count = f.parameters.len + extra - output_types.len;
            for (f.parameters[0..arg_count], 0..) |param, index| {
                const new_param = try self.translateParameter(param, inout_index == index);
                try self.append(&new_params, new_param);
                try self.addSubstitution(param.type, new_param.type);
            }
            const payload_type = switch (output_types.len) {
                0 => self.void_type,
                1 => output_types[0],
                else => try self.createType(.{ .type_tuple = output_types }),
            };
            const error_set = if (is_pointer_target) self.full_error_set else self.new_error_set;
            const return_type = switch (return_error_union) {
                true => try self.createType(.{
                    .error_union = .{
                        .payload_type = payload_type,
                        .error_set = error_set,
                    },
                }),
                false => payload_type,
            };
            return try self.createType(.{
                .function = .{
                    .parameters = new_params,
                    .return_type = return_type,
                    .alignment = f.alignment,
                },
            });
        }

        fn translateBitField(self: *@This(), item: EnumItem) !Field {
            const new_name = try options.enum_name_fn(self.allocator, item.name);
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

        fn createBitFieldDeclaration(self: *@This(), name: []const u8, fields: []Field) !Declaration {
            const new_name = try options.enum_name_fn(self.allocator, name);
            var pairs: [][]const u8 = &.{};
            for (fields) |field| {
                const assign = try std.fmt.allocPrint(self.allocator, ".{s} = true", .{field.name});
                try self.append(&pairs, assign);
            }
            const set = switch (pairs.len) {
                0 => ".{}",
                else => try std.fmt.allocPrint(self.allocator, ".{s}", .{pairs}),
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
            const new_name = try options.enum_name_fn(self.allocator, name);
            const number = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
            return .{
                .name = new_name,
                .expr = .{ .unknown = number },
            };
        }

        fn addSubstitution(self: *@This(), old_type: *Type, new_type: *Type) !void {
            const is_unique_type = switch (new_type.*) {
                .enumeration, .expression => false,
                else => true,
            };
            if (new_type == old_type or is_unique_type) {
                try self.new_to_old_param_map.put(new_type, old_type);
            }
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

        fn isReturningStatus(self: *@This(), t: *Type) bool {
            const name = self.obtainTypeName(t.function.return_type) catch return false;
            return std.mem.eql(u8, options.c_error_type, name);
        }

        fn shouldReturnErrorUnion(self: *@This(), t: *Type) bool {
            return options.always_return_error_union or self.isReturningStatus(t);
        }

        fn deriveErrorSet(self: *@This()) !void {
            var names: [][]const u8 = &.{};
            var errors: [][]const u8 = &.{};
            var non_errors: [][]const u8 = &.{};
            if (self.old_namespace.getType(options.c_error_type)) |t| {
                if (t.* == .enumeration) {
                    for (t.enumeration.items) |item| {
                        if (options.enum_is_error_fn(item.name, item.value)) {
                            const err_name = try options.error_name_fn(self.allocator, item.name);
                            try self.append(&names, err_name);
                            const en_name = try options.enum_name_fn(self.allocator, item.name);
                            try self.append(&errors, en_name);
                        } else {
                            const en_name = try options.enum_name_fn(self.allocator, item.name);
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

        fn obtainTranslateCall(self: *@This(), decl: Declaration, new_t: *const Type) !Expression {
            const return_error_union = self.shouldReturnErrorUnion(decl.type.?);
            var non_unique_args: [][]const u8 = &.{};
            for (new_t.function.parameters, 0..) |param, index| {
                // we need to do local substitution when the original type isn't unique enough
                // for global replacement and when a non-const pointer is use for input
                if (self.new_to_old_param_map.get(param.type) == null or param.is_inout) {
                    self.current_root = self.new_root;
                    const name = try self.obtainTypeName(param.type);
                    self.current_root = self.old_root;
                    const pair = switch (param.is_inout) {
                        true => try std.fmt.allocPrint(self.allocator, ".@\"{d}\" = inout({s})", .{ index, name }),
                        false => try std.fmt.allocPrint(self.allocator, ".@\"{d}\" = {s}", .{ index, name }),
                    };
                    try self.append(&non_unique_args, pair);
                }
            }
            const ignore_non_error_return_value = switch (self.non_error_enums.len) {
                0, 1 => false,
                else => switch (options.ignore_non_default_success_status) {
                    // ignore the return value if it's a positive status code
                    true => self.isReturningStatus(decl.type.?),
                    false => false,
                },
            };
            if (!return_error_union) {
                const return_type = new_t.function.return_type;
                if (self.new_to_old_param_map.get(return_type) == null) {
                    self.current_root = self.new_root;
                    const name = try self.obtainTypeName(return_type);
                    self.current_root = self.old_root;
                    const pair = try std.fmt.allocPrint(self.allocator, ".retval = {s}", .{name});
                    try self.append(&non_unique_args, pair);
                }
            }
            const local_subs = if (non_unique_args.len > 0) get: {
                const local_subs_pairs = try std.mem.join(self.allocator, ", ", non_unique_args);
                break :get try std.fmt.allocPrint(self.allocator, " {s} ", .{local_subs_pairs});
            } else "";
            const code = try std.fmt.allocPrint(self.allocator, "{s}.translate(\"{s}\", {}, {}, .{{{s}}})", .{
                options.translater,
                decl.name,
                return_error_union,
                ignore_non_error_return_value,
                local_subs,
            });
            return .{ .unknown = code };
        }

        fn obtainTypeName(self: *@This(), t: *Type) ![]const u8 {
            const indent_before = self.indent_level;
            defer self.indent_level = indent_before;
            self.indent_level = 0;
            self.write_to_byte_array = true;
            defer self.write_to_byte_array = false;
            self.byte_array.clearRetainingCapacity();
            self.printTypeRef(t) catch {};
            return try self.allocator.dupe(u8, self.byte_array.items);
        }

        fn printImports(self: *@This()) anyerror!void {
            try self.printTxt("const std = @import(\"std\");\n");
            try self.printTxt("const api_translator = @import(\"api-translator.zig\");\n");
            if (self.need_inout_import) {
                try self.printTxt("const inout = api_translator.inout;\n");
            }
            try self.printFmt("const {s} = @cImport({{\n", .{options.c_import});
            for (options.header_paths) |path| {
                try self.printFmt("@cInclude(\"{s}\");\n", .{path});
            }
            try self.printTxt("}});\n\n");
        }

        fn printTypeRef(self: *@This(), t: *Type) anyerror!void {
            if (t == self.current_root) {
                try self.printTxt("@This()");
            } else if (t == self.old_root) {
                try self.printTxt(options.c_import);
            } else {
                const namespace = if (self.current_root == self.new_root)
                    self.new_namespace
                else
                    self.old_namespace;
                if (namespace.getName(t)) |name| {
                    try self.printFmt("{s}", .{name});
                } else if (self.old_namespace.getName(t)) |name| {
                    try self.printFmt("{s}.{s}", .{ options.c_import, name });
                } else try self.printTypeDef(t);
            }
        }

        fn printTypeDef(self: *@This(), t: *Type) anyerror!void {
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
                    for (c.fields) |field| try self.printField(field);
                    if (c.fields.len > 0 and c.decls.len > 0) try self.printTxt("\n");
                    for (c.decls, 0..) |decl, i| {
                        if (i > 0) {
                            if (isFunctionDecl(decl) or isFunctionDecl(c.decls[i - 1])) {
                                try self.printTxt("\n");
                            }
                        }
                        try self.printDeclaration(decl);
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
                    try self.printTypeRef(p.child_type);
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
                    try self.printTypeRef(e.error_set);
                    try self.printTxt("!");
                    try self.printTypeRef(e.payload_type);
                },
                .function => |f| {
                    try self.printTxt("fn (\n");
                    for (f.parameters) |param| {
                        if (param.name) |n| try self.printFmt("{s}: ", .{n});
                        try self.printTypeRef(param.type);
                        try self.printTxt(",\n");
                    }
                    try self.printTxt(") ");
                    if (f.alignment) |a| try self.printFmt("align({s}) ", .{a});
                    try self.printTypeRef(f.return_type);
                },
                .optional => |ot| {
                    try self.printTxt("?");
                    try self.printTypeRef(ot);
                },
                .type_tuple => |types| {
                    try self.printTxt("std.meta.Tuple(&.{{ ");
                    for (types, 0..) |tt, i| {
                        try self.printTypeRef(tt);
                        if (i != types.len - 1) try self.printTxt(", ");
                    }
                    try self.printTxt(" }})");
                },
                .expression => |e| {
                    switch (e) {
                        .type => |et| try self.printTypeRef(et),
                        .identifier, .unknown => |s| try self.printFmt("{s}", .{s}),
                    }
                },
            }
        }

        fn printField(self: *@This(), field: Field) anyerror!void {
            try self.printFmt("{s}: ", .{field.name});
            try self.printTypeRef(field.type);
            if (field.alignment) |a| try self.printFmt(" align({s})", .{a});
            if (field.default_value) |v| try self.printFmt(" = {s}", .{v});
            try self.printTxt(",\n");
        }

        fn printDeclaration(self: *@This(), decl: Declaration) anyerror!void {
            const mut = if (decl.mutable) "var" else "const";
            try self.printFmt("pub {s} {s}", .{ mut, decl.name });
            if (decl.type) |t| {
                try self.printTxt(": ");
                try self.printTypeRef(t);
            }
            try self.printTxt(" = ");
            switch (decl.expr) {
                .type => |t| try self.printTypeDef(t),
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
            if (!options.callbacks_return_default_success_status) {
                try self.printTxt(".callbacks_return_default_success_status = false,\n");
            }
            try self.printTxt("}});\n");
        }

        fn printTypeSubstitutions(self: *@This()) !void {
            try self.printTxt(".substitutions = &.{{\n");
            var iterator = self.new_to_old_param_map.iterator();
            var added: std.StringHashMap(bool) = .init(self.allocator);
            const Sub = struct { old_name: []const u8, new_name: []const u8 };
            var subs: []Sub = &.{};
            while (iterator.next()) |entry| {
                const new_t = entry.key_ptr.*;
                const old_t = entry.value_ptr.*;
                if (old_t != new_t) {
                    const old_name = try self.obtainTypeName(old_t);
                    if (added.get(old_name) == null) {
                        const new_name = try self.obtainTypeName(new_t);
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
            for (subs) |sub| {
                try self.printFmt(".{{ .old = {s}, .new = {s} }},\n", .{ sub.old_name, sub.new_name });
            }
            try self.printTxt("}},\n");
        }

        fn printErrorScheme(self: *@This()) !void {
            const old_enum_t = self.old_namespace.getType(options.c_error_type) orelse {
                std.debug.print("Unable to find enum type '{s}'\n", .{options.c_error_type});
                return error.Unexpected;
            };
            const new_enum_t = try self.translateType(old_enum_t, false);
            const enum_name = try self.obtainTypeName(new_enum_t);
            try self.printFmt(".error_scheme = api_translator.BasicErrorScheme({s}, {s}, .{{\n", .{ enum_name, options.error_set });
            const def_pos_status, const def_neg_status = if (new_enum_t.* == .enumeration) .{
                try std.fmt.allocPrint(self.allocator, ".{s}", .{self.non_error_enums[0]}),
                try std.fmt.allocPrint(self.allocator, ".{s}", .{self.error_enums[0]}),
            } else if (std.mem.eql(u8, enum_name, "bool")) .{
                "true",
                "false",
            } else if (std.mem.eql(u8, enum_name, "void")) .{
                "{}",
                "{}",
            } else .{
                "0",
                "1",
            };
            try self.printFmt(".default_success_status = {s},\n", .{def_pos_status});
            try self.printFmt(".default_failure_status = {s},\n", .{def_neg_status});
            try self.printFmt(".default_error = {s}.Unexpected,\n", .{options.error_set});
            try self.printTxt("}}),\n");
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
                const arg = try std.fmt.allocPrint(self.allocator, "-I{s}", .{include_path});
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
            need_upper = true;
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
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, .{
            .default_success_status = 0,
            .default_failure_status = 1,
            .default_error = error.Unexpected,
        }),
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
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, .{
            .default_success_status = 0,
            .default_failure_status = 1,
            .default_error = error.Unexpected,
        }),
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
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, .{
            .default_success_status = 0,
            .default_failure_status = 1,
            .default_error = error.Unexpected,
        }),
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
        .error_scheme = BasicErrorScheme(c_uint, error{Unexpected}, .{
            .default_success_status = 0,
            .default_failure_status = 1,
            .default_error = error.Unexpected,
        }),
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
    const NewStruct2 = struct {
        a: i32,
        b: i32,
    };
    const NewStruct3 = packed struct(u32) {
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
    const new2: NewStruct2 = .{ .a = 123, .b = 456 };
    const old2: OldStruct1 = convert(OldStruct1, new2);
    try expectEqual(old2.number2, 456);
    const old_ptr2 = convert(*const OldStruct1, &new2);
    try expectEqual(old_ptr2.number2, 456);
    const enum1: StatusEnum = .failure;
    const old_enum1 = convert(c_uint, enum1);
    try expectEqual(old_enum1, 1);
    const new3: NewStruct3 = .{};
    const old_enum2 = convert(c_uint, new3);
    try expectEqual(old_enum2, 0b101);
    const old_enum3: c_uint = 0b110;
    const new4 = convert(NewStruct3, old_enum3);
    try expectEqual(new4, NewStruct3{ .flag1 = false, .flag2 = true, .flag3 = true });
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
    const OldStruct2 = struct {
        callback1: *const fn () callconv(.c) void,
        callback2: *const fn (*const OldStruct1) callconv(.c) c_uint,
    };
    const ErrorSet = error{ Failure, Unexpected };
    const NewStruct2 = struct {
        cb1: fn () anyerror!void,
        cb2: fn (*const NewStruct1) anyerror!void,
    };
    const StatusEnum = enum(c_uint) {
        ok,
        failure,
        unexpected,
    };
    const c = struct {};
    const c_to_zig = Translator(.{
        .c_import_ns = c,
        .substitutions = &.{
            .{ .old = *const OldStruct1, .new = *const NewStruct1 },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, .{
            .default_success_status = .ok,
            .default_failure_status = .failure,
            .default_error = error.Unexpected,
        }),
    });
    const convert = c_to_zig.convert;
    const ns = struct {
        var called1 = false;
        var called2 = false;
        var result2: ?NewStruct1 = null;

        fn func1() anyerror!void {
            called1 = true;
        }

        fn func2(ptr: *const NewStruct1) anyerror!void {
            called2 = true;
            result2 = ptr.*;
        }

        fn func3(_: *const NewStruct1) anyerror!void {
            return error.Unexpected;
        }

        fn func4(_: *const NewStruct1) anyerror!void {
            return error.Failure;
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
    const old2 = convert(OldStruct2, NewStruct2{ .cb1 = ns.func1, .cb2 = ns.func3 });
    const res2 = old2.callback2(&input);
    try expectEqual(2, res2);
    const old3 = convert(OldStruct2, NewStruct2{ .cb1 = ns.func1, .cb2 = ns.func4 });
    const res3 = old3.callback2(&input);
    try expectEqual(1, res3);
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
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, .{
            .default_success_status = .ok,
            .default_failure_status = .failure,
            .default_error = error.Unexpected,
        }),
    });
    const Fn1 = c_to_zig.Translated(fn (i32, OldStruct) StatusEnum, true, false, .{}, false);
    try expectEqual(Fn1, fn (i32, NewStruct) ErrorSet!void);
    const Fn2 = c_to_zig.Translated(fn (i32, []const OldStruct) StatusEnum, true, false, .{}, false);
    try expectEqual(Fn2, fn (i32, []const NewStruct) ErrorSet!void);
    const Fn3 = c_to_zig.Translated(fn (i32, *OldStruct) StatusEnum, true, false, .{}, false);
    try expectEqual(Fn3, fn (i32) ErrorSet!NewStruct);
    const Fn4 = c_to_zig.Translated(fn (i32, *bool, *OldStruct) StatusEnum, true, false, .{}, false);
    try expectEqual(Fn4, fn (i32) ErrorSet!std.meta.Tuple(&.{ bool, NewStruct }));
    const Fn5 = c_to_zig.Translated(fn (i32, OldStruct) bool, false, false, .{}, false);
    try expectEqual(Fn5, fn (i32, NewStruct) bool);
    const Fn6 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, false, .{}, false);
    try expectEqual(Fn6, fn (i32, NewStruct) c_int);
    const Fn7 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, true, .{}, false);
    try expectEqual(Fn7, fn (i32, NewStruct) void);
    const Fn8 = c_to_zig.Translated(fn (i32, *bool, ?*const OldStruct) StatusEnum, true, false, .{}, false);
    try expectEqual(Fn8, fn (i32, *bool, *const NewStruct) ErrorSet!void);
    const Fn9 = c_to_zig.Translated(fn (i32, *bool, ?*const OldStruct) StatusEnum, true, false, .{}, true);
    try expectEqual(Fn9, fn (i32, *bool, *const NewStruct) anyerror!void);
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
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, .{
            .default_success_status = .ok,
            .default_failure_status = .failure,
            .default_error = error.Unexpected,
        }),
    });
    _ = ActionEnum;
    const func1 = c_to_zig.translate("hello", true, false, .{});
    try expectEqual(@TypeOf(func1), fn (NewStruct, i32) ErrorSet!void);
    try func1(.{}, 123);
}

test "CodeGenerator (alpha)" {
    const ns = struct {
        const prefix = "alpha_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn getFieldName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, 0);
        }

        fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, prefix.len);
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"./test"},
        .header_paths = &.{"alpha.c"},
        .c_error_type = "alpha_status",
        .filter_fn = ns.filter,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    generator.analyze() catch |err| {
        // skip the code generation when we're not in the right directory
        return if (err == error.FileNotFound) {} else err;
    };
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "test/alpha.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (beta)" {
    const ns = struct {
        const prefix = "beta_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn isError(_: []const u8, value: i128) bool {
            return value < 0;
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn getFieldName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, 0);
        }

        fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, prefix.len);
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"./test"},
        .header_paths = &.{"beta.c"},
        .c_error_type = "beta_status",
        .filter_fn = ns.filter,
        .enum_is_error_fn = ns.isError,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    generator.analyze() catch |err| {
        // skip the code generation when we're not in the right directory
        return if (err == error.FileNotFound) {} else err;
    };
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "test/beta.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (beta, return status)" {
    const ns = struct {
        const prefix = "beta_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn isError(_: []const u8, value: i128) bool {
            return value < 0;
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn getFieldName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, 0);
        }

        fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, prefix.len);
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"./test"},
        .header_paths = &.{"beta.c"},
        .c_error_type = "beta_status",
        .ignore_non_default_success_status = false,
        .filter_fn = ns.filter,
        .enum_is_error_fn = ns.isError,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    generator.analyze() catch |err| {
        // skip the code generation when we're not in the right directory
        return if (err == error.FileNotFound) {} else err;
    };
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "test/beta-with-status.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}
