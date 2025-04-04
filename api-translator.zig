const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;

pub const TypeSubstitution = struct {
    old: type,
    new: type,
};
pub const TranslatorOptions = struct {
    substitutions: []const TypeSubstitution = &.{},
    error_scheme: type,
    ignore_cb_status: bool = true,
};
pub const CodeGeneratorOptions = struct {
    include_paths: []const []const u8,
    header_paths: []const []const u8,
    translater: []const u8 = "c_to_zig",
    error_set: []const u8 = "Error",
    c_error_type: []const u8,
    c_import: []const u8 = "c",
    c_root_struct: ?[]const u8 = null,
    add_simple_test: bool = true,

    filter_fn: fn ([]const u8) bool,

    enum_is_error_fn: fn ([]const u8, i128) bool = nonZero,
    ptr_is_many_fn: fn ([]const u8, []const u8) bool = ifCharType,
    ptr_is_null_terminated_fn: fn ([]const u8, []const u8) bool = ifCharType,
    ptr_is_optional_fn: fn ([]const u8, []const u8) bool = neverOptional,
    param_is_input_fn: fn ([]const u8, ?[]const u8, usize, []const u8) bool = alwaysOutput,

    fn_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    type_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    const_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    param_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    field_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    enum_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,
    error_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = noChange,

    pub fn noChange(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
        return arg;
    }

    pub fn nonZero(_: []const u8, value: i128) bool {
        return value != 0;
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

pub fn BasicErrorScheme(
    old_enum_type: type,
    new_error_set: type,
    options: struct {
        non_error_statuses: []const old_enum_type,
        default_status: old_enum_type,
        default_error: new_error_set,
    },
) type {
    return struct {
        pub const Status = old_enum_type;
        pub const ErrorSet = new_error_set;
        pub const PositiveStatus = if (options.non_error_statuses.len > 1) Status else void;
        pub const Result = union(enum) {
            status: PositiveStatus,
            err: ErrorSet,

            const default: PositiveStatus = if (PositiveStatus == void) {} else options.non_error_status[0];
        };

        const error_list = init: {
            const es = @typeInfo(ErrorSet).error_set.?;
            var list: [es.len]ErrorSet = undefined;
            for (es, 0..) |e, index| {
                list[index] = @field(ErrorSet, e.name);
            }
            break :init list;
        };
        const error_status_list = init: {
            switch (@typeInfo(Status)) {
                .@"enum" => |en| {
                    // leave out ones that appear in non_error_status
                    var values: [en.fields.len - options.non_error_statuses.len]Status = undefined;
                    var index: usize = 0;
                    for (en.fields) |field| {
                        const value = @field(Status, field.name);
                        if (std.mem.indexOfScalar(Status, options.non_error_statuses, value) == null) {
                            values[index] = value;
                            index += 1;
                        }
                    }
                    break :init values;
                },
                .bool => {
                    break :init [1]bool{false};
                },
                .int => {
                    // assume 0 means success
                    var values: [error_list.len]Status = undefined;
                    var value: Status = 1;
                    for (&values) |*ptr| {
                        ptr.* = value;
                        value += 1;
                    }
                    break :init values;
                },
                else => @compileLog("Unrecognized status type"),
            }
        };

        pub fn fromEnum(arg: Status) Result {
            const status = if (@typeInfo(Status) == .int) @abs(arg) else arg;
            if (std.mem.indexOfScalar(Status, options.non_error_statuses, status)) |_| {
                return .{ .status = if (PositiveStatus == void) {} else status };
            } else if (std.mem.indexOfScalar(Status, &error_status_list, status)) |index| {
                return .{ .err = error_list[index] };
            } else {
                return .{ .err = options.default_error };
            }
        }

        pub fn toEnum(arg: Result) Status {
            return switch (arg) {
                .status => |s| if (PositiveStatus == void) options.non_error_statuses[0] else s,
                .err => |e| if (std.mem.indexOfScalar(ErrorSet, error_list, e)) |index|
                    error_status_list[index]
                else
                    options.default_status,
            };
        }
    };
}

pub fn Translator(comptime options: TranslatorOptions) type {
    return struct {
        pub fn Translated(
            comptime OldFn: anytype,
            comptime can_fail: bool,
            comptime ignore_status: bool,
            comptime local_subs: anytype,
        ) type {
            const old_fn = @typeInfo(OldFn).@"fn";
            const OldRT = old_fn.return_type.?;
            const extra = switch (ignore_status) {
                true => 0,
                false => switch (can_fail) {
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
                    types[old_fn.params.len] = switch (can_fail) {
                        true => options.error_scheme.PositiveStatus,
                        false => Substitute(OldRT, local_subs, null, old_fn.params.len),
                    };
                }
                const start_index = inline for (0..old_fn.params.len) |j| {
                    const i = old_fn.params.len - j - 1;
                    const Target = WritableTarget(old_fn.params[i].type.?) orelse break i + 1;
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
            const NewError = options.error_scheme.ErrorSet;
            const NewRT = if (can_fail) NewError!Payload else Payload;
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
            comptime func: anytype,
            comptime can_fail: bool,
            comptime ignore_status: bool,
            local_subs: anytype,
        ) Translated(@TypeOf(func), can_fail, ignore_status, local_subs) {
            const OldFn = @TypeOf(func);
            const NewFn = Translated(OldFn, can_fail, ignore_status, local_subs);
            const NewRT = @typeInfo(NewFn).@"fn".return_type.?;
            const Payload = @typeInfo(NewRT).error_union.payload;
            const ns = struct {
                inline fn call(new_args: std.meta.ArgsTuple(NewFn)) NewRT {
                    var old_args: std.meta.ArgsTuple(OldFn) = undefined;
                    // copy arguments
                    inline for (new_args, 0..) |new_arg, i| {
                        const ArgT = @TypeOf(old_args[i]);
                        old_args[i] = cast(ArgT, new_arg);
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
                    switch (output_count) {
                        1 => old_args[new_args.len] = @ptrCast(&payload),
                        else => for (new_args.len..old_args.len) |i| {
                            const ArgT = @TypeOf(old_args[i]);
                            old_args[i] = cast(ArgT, &payload[i - new_args.len]);
                        },
                    }
                    // call original function
                    const old_rv = @call(.auto, func, old_args);
                    if (can_fail) {
                        // see if the call encountered an error
                        const status = cast(options.error_scheme.Status, old_rv);
                        switch (options.error_scheme.fromEnum(status)) {
                            .err => |e| return e,
                            .status => |s| if (extra > 0) {
                                // add positive status to result
                                if (last > 0) payload[last] = s else payload = s;
                            },
                        }
                    } else if (extra > 0) {
                        if (last > 0) {
                            payload[last] = cast(@TypeOf(payload[last]), old_rv);
                        } else {
                            payload = cast(@TypeOf(payload), old_rv);
                        }
                    }
                    return payload;
                }
            };
            return fn_transform.spreadArgs(ns.call, .auto);
        }

        pub fn translateCallback(
            comptime FT: type,
            comptime func: Translated(FT, true, options.ignore_cb_status, .{}),
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
                        new_args[i] = cast(@TypeOf(new_args[i]), old_args[i]);
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
                        return @bitCast(status);
                    }
                }
            };
            return fn_transform.spreadArgs(ns.call, .c);
        }

        fn cast(comptime T: type, arg: anytype) T {
            const AT = @TypeOf(arg);
            return switch (@typeInfo(T)) {
                .pointer => |pt| switch (@typeInfo(pt.child)) {
                    .@"fn" => &translateCallback(pt.child, arg),
                    else => @ptrCast(arg),
                },
                .@"enum" => @enumFromInt(arg),
                .int => if (@typeInfo(AT) == .@"enum") @intFromEnum(arg) else arg,
                else => @bitCast(arg),
            };
        }

        fn SwitchType(comptime T: type, comptime dir: enum { old_to_new, new_to_old }) type {
            return inline for (options.substitutions) |sub| {
                if (dir == .old_to_new) {
                    if (T == sub.old) break sub.new;
                } else {
                    if (T == sub.new) break sub.old;
                }
            } else switch (@typeInfo(T)) {
                .pointer => |pt| define: {
                    if (@typeInfo(pt.child) == .@"fn" and dir == .old_to_new) {
                        // use function type of translated callback, since translation
                        // requires the actual function
                        break :define Translated(pt.child, true, options.ignore_cb_status, .{});
                    }
                    var new_pt = pt;
                    if (pt.is_const) {
                        // look for substitution of non-const pointer
                        new_pt.is_const = false;
                        const PT = SwitchType(@Type(.{ .pointer = new_pt }), dir);
                        new_pt = @typeInfo(PT).pointer;
                        new_pt.is_const = true;
                    } else {
                        // look for substitution of child
                        new_pt.child = SwitchType(pt.child, dir);
                    }
                    break :define @Type(.{ .pointer = new_pt });
                },
                .array => |ar| define: {
                    var new_ar = ar;
                    new_ar.child = SwitchType(ar.child, dir);
                    break :define @Type(.{ .array = new_ar });
                },
                .optional => |op| define: {
                    var new_op = op;
                    new_op.child = SwitchType(op.child, dir);
                    break :define @Type(.{ .optional = new_op });
                },
                .error_union => |eu| define: {
                    var new_eu = eu;
                    new_eu.payload = SwitchType(eu.payload, dir);
                    break :define @Type(.{ .error_union = new_eu });
                },
                else => T,
            };
        }

        fn Substitute(comptime T: type, tuple: anytype, arg_index: ?usize, arg_count: usize) type {
            const keys = if (arg_index) |index| .{
                std.fmt.comptimePrint("{d}", .{index}),
                std.fmt.comptimePrint("{d}", .{-@as(isize, @intCast(arg_count - index))}),
            } else .{"retval"};
            return inline for (keys) |key| {
                // look for type in function-specific tuple first
                if (@hasField(@TypeOf(tuple), key)) break @field(tuple, key);
            } else SwitchType(T, .old_to_new);
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
    type_tuple: []*Type,
    expression: Expression,
};
pub const Field = struct {
    name: []const u8,
    type: *Type,
    alignment: ?[]const u8 = null,
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
        cwd_path: []const u8,
        indent_level: usize,
        indented: bool,
        current_root: *Type,
        old_root: *Type,
        old_namespace: Namespace,
        old_to_new_type_map: std.AutoHashMap(*Type, *Type),
        new_root: *Type,
        new_namespace: Namespace,
        new_error_set: *Type,
        non_error_enums: []const []const u8,
        void_type: *Type,
        type_lookup: ?*Type,
        write_to_byte_array: bool,
        byte_array: std.ArrayList(u8),
        output_writer: std.io.AnyWriter,

        pub fn init(allocator: std.mem.Allocator) !*@This() {
            var arena: std.heap.ArenaAllocator = .init(allocator);
            var self = try arena.allocator().create(@This());
            self.arena = arena;
            self.allocator = self.arena.allocator();
            self.cwd_path = try std.process.getCwdAlloc(self.allocator);
            self.indent_level = 0;
            self.indented = false;
            self.old_root = try self.createType(.{ .container = .{ .kind = "struct" } });
            self.old_namespace = .init(self.allocator);
            self.old_to_new_type_map = .init(self.allocator);
            self.new_root = try self.createType(.{ .container = .{ .kind = "struct" } });
            self.new_namespace = .init(self.allocator);
            self.new_error_set = try self.createType(.{ .error_set = &.{} });
            self.non_error_enums = &.{};
            self.void_type = try self.createType(.{ .expression = .{ .identifier = "void" } });
            self.type_lookup = null;
            self.write_to_byte_array = false;
            self.byte_array = .init(self.allocator);
            self.current_root = self.old_root;
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
                    const is_function = decl.type != null and decl.type.?.* == .function;
                    // get name in target namespace
                    const new_name = if (is_function)
                        try options.fn_name_fn(self.allocator, decl.name)
                    else switch (decl.expr) {
                        .type, .identifier => try options.type_name_fn(self.allocator, decl.name),
                        .unknown => try options.const_name_fn(self.allocator, decl.name),
                    };
                    const new_decl_t = if (decl.type) |t| try self.obtainTranslatedType(t) else null;
                    const expr = if (is_function)
                        try self.getTranslateCall(decl, new_decl_t.?)
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
                .type => |t| .{ .type = try self.obtainTranslatedType(t) },
                .identifier => |i| if (self.old_namespace.getType(i)) |t| .{
                    .type = translate: {
                        // if the attempt to translate the type comes back here, then
                        // just use the old name, which should refer to a builtin type
                        if (self.type_lookup == t) {
                            break :translate t;
                        } else {
                            self.type_lookup = t;
                            defer self.type_lookup = null;
                            break :translate try self.obtainTranslatedType(t);
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
                .type = try self.obtainTranslatedType(field.type),
                .alignment = field.alignment,
            };
        }

        fn translateParameter(self: *@This(), param: Parameter) !Parameter {
            const new_name = if (param.name) |n| try options.param_name_fn(self.allocator, n) else null;
            return .{
                .name = new_name,
                .type = try self.obtainTranslatedType(param.type),
            };
        }

        fn translateEnumItem(self: *@This(), item: EnumItem) !EnumItem {
            const new_name = try options.enum_name_fn(self.allocator, item.name);
            return .{
                .name = new_name,
                .value = item.value,
            };
        }

        fn translateType(self: *@This(), t: *Type) !Type {
            switch (t.*) {
                .container => |c| {
                    var new_fields: []Field = &.{};
                    for (c.fields) |field| {
                        const new_field = try self.translateField(field);
                        try self.append(&new_fields, new_field);
                    }
                    return .{
                        .container = .{
                            .kind = c.kind,
                            .fields = new_fields,
                        },
                    };
                },
                .pointer => |p| {
                    const ptr_name = try self.obtainTypeName(t);
                    const target_name = try self.obtainTypeName(p.child_type);
                    const is_null_terminated = options.ptr_is_null_terminated_fn(ptr_name, target_name);
                    const is_many = options.ptr_is_many_fn(ptr_name, target_name);
                    const is_optional = options.ptr_is_optional_fn(ptr_name, target_name);
                    return .{
                        .pointer = .{
                            .child_type = try self.obtainTranslatedType(p.child_type),
                            .alignment = p.alignment,
                            .sentinel = if (is_null_terminated) "0" else null,
                            .size = if (is_many) .many else .one,
                            .is_const = p.is_const,
                            .is_volatile = p.is_volatile,
                            .is_optional = is_optional,
                            .allows_zero = p.allows_zero,
                        },
                    };
                },
                .enumeration => |e| {
                    var new_items: []EnumItem = &.{};
                    for (e.items) |item| {
                        const new_field = try self.translateEnumItem(item);
                        try self.append(&new_items, new_field);
                    }
                    return .{
                        .enumeration = .{
                            .items = new_items,
                            .is_signed = e.is_signed,
                        },
                    };
                },
                .function => |f| {
                    var new_params: []Parameter = &.{};
                    // look for writable pointer
                    var output_types: []*Type = &.{};
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
                            break index + 1;
                        }
                    } else 0;
                    for (f.parameters, 0..) |param, index| {
                        if (index >= output_start) {
                            const output_type = try self.obtainTranslatedType(param.type);
                            try self.append(&output_types, output_type.pointer.child_type);
                        }
                    }
                    // see if the translated function should return an error union
                    const can_fail = self.isReturningError(t);
                    const status_type = try self.obtainTranslatedType(f.return_type);
                    const extra: usize = if (!can_fail or self.non_error_enums.len > 1) 1 else 0;
                    if (extra == 1) try self.append(&output_types, status_type);
                    const arg_count = f.parameters.len + extra - output_types.len;
                    for (f.parameters[0..arg_count]) |param| {
                        const new_param = try self.translateParameter(param);
                        try self.append(&new_params, new_param);
                    }
                    const payload_type = switch (output_types.len) {
                        0 => self.void_type,
                        1 => output_types[0],
                        else => try self.createType(.{ .type_tuple = output_types }),
                    };
                    const return_type = switch (can_fail) {
                        true => try self.createType(.{
                            .error_union = .{
                                .payload_type = payload_type,
                                .error_set = self.new_error_set,
                            },
                        }),
                        false => payload_type,
                    };
                    return .{
                        .function = .{
                            .parameters = new_params,
                            .return_type = return_type,
                            .alignment = f.alignment,
                        },
                    };
                },
                .expression => |e| {
                    return .{
                        .expression = try self.translateExpression(e),
                    };
                },
                else => unreachable,
            }
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

        fn isReturningError(self: *@This(), t: *Type) bool {
            const name = self.obtainTypeName(t.function.return_type) catch return false;
            return std.mem.eql(u8, options.c_error_type, name);
        }

        fn deriveErrorSet(self: *@This()) !void {
            var names: [][]const u8 = &.{};
            var non_errors: [][]const u8 = &.{};
            if (self.old_namespace.getType(options.c_error_type)) |t| {
                if (t.* == .enumeration) {
                    for (t.enumeration.items) |item| {
                        if (options.enum_is_error_fn(item.name, item.value)) {
                            const name = try options.error_name_fn(self.allocator, item.name);
                            try self.append(&names, name);
                        } else {
                            const name = try options.enum_name_fn(self.allocator, item.name);
                            try self.append(&non_errors, name);
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
            self.non_error_enums = non_errors;
        }

        fn getTranslateCall(self: *@This(), decl: Declaration, new_t: *const Type) !Expression {
            const can_fail = self.isReturningError(decl.type.?);
            const ignore_status = false;
            var non_unique_args: [][]const u8 = &.{};
            for (new_t.function.parameters, 0..) |param, index| {
                switch (param.type.*) {
                    .enumeration, .expression => {
                        if (self.new_namespace.getName(param.type)) |name| {
                            const pair = try std.fmt.allocPrint(self.allocator, ".@\"{d}\" = {s}", .{ index, name });
                            try self.append(&non_unique_args, pair);
                        }
                    },
                    else => {},
                }
            }
            if (!can_fail) {
                const return_type = new_t.function.return_type;
                switch (return_type.*) {
                    .enumeration, .expression => {
                        if (self.new_namespace.getName(return_type)) |name| {
                            const pair = try std.fmt.allocPrint(self.allocator, ".retval = {s}", .{name});
                            try self.append(&non_unique_args, pair);
                        }
                    },
                    else => {},
                }
            }
            const local_subs = if (non_unique_args.len > 0) get: {
                const local_subs_pairs = try std.mem.join(self.allocator, ", ", non_unique_args);
                break :get try std.fmt.allocPrint(self.allocator, " {s} ", .{local_subs_pairs});
            } else "";
            const code = try std.fmt.allocPrint(self.allocator, "{s}.translate({s}.{s}, {}, {}, .{{{s}}})", .{
                options.translater,
                options.c_import,
                decl.name,
                can_fail,
                ignore_status,
                local_subs,
            });
            return .{ .unknown = code };
        }

        fn obtainTranslatedType(self: *@This(), t: *Type) std.mem.Allocator.Error!*Type {
            return self.old_to_new_type_map.get(t) orelse add: {
                const new_t = try self.createType(try self.translateType(t));
                try self.old_to_new_type_map.put(t, new_t);
                break :add new_t;
            };
        }

        fn obtainTypeName(self: *@This(), t: *Type) ![]const u8 {
            self.write_to_byte_array = true;
            defer self.write_to_byte_array = false;
            self.byte_array.clearRetainingCapacity();
            self.printTypeRef(t) catch {};
            return try self.allocator.dupe(u8, self.byte_array.items);
        }

        fn printImports(self: *@This()) anyerror!void {
            try self.printTxt("const std = @import(\"std\");\n");
            try self.printTxt("const api_translator = @import(\"api-translator.zig\");\n");
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
                } else try self.printTypeDef(t);
            }
        }

        fn printTypeDef(self: *@This(), t: *Type) anyerror!void {
            switch (t.*) {
                .container => |c| {
                    if (t != self.new_root) {
                        if (!std.mem.eql(u8, c.kind, "opaque")) try self.printTxt("extern ");
                        if (c.fields.len == 0) {
                            try self.printFmt("{s} {{}}", .{c.kind});
                        } else {
                            try self.printFmt("{s} {{\n", .{c.kind});
                        }
                    }
                    for (c.fields) |field| try self.printField(field);
                    if (c.fields.len > 0 and c.decls.len > 0) try self.printTxt("\n");
                    for (c.decls) |decl| try self.printDeclaration(decl);
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
                    const is_binary = check: {
                        var pow2_count: usize = 0;
                        var not_pow2_count: usize = 0;
                        for (e.items) |item| {
                            if (item.value > 0) {
                                if (std.math.isPowerOfTwo(item.value)) {
                                    pow2_count += 1;
                                } else {
                                    not_pow2_count += 1;
                                }
                            }
                            if (item.value < 0) {
                                break :check false;
                            }
                        }
                        break :check pow2_count > 3 and pow2_count > not_pow2_count;
                    };
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
                            if (is_binary) {
                                inline for (.{ 64, 32, 16, 8 }) |bits| {
                                    if (max_width > (bits / 2) or bits == 8) {
                                        const width = std.fmt.comptimePrint("{d}", .{bits});
                                        try self.printFmt(" = 0b{b:0" ++ width ++ "}", .{@as(u128, @bitCast(item.value))});
                                        break;
                                    }
                                }
                            } else if (is_hexidecimal) {
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
            try self.printTxt(";\n\n");
        }

        fn printTrainslatorSetup(self: *@This()) !void {
            const old_enum_t = self.old_namespace.getType(options.c_error_type) orelse {
                std.debug.print("Unable to find enum type '{s}'\n", .{options.c_error_type});
                return error.Unexpected;
            };
            var enum_name: []const u8 = options.c_error_type;
            const is_bool = std.mem.eql(u8, enum_name, "bool");
            var non_error_statuses: []const u8 = if (is_bool) ".{true}" else ".{0}";
            var default_status: []const u8 = if (is_bool) "false" else "1";
            const new_enum_t = try self.obtainTranslatedType(old_enum_t);
            if (self.new_namespace.getName(new_enum_t)) |name| enum_name = name;
            if (new_enum_t.* == .enumeration) {
                var literals: [][]const u8 = &.{};
                for (self.non_error_enums) |name| {
                    const literal = try std.fmt.allocPrint(self.allocator, ".{s}", .{name});
                    try self.append(&literals, literal);
                }
                const list = try std.mem.join(self.allocator, ", ", literals);
                non_error_statuses = switch (self.non_error_enums.len) {
                    1 => try std.fmt.allocPrint(self.allocator, ".{{{s}}}", .{list}),
                    else => try std.fmt.allocPrint(self.allocator, ".{{ {s} }}", .{list}),
                };
                for (new_enum_t.enumeration.items) |item| {
                    const is_error = for (self.non_error_enums) |name| {
                        if (std.mem.eql(u8, name, item.name)) break false;
                    } else true;
                    if (is_error) {
                        default_status = try std.fmt.allocPrint(self.allocator, ".{s}", .{item.name});
                        break;
                    }
                }
            }
            try self.printFmt("const {s} = api_translator.Translator(.{{\n", .{options.translater});
            try self.printTxt(".substitutions = &.{{\n");
            for (self.old_root.container.decls) |decl| {
                if (decl.expr == .type) {
                    if (self.old_to_new_type_map.get(decl.expr.type)) |new_t| {
                        if (self.new_namespace.getName(new_t)) |new_name| {
                            try self.printFmt(".{{ .old = {s}.{s}, .new = {s} }},\n", .{
                                options.c_import,
                                decl.name,
                                new_name,
                            });
                        }
                    }
                }
            }
            try self.printTxt("}},\n");
            try self.printFmt(".error_scheme = api_translator.BasicErrorScheme({s}, {s}, .{{\n", .{ enum_name, options.error_set });
            try self.printFmt(".non_error_statuses = &{s},\n", .{non_error_statuses});
            try self.printFmt(".default_status = {s},\n", .{default_status});
            try self.printFmt(".default_error = {s}.Unexpected,\n", .{options.error_set});
            try self.printTxt("}}),\n");
            try self.printTxt("}});\n");
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
                    self.cwd_path,
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
    const c_to_zig = Translator(.{
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, .{
            .non_error_statuses = &.{.ok},
            .default_status = .unexpected,
            .default_error = error.Unexpected,
        }),
    });
    const Fn1 = c_to_zig.Translated(fn (i32, OldStruct) StatusEnum, true, false, .{});
    try expectEqual(Fn1, fn (i32, NewStruct) ErrorSet!void);
    const Fn2 = c_to_zig.Translated(fn (i32, []const OldStruct) StatusEnum, true, false, .{});
    try expectEqual(Fn2, fn (i32, []const NewStruct) ErrorSet!void);
    const Fn3 = c_to_zig.Translated(fn (i32, *OldStruct) StatusEnum, true, false, .{});
    try expectEqual(Fn3, fn (i32) ErrorSet!NewStruct);
    const Fn4 = c_to_zig.Translated(fn (i32, *bool, *OldStruct) StatusEnum, true, false, .{});
    try expectEqual(Fn4, fn (i32) ErrorSet!std.meta.Tuple(&.{ bool, NewStruct }));
    const Fn5 = c_to_zig.Translated(fn (i32, OldStruct) bool, false, false, .{});
    try expectEqual(Fn5, fn (i32, NewStruct) bool);
    const Fn6 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, false, .{});
    try expectEqual(Fn6, fn (i32, NewStruct) c_int);
    const Fn7 = c_to_zig.Translated(fn (i32, OldStruct) c_int, false, true, .{});
    try expectEqual(Fn7, fn (i32, NewStruct) void);
    const Fn8 = c_to_zig.Translated(fn (i32, *bool, ?*const OldStruct) StatusEnum, true, false, .{});
    try expectEqual(Fn8, fn (i32, *bool, ?*const NewStruct) ErrorSet!void);
}

test "translate" {
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
    const c_to_zig = Translator(.{
        .substitutions = &.{
            .{ .old = OldStruct, .new = NewStruct },
        },
        .error_scheme = BasicErrorScheme(StatusEnum, ErrorSet, .{
            .non_error_statuses = &.{.ok},
            .default_status = .unexpected,
            .default_error = error.Unexpected,
        }),
    });
    const ns = struct {
        fn hello(_: OldStruct, _: i32) c_uint {
            return 0;
        }

        fn world(_: i32, ptr: *OldStruct) c_uint {
            ptr.* = .{ .number1 = 123, .number2 = 456 };
            return 0;
        }
    };
    _ = ActionEnum;
    const func1 = c_to_zig.translate(ns.hello, true, false, .{});
    try expectEqual(@TypeOf(func1), fn (NewStruct, i32) ErrorSet!void);
    try func1(.{}, 123);
}

// test "Translator" {
//     const c = @cImport(@cInclude("./test/include/animal.h"));
//     const Self = struct {
//         pub const Struct = struct {
//             number1: i32,
//             nunmer2: i32,
//         };
//         pub const Enum = enum(c_uint) { dog, cat, fox };
//         pub const ErrorSet = error{ Sick, Dead, Crazy, Unknown };
//         pub const Status = enum {
//             ok,
//             sick,
//             dead,
//             crazy,
//             unknown,
//         };
//         pub const Cow = struct {
//             number1: i32,
//             number2: i32,
//         };
//         pub const Hen = struct {
//             number1: i32,
//         };
//         pub const Pig = struct {
//             number1: i32,
//             number2: i32,
//             number3: i32,
//         };
//     };
// }

// test "Substitute" {
//     const OldStruct = options.substitutions[0].old;
//     const NewStruct = options.substitutions[0].new;
//     const OldEnum = options.substitutions[1].old;
//     const NewEnum = options.substitutions[1].new;
//     const T1 = Substitute(OldStruct, .{}, 0, 1);
//     try expectEqual(T1, NewStruct);
//     const T2 = Substitute(OldEnum, .{ .@"0" = NewEnum }, 0, 1);
//     try expectEqual(T2, NewEnum);
//     const T3 = Substitute(OldEnum, .{ .@"-1" = NewEnum }, 0, 1);
//     try expectEqual(T3, NewEnum);
//     const T4 = Substitute([]OldStruct, .{}, 0, 1);
//     try expectEqual(T4, []NewStruct);
//     const T5 = Substitute([]const OldStruct, .{}, 0, 1);
//     try expectEqual(T5, []const NewStruct);
//     const T6 = Substitute([4]OldStruct, .{}, 0, 1);
//     try expectEqual(T6, [4]NewStruct);
//     const T7 = Substitute(?OldStruct, .{}, 0, 1);
//     try expectEqual(T7, ?NewStruct);
//     const T8 = Substitute(anyerror!OldStruct, .{}, 0, 1);
//     try expectEqual(T8, anyerror!NewStruct);
//     const T9 = Substitute(OldEnum, .{ .retval = NewEnum }, null, 1);
//     try expectEqual(T9, NewEnum);
// }

// test "WritableTarget" {
//     const Null = @TypeOf(null);
//     const T1 = WritableTarget(*usize) orelse Null;
//     try expectEqual(T1, usize);
//     const T2 = WritableTarget(*const usize) orelse Null;
//     try expectEqual(T2, Null);
//     const T3 = WritableTarget(*void) orelse Null;
//     try expectEqual(T3, Null);
//     const T4 = WritableTarget(*anyopaque) orelse Null;
//     try expectEqual(T4, Null);
//     const T5 = WritableTarget(*opaque {}) orelse Null;
//     try expectEqual(T5, Null);
//     const T6 = WritableTarget(*type) orelse Null;
//     try expectEqual(T6, Null);
// }

test "CodeGenerator" {
    const ns = struct {
        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, "animal_");
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, 7, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, 7, true);
        }

        fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            const index = if (std.mem.lastIndexOfScalar(u8, name, '_')) |i| i + 1 else 0;
            return snakify(allocator, name, index);
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, 7, true);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"./test/include"},
        .header_paths = &.{"animal.h"},
        .c_error_type = "animal_status",
        .filter_fn = ns.filter,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
}
