const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

pub const TranslatorOptions = struct {
    substitutions: []const TypeSubstitution = &.{},
    error_type: TypeSubstitution = undefined,
    default_error: anyerror = undefined,
};
pub const TypeSubstitution = struct {
    old: type,
    new: type,
};

pub const CodeGeneratorOptions = struct {
    include_path: []const u8,
    header_paths: []const []const u8,
    translater: []const u8 = "c_to_zig",
    c_error_type: []const u8,
    c_error_none: []const u8,
    error_set: []const u8 = "Error",
    c_import: []const u8 = "c",
    c_root_struct: ?[]const u8 = null,
    target_ns: type,
    writer_type: type = std.fs.File.Writer,

    filter_fn: fn ([]const u8) bool,
    fn_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
    type_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
    const_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
    param_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
    field_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
    enum_item_name_fn: fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = returnArg,
};

pub fn Translator(comptime options: TranslatorOptions) type {
    return struct {
        const OldError = options.error_type.old;
        const NewError = options.error_type.new;

        pub fn Translated(comptime OldFn: anytype, local_subs: anytype) type {
            // look for non-const pointers, scanning backward
            const old_fn = @typeInfo(OldFn).@"fn";
            const OldRT = old_fn.return_type.?;
            const extra = if (OldRT != OldError and OldRT != void) 1 else 0;
            const OutputTypes = init: {
                // room for an extra type when the return value of the function isn't OldError or void
                var types: [old_fn.params.len + extra]type = undefined;
                if (extra == 1) {
                    types[old_fn.params.len] = Substitute(OldRT, local_subs, null, old_fn.params.len);
                }
                const start_index = inline for (0..old_fn.params.len) |j| {
                    const i = old_fn.params.len - j - 1;
                    const info = @typeInfo(old_fn.params[i].type.?);
                    if (info != .pointer or info.pointer.is_const) break i + 1;
                    const Target = switch (@typeInfo(info.pointer.child)) {
                        .optional => |op| op.child,
                        else => info.pointer.child,
                    };
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
            const NewRT = if (OldRT == OldError) NewError!Payload else Payload;
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

        test "Translated" {
            const OldStruct = options.substitutions[0].old;
            const NewStruct = options.substitutions[0].new;
            const Fn1 = Translated(fn (i32, OldStruct) OldError, .{});
            try expect(Fn1 == fn (i32, NewStruct) NewError!void);
            const Fn2 = Translated(fn (i32, []const OldStruct) OldError, .{});
            try expect(Fn2 == fn (i32, []const NewStruct) NewError!void);
            const Fn3 = Translated(fn (i32, *OldStruct) OldError, .{});
            try expect(Fn3 == fn (i32) NewError!NewStruct);
            const Fn4 = Translated(fn (i32, *bool, *OldStruct) OldError, .{});
            try expect(Fn4 == fn (i32) NewError!std.meta.Tuple(&.{ bool, NewStruct }));
            const Fn5 = Translated(fn (i32, OldStruct) bool, .{});
            try expect(Fn5 == fn (i32, NewStruct) bool);
        }

        const error_list = init: {
            const es = @typeInfo(NewError).error_set.?;
            var list: [es.len]NewError = undefined;
            for (es, 0..) |e, index| {
                list[index] = @field(NewError, e.name);
            }
            break :init list;
        };

        pub fn translate(comptime func: anytype) Translated(@TypeOf(func)) {
            const OldFn = @TypeOf(func);
            const NewFn = Translated(OldFn);
            const RT = @typeInfo(NewFn).@"fn".return_type.?;
            const Payload = @typeInfo(RT).error_union.payload;
            const ns = struct {
                inline fn call(new_args: std.meta.ArgsTuple(NewFn)) RT {
                    var old_args: std.meta.ArgsTuple(OldFn) = undefined;
                    // copy arguments
                    inline for (new_args, 0..) |new_arg, index| {
                        old_args[index] = @bitCast(new_arg);
                    }
                    var result: Payload = undefined;
                    if (Payload != void) {
                        // add pointers to result
                        const outputs = old_args.len - new_args.len;
                        if (outputs == 1) {
                            old_args[old_args.len - 1] = @ptrCast(&result);
                        } else {
                            for (new_args.len..old_args.len) |i| {
                                old_args[i] = @ptrCast(&result[i - new_args.len]);
                            }
                        }
                    } else result = {};
                    // call original function
                    const old_rv = @call(.auto, func, old_args);
                    // 0 is assumed to mean no error
                    const num = switch (@typeInfo(OldError)) {
                        .int => old_rv,
                        .@"enum" => @intFromEnum(old_rv),
                    };
                    const index: usize = @intCast(@abs(num));
                    return if (index == 0)
                        result
                    else if (index - 1 < error_list.len)
                        error_list[index - 1]
                    else
                        options.default_error;
                }
            };
            return fn_transform.spreadArgs(ns.call, .auto);
        }

        fn Substitute(comptime T: type, tuple: anytype, arg_index: ?usize, arg_count: usize) type {
            const keys = if (arg_index) |index| .{
                std.fmt.comptimePrint("{d}", .{index}),
                std.fmt.comptimePrint("{d}", .{-@as(isize, @intCast(arg_count - index))}),
            } else .{"retval"};
            return inline for (keys) |key| {
                // look for type in function-specific tuple first
                if (@hasField(@TypeOf(tuple), key)) break @field(tuple, key);
            } else inline for (options.substitutions) |sub| {
                // then look in global list
                if (T == sub.old) break sub.new;
            } else switch (@typeInfo(T)) {
                .pointer => |pt| redefine: {
                    var new_pt = pt;
                    if (pt.is_const) {
                        // look for substitution of non-const pointer
                        new_pt.is_const = false;
                        const PT = Substitute(@Type(.{ .pointer = new_pt }), .{}, 0, 0);
                        new_pt = @typeInfo(PT).pointer;
                        new_pt.is_const = true;
                    } else {
                        // look for substitution of child
                        new_pt.child = Substitute(pt.child, .{}, 0, 0);
                    }
                    break :redefine @Type(.{ .pointer = new_pt });
                },
                .array => |ar| redefine: {
                    var new_ar = ar;
                    new_ar.child = Substitute(ar.child, .{}, 0, 0);
                    break :redefine @Type(.{ .array = new_ar });
                },
                .optional => |op| redefine: {
                    var new_op = op;
                    new_op.child = Substitute(op.child, .{}, 0, 0);
                    break :redefine @Type(.{ .optional = new_op });
                },
                .error_union => |eu| redefine: {
                    var new_eu = eu;
                    new_eu.payload = Substitute(eu.payload, .{}, 0, 0);
                    break :redefine @Type(.{ .error_union = new_eu });
                },
                else => T,
            };
        }

        test "Substitute" {
            const OldStruct = options.substitutions[0].old;
            const NewStruct = options.substitutions[0].new;
            const OldEnum = options.substitutions[1].old;
            const NewEnum = options.substitutions[1].new;
            const T1 = Substitute(OldStruct, .{}, 0, 1);
            try expect(T1 == NewStruct);
            const T2 = Substitute(OldEnum, .{ .@"0" = NewEnum }, 0, 1);
            try expect(T2 == NewEnum);
            const T3 = Substitute(OldEnum, .{ .@"-1" = NewEnum }, 0, 1);
            try expect(T3 == NewEnum);
            const T4 = Substitute([]OldStruct, .{}, 0, 1);
            try expect(T4 == []NewStruct);
            const T5 = Substitute([]const OldStruct, .{}, 0, 1);
            try expect(T5 == []const NewStruct);
            const T6 = Substitute([4]OldStruct, .{}, 0, 1);
            try expect(T6 == [4]NewStruct);
            const T7 = Substitute(?OldStruct, .{}, 0, 1);
            try expect(T7 == ?NewStruct);
            const T8 = Substitute(anyerror!OldStruct, .{}, 0, 1);
            try expect(T8 == anyerror!NewStruct);
            const T9 = Substitute(OldEnum, .{ .retval = NewEnum }, null, 1);
            try expect(T9 == NewEnum);
        }
    };
}

test "Translator" {
    const c = @cImport(@cInclude("./test/include/animal.h"));
    const Self = struct {
        pub const Struct = struct {
            number1: i32,
            nunmer2: i32,
        };
        pub const Enum = enum(c_uint) { dog, cat, fox };
        pub const ErrorSet = error{ Woof, Meow, WhatDoesTheFoxSay };
    };
    _ = Translator(.{
        .substitutions = &.{
            .{ .old = c.animal_struct, .new = Self.Struct },
            .{ .old = c.animal_status, .new = Self.Enum },
        },
        .error_type = .{ .old = c.animal_status, .new = Self.ErrorSet },
        .default_error = Self.ErrorSet.Woof,
    });
}

pub fn CodeGenerator(comptime options: CodeGeneratorOptions) type {
    return struct {
        var gpa: std.heap.DebugAllocator(.{}) = .init;
        var arena: std.heap.ArenaAllocator = .init(gpa.allocator());
        const allocator = arena.allocator();
        const Ast = std.zig.Ast;

        const Expr = union(enum) {
            type: *Type,
            unknown: []const u8,
        };
        const Type = union(enum) {
            container: struct {
                kind: []const u8,
                fields: []Field = &.{},
                decls: []Decl = &.{},
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
            unknown: []const u8,
        };
        const Field = struct {
            name: []const u8,
            type: *Type,
            alignment: ?[]const u8 = null,
        };
        const Decl = struct {
            name: []const u8,
            type: ?*Type = null,
            alignment: ?[]const u8 = null,
            mutable: bool = false,
            expr: Expr = .{ .unknown = "" },
        };
        const Parameter = struct {
            type: *Type,
            name: ?[]const u8 = null,
        };
        const EnumItem = struct {
            name: []const u8,
            value: i128,
        };

        print_count: usize,
        indent_needed: bool,
        indent_level: usize,
        indented: bool,
        old_root: *Type,
        old_type_map: std.StringArrayHashMap(*Type),
        old_name_map: std.AutoArrayHashMap(*Type, []const u8),
        old_to_new_type_map: std.AutoArrayHashMap(*Type, *Type),
        new_root: *Type,
        new_name_map: std.AutoArrayHashMap(*Type, []const u8),
        writer: options.writer_type,

        pub fn init(writer: options.writer_type) !@This() {
            return .{
                .print_count = 0,
                .indent_needed = false,
                .indent_level = 0,
                .indented = false,
                .old_root = try createType(.{ .container = .{ .kind = "struct" } }),
                .old_type_map = .init(allocator),
                .old_name_map = .init(allocator),
                .old_to_new_type_map = .init(allocator),
                .new_root = try createType(.{ .container = .{ .kind = "struct" } }),
                .new_name_map = .init(allocator),
                .writer = writer,
            };
        }

        pub fn deinit(_: *@This()) void {
            _ = arena.reset(.free_all);
        }

        pub fn generateDiff(self: *@This()) !bool {
            // obtain declarations from header files
            try self.processHeaderFiles();
            // create new declarations with new names
            try self.translateDeclarations();
            // print out differences
            try self.printDeclarations();
            return self.print_count > 0;
        }

        test "generateDiff" {
            var self: @This() = try .init(std.io.getStdOut().writer());
            defer self.deinit();
            _ = try self.generateDiff();
        }

        fn processHeaderFiles(self: *@This()) !void {
            const base_path = try std.process.getCwdAlloc(allocator);
            for (options.header_paths) |path| {
                const full_path = try std.fs.path.resolve(allocator, &.{
                    base_path,
                    options.include_path,
                    path,
                });
                const output = try self.translateHeaderFile(full_path);
                const source = try allocator.dupeZ(u8, output);
                const tree = try Ast.parse(allocator, source, .zig);
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

        test "processHeaderFiles" {
            var self: @This() = try .init(std.io.getStdOut().writer());
            defer self.deinit();
            try self.processHeaderFiles();
            try expect(self.old_root.container.decls.len > 0);
        }

        fn processFnProto(self: *@This(), tree: Ast, proto: Ast.full.FnProto) !void {
            if (proto.visib_token == null or proto.name_token == null) return;
            var params: []Parameter = &.{};
            for (proto.ast.params) |param| {
                const expr = try self.processExprNode(tree, param);
                // see if there's a colon in front of the param type
                const before = tree.tokenSlice(tree.firstToken(param) - 1);
                try append(allocator, &params, .{
                    .name = switch (std.mem.eql(u8, before, ":")) {
                        true => tree.tokenSlice(tree.firstToken(param) - 2),
                        false => null,
                    },
                    .type = switch (expr) {
                        .type => |t| t,
                        .unknown => |s| try self.obtainType(s),
                    },
                });
            }
            const type_ptr = try createType(.{
                .function = .{
                    .parameters = params,
                    .return_type = try self.obtainType(nodeSlice(tree, proto.ast.return_type).?),
                    .alignment = nodeSlice(tree, proto.ast.align_expr),
                },
            });
            try self.addDecl(.{
                .name = tree.tokenSlice(proto.name_token.?),
                .type = type_ptr,
            });
        }

        fn processVarDecl(self: *@This(), tree: Ast, decl: Ast.full.VarDecl) !void {
            if (decl.visib_token == null) return;
            try self.addDecl(.{
                .name = tree.tokenSlice(decl.ast.mut_token + 1),
                .mutable = std.mem.eql(u8, "var", tree.tokenSlice(decl.ast.mut_token)),
                .type = try self.obtainTypeOrNull(nodeSlice(tree, decl.ast.type_node)),
                .alignment = nodeSlice(tree, decl.ast.align_node),
                .expr = try self.processExprNode(tree, decl.ast.init_node),
            });
        }

        fn processExprNode(self: *@This(), tree: Ast, node: Ast.Node.Index) !Expr {
            var buffer2: [2]Ast.Node.Index = undefined;
            if (tree.fullContainerDecl(&buffer2, node)) |decl| {
                var fields: []Field = &.{};
                for (decl.ast.members) |member| {
                    if (tree.fullContainerField(member)) |field| {
                        try append(allocator, &fields, .{
                            .name = tree.tokenSlice(field.ast.main_token),
                            .type = try self.obtainType(nodeSlice(tree, field.ast.type_expr).?),
                            .alignment = nodeSlice(tree, field.ast.align_expr),
                        });
                    }
                }
                const ptr = try createType(.{
                    .container = .{
                        .kind = tree.tokenSlice(decl.ast.main_token),
                        .fields = fields,
                    },
                });
                return .{ .type = ptr };
            } else if (self.detectPointerType(tree, node)) |tuple| {
                const ptr_type, const is_optional = tuple;
                const ptr = try createType(.{
                    .pointer = .{
                        .child_type = try self.obtainType(nodeSlice(tree, ptr_type.ast.child_type).?),
                        .sentinel = nodeSlice(tree, ptr_type.ast.sentinel),
                        .size = ptr_type.size,
                        .is_const = ptr_type.const_token != null,
                        .is_volatile = ptr_type.volatile_token != null,
                        .is_optional = is_optional,
                        .allows_zero = ptr_type.allowzero_token != null,
                        .alignment = nodeSlice(tree, ptr_type.ast.align_node),
                    },
                });
                return .{ .type = ptr };
            } else if (self.detectEnumType(tree, node)) |tuple| {
                const count, const is_signed = tuple;
                // remove decls containing integers and use them as the enum values
                const index: usize = self.old_root.container.decls.len - count;
                var items: []EnumItem = &.{};
                for (0..count) |_| {
                    const decl = self.old_root.container.decls[index];
                    const value = std.fmt.parseInt(i128, decl.expr.unknown, 10) catch unreachable;
                    try append(allocator, &items, .{ .name = decl.name, .value = value });
                    remove(allocator, &self.old_root.container.decls, index);
                }
                const ptr = try createType(.{
                    .enumeration = .{
                        .items = items,
                        .is_signed = is_signed,
                    },
                });
                return .{ .type = ptr };
            } else {
                return .{ .unknown = nodeSlice(tree, node).? };
            }
        }

        fn detectPointerType(_: *@This(), tree: Ast, node: Ast.Node.Index) ?std.meta.Tuple(&.{ Ast.full.PtrType, bool }) {
            if (tree.fullPtrType(node)) |ptr_type| return .{ ptr_type, false } else {
                const tag = tree.nodes.items(.tag)[node];
                if (tag == .optional_type) {
                    const data = tree.nodes.items(.data)[node];
                    if (tree.fullPtrType(data.lhs)) |ptr_type| return .{ ptr_type, true };
                }
            }
            return null;
        }

        fn detectEnumType(self: *@This(), tree: Ast, node: Ast.Node.Index) ?std.meta.Tuple(&.{ usize, bool }) {
            // C enums get translated as either c_uint or c_int
            const expr = nodeSlice(tree, node).?;
            const is_signed_int = std.mem.eql(u8, expr, "c_int");
            const is_unsigned_int = std.mem.eql(u8, expr, "c_uint");
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
                if (count > 0) return .{ count, is_signed_int };
            }
            return null;
        }

        fn addDecl(self: *@This(), decl: Decl) !void {
            var copy = decl;
            if (decl.expr == .type) {
                if (self.old_type_map.get(decl.name)) |type_ptr| {
                    // copy type info into the existing Type object
                    type_ptr.* = decl.expr.type.*;
                    copy.expr.type = type_ptr;
                } else {
                    // add the type under the decl name
                    try self.old_type_map.put(decl.name, decl.expr.type);
                    try self.old_name_map.put(decl.expr.type, decl.name);
                }
            }
            try append(allocator, &self.old_root.container.decls, copy);
        }

        fn obtainTypeOrNull(self: *@This(), name: ?[]const u8) !?*Type {
            return if (name) |n| self.obtainType(n) else null;
        }

        fn createType(info: Type) !*Type {
            const t = try allocator.create(Type);
            t.* = info;
            return t;
        }

        fn obtainType(self: *@This(), name: []const u8) !*Type {
            return self.old_type_map.get(name) orelse add: {
                const t = try createType(.{ .unknown = name });
                try self.old_type_map.put(name, t);
                try self.old_name_map.put(t, name);
                break :add t;
            };
        }

        fn nodeSlice(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
            if (node == 0) return null;
            const span = tree.nodeToSpan(node);
            return tree.source[span.start..span.end];
        }

        fn translateDeclarations(self: *@This()) !void {
            for (self.old_root.container.decls) |decl| {
                if (options.filter_fn(decl.name)) {
                    // get name in target namespace
                    const new_name = if (decl.type != null and decl.type.?.* == .function)
                        try options.fn_name_fn(allocator, decl.name)
                    else switch (decl.expr) {
                        .type => try options.type_name_fn(allocator, decl.name),
                        .unknown => try options.const_name_fn(allocator, decl.name),
                    };
                    const new_decl_t = if (decl.type) |t| try self.obtainTranslatedType(t) else null;
                    const expr: Expr = switch (decl.expr) {
                        .type => |t| translate: {
                            const new_t = try self.obtainTranslatedType(t);
                            try self.new_name_map.put(new_t, new_name);
                            break :translate .{ .type = new_t };
                        },
                        .unknown => |u| .{ .unknown = u },
                    };
                    try append(allocator, &self.new_root.container.decls, .{
                        .name = new_name,
                        .type = new_decl_t,
                        .alignment = decl.alignment,
                        .mutable = false,
                        .expr = expr,
                    });
                }
            }
        }

        fn translateField(self: *@This(), field: Field) !Field {
            const new_name = try options.field_name_fn(allocator, field.name);
            return .{
                .name = new_name,
                .type = try self.obtainTranslatedType(field.type),
                .alignment = field.alignment,
            };
        }

        fn translateParameter(self: *@This(), param: Parameter) !Parameter {
            const new_name = if (param.name) |n| try options.param_name_fn(allocator, n) else null;
            return .{
                .name = new_name,
                .type = try self.obtainTranslatedType(param.type),
            };
        }

        fn translateEnumItem(_: *@This(), item: EnumItem) !EnumItem {
            const new_name = try options.enum_item_name_fn(allocator, item.name);
            return .{
                .name = new_name,
                .value = item.value,
            };
        }

        fn obtainTranslatedType(self: *@This(), t: *Type) std.mem.Allocator.Error!*Type {
            return self.old_to_new_type_map.get(t) orelse add: {
                const new_t = try createType(try self.translateType(t));
                try self.old_to_new_type_map.put(t, new_t);
                break :add new_t;
            };
        }

        fn translateType(self: *@This(), t: *Type) !Type {
            switch (t.*) {
                .container => |c| {
                    var new_fields: []Field = &.{};
                    for (c.fields) |field| {
                        const new_field = try self.translateField(field);
                        try append(allocator, &new_fields, new_field);
                    }
                    return .{
                        .container = .{
                            .kind = c.kind,
                            .fields = new_fields,
                        },
                    };
                },
                .pointer => |p| {
                    return .{
                        .pointer = .{
                            .child_type = try self.obtainTranslatedType(p.child_type),
                            .alignment = p.alignment,
                            .sentinel = p.sentinel,
                            .size = p.size,
                            .is_const = p.is_const,
                            .is_volatile = p.is_volatile,
                            .is_optional = p.is_optional,
                            .allows_zero = p.allows_zero,
                        },
                    };
                },
                .enumeration => |e| {
                    var new_items: []EnumItem = &.{};
                    for (e.items) |item| {
                        const new_field = try self.translateEnumItem(item);
                        try append(allocator, &new_items, new_field);
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
                    // TODO adjust argument list
                    for (f.parameters, 0..) |param, index| {
                        _ = index;
                        const new_param = try self.translateParameter(param);
                        try append(allocator, &new_params, new_param);
                    }
                    // TODO change type
                    const return_type = try self.obtainTranslatedType(f.return_type);
                    return .{
                        .function = .{
                            .parameters = new_params,
                            .return_type = return_type,
                            .alignment = f.alignment,
                        },
                    };
                },
                .unknown => |u| {
                    return .{ .unknown = u };
                },
            }
        }

        const WriteError = std.fs.File.WriteError;

        fn printDeclarations(self: *@This()) WriteError!void {
            for (self.new_root.container.decls) |decl| try self.printDeclaration(decl);
        }

        fn printDeclaration(self: *@This(), decl: Decl) WriteError!void {
            const mut = if (decl.mutable) "var" else "const";
            try self.print("pub {s} {s}", .{ mut, decl.name });
            if (decl.type) |t| {
                try self.print(": ", .{});
                try self.printTypeRef(t);
            }
            try self.print(" = ", .{});
            try self.printExpr(decl.expr);
            try self.print(";\n\n", .{});
        }

        fn printExpr(self: *@This(), expr: Expr) WriteError!void {
            switch (expr) {
                .type => |t| try self.printTypeDecl(t),
                .unknown => |u| try self.print("{s}", .{u}),
            }
        }

        fn printTypeRef(self: *@This(), t: *Type) WriteError!void {
            if (self.new_name_map.get(t)) |name| {
                try self.print("{s}", .{name});
            } else try self.printTypeDecl(t);
        }

        fn printTypeDecl(self: *@This(), t: *Type) WriteError!void {
            switch (t.*) {
                .container => |c| {
                    if (!std.mem.eql(u8, c.kind, "opaque")) try self.print("extern ", .{});
                    if (c.fields.len == 0) {
                        try self.print("{s} {{}}", .{c.kind});
                    } else {
                        try self.print("{s} {{\n", .{c.kind});
                        for (c.fields) |field| {
                            try self.print("{s}: ", .{field.name});
                            try self.printTypeRef(field.type);
                            if (field.alignment) |a| try self.print(" align({s})", .{a});
                            try self.print(",\n", .{});
                        }
                        try self.print("}}", .{});
                    }
                },
                .pointer => |p| {
                    if (p.is_optional) try self.print("?", .{});
                    switch (p.size) {
                        .one => try self.print("*", .{}),
                        .many => try self.print("[*", .{}),
                        .slice => try self.print("[", .{}),
                        .c => try self.print("[*c", .{}),
                    }
                    if (p.sentinel) |s| try self.print(":{s}", .{s});
                    if (p.size != .one) try self.print("]", .{});
                    if (p.allows_zero) try self.print("allows_zero ", .{});
                    if (p.is_const) try self.print("const ", .{});
                    if (p.alignment) |a| try self.print("align({s}) ", .{a});
                    if (p.is_volatile) try self.print("volatile ", .{});
                    try self.printTypeRef(p.child_type);
                },
                .enumeration => |e| {
                    try self.print("enum({s}) {{\n", .{if (e.is_signed) "c_int" else "c_uint"});
                    const is_sequential = for (e.items, 0..) |item, index| {
                        if (index > 0 and item.value != e.items[index - 1].value + 1) break false;
                    } else true;
                    const is_binary = for (e.items) |item| {
                        if (item.value < 0) break false;
                        if (item.value > 0 and !std.math.isPowerOfTwo(item.value)) break false;
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
                        try self.print("{s}", .{item.name});
                        if (!is_sequential or (index == 0 and item.value != 0)) {
                            if (is_binary) {
                                inline for (.{ 64, 32, 16, 8 }) |bits| {
                                    if (max_width > bits or bits == 8) {
                                        const width = std.fmt.comptimePrint("{d}", .{bits});
                                        try self.print(" = 0b{b:0" ++ width ++ "}", .{item.value});
                                        break;
                                    }
                                }
                            } else if (is_hexidecimal) {
                                inline for (.{ 64, 32, 16, 8 }) |bits| {
                                    if (max_width > bits or bits == 8) {
                                        const width = std.fmt.comptimePrint("{d}", .{bits / 4});
                                        try self.print(" = 0x{x:0" ++ width ++ "}", .{item.value});
                                        break;
                                    }
                                }
                            } else {
                                try self.print(" = {d}", .{item.value});
                            }
                        }
                        try self.print(",\n", .{});
                    }
                    try self.print("}}", .{});
                },
                .function => |f| {
                    try self.print("fn (\n", .{});
                    for (f.parameters) |param| {
                        if (param.name) |n| try self.print("{s}: ", .{n});
                        try self.printTypeRef(param.type);
                        try self.print(",\n", .{});
                    }
                    try self.print(") ", .{});
                    if (f.alignment) |a| try self.print("align({s}) ", .{a});
                    try self.printTypeRef(f.return_type);
                },
                .unknown => |u| {
                    try self.print("{s}", .{u});
                },
            }
        }

        fn print(self: *@This(), comptime fmt: []const u8, args: anytype) WriteError!void {
            self.print_count += 1;
            if (std.mem.startsWith(u8, fmt, "}") or std.mem.startsWith(u8, fmt, ")")) {
                self.indent_needed = false;
                self.indent_level -= 1;
            }
            if (self.indent_level > 0 and !self.indented) {
                for (0..self.indent_level) |_| {
                    try self.writer.print("    ", .{});
                }
                self.indented = true;
            }
            try self.writer.print(fmt, args);
            if (std.mem.endsWith(u8, fmt, "{\n") or std.mem.endsWith(u8, fmt, "(\n")) {
                self.indent_needed = true;
                self.indent_level += 1;
            }
            if (std.mem.endsWith(u8, fmt, "\n")) {
                self.indented = false;
            }
        }

        fn translateHeaderFile(_: *@This(), full_path: []const u8) ![]const u8 {
            const result = try std.process.Child.run(.{
                .allocator = allocator,
                .argv = &.{ "zig", "translate-c", full_path },
                .max_output_bytes = 1024 * 1024 * 128,
            });
            if (result.stderr.len != 0) {
                std.debug.print("{s}\n", .{result.stderr});
                return error.Failure;
            }
            return result.stdout;
        }

        test "translateHeaderFile" {
            var self: @This() = try .init(std.io.getStdOut().writer());
            defer self.deinit();
            const path = options.header_paths[0];
            const base_path = try std.process.getCwdAlloc(allocator);
            const full_path = try std.fs.path.resolve(allocator, &.{
                base_path,
                options.include_path,
                path,
            });
            const code = try self.translateHeaderFile(full_path);
            const prefix = std.mem.sliceTo(options.header_paths[0], '.');
            try expect(std.mem.containsAtLeast(u8, code, 1, prefix));
        }
    };
}

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

        fn getEnumItemName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            const index = if (std.mem.lastIndexOfScalar(u8, name, '_')) |i| i + 1 else 0;
            return snakify(allocator, name, index);
        }
    };
    _ = CodeGenerator(.{
        .include_path = "./test/include",
        .header_paths = &.{"animal.h"},
        .c_error_type = "animal_status",
        .c_error_none = "animal_ok",
        .target_ns = @This(),
        .filter_fn = ns.filter,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_item_name_fn = ns.getEnumItemName,
        .writer_type = std.fs.File.Writer,
    });
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

fn append(allocator: std.mem.Allocator, slice_ptr: anytype, value: GrandchildOf(@TypeOf(slice_ptr))) !void {
    const len = slice_ptr.*.len;
    const capacity = calcCapacity(len);
    const new_len = len + 1;
    const new_capacity = calcCapacity(new_len);
    if (new_capacity != capacity) {
        const slice_before = slice_ptr.*.ptr[0..capacity];
        slice_ptr.* = try allocator.realloc(slice_before, new_capacity);
    }
    slice_ptr.*.len = new_len;
    slice_ptr.*[len] = value;
}

fn remove(allocator: std.mem.Allocator, slice_ptr: anytype, index: usize) void {
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
        slice_ptr.* = allocator.realloc(slice_before, new_capacity) catch unreachable;
    }
    slice_ptr.*.len = new_len;
}

fn returnArg(_: std.mem.Allocator, arg: []const u8) std.mem.Allocator.Error![]const u8 {
    return arg;
}
