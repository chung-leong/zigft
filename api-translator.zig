const std = @import("std");
const fn_transform = @import("./fn-transform.zig");

const expect = std.testing.expect;
const expectEqualSlices = std.testing.expectEqualSlices;

pub const TranslatorOptions = struct {
    substitutions: []const TypeSubstitution = &.{},
    error_type: TypeSubstitution = undefined,
    default_error: anyerror = undefined,
    c_import_ns: type,
    target_ns: type,
};
pub const TypeSubstitution = struct {
    old: @TypeOf(.enum_literal),
    new: @TypeOf(.enum_literal),
};
pub const CodeGeneratorOptions = struct {
    include_path: []const u8,
    header_paths: []const []const u8,
    translater_name: []const u8 = "c_to_zig",
    writer_type: type = std.fs.File.Writer,
    decl_filter_fn: fn ([]const u8) bool,
    fn_name_fn: ?fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = null,
    const_name_fn: ?fn (std.mem.Allocator, []const u8) std.mem.Allocator.Error![]const u8 = null,
    param_name_fn: ?fn (std.mem.Allocator, []const u8, []const u8) std.mem.Allocator.Error![]const u8 = null,
    field_name_fn: ?fn (std.mem.Allocator, []const u8, []const u8) std.mem.Allocator.Error![]const u8 = null,
    enum_item_name_fn: ?fn (std.mem.Allocator, []const u8, []const u8) std.mem.Allocator.Error![]const u8 = null,
};

pub fn Translator(comptime options: TranslatorOptions) type {
    return struct {
        const OldError = @field(options.c_import_ns, @tagName(options.error_type.old));
        const NewError = @field(options.target_ns, @tagName(options.error_type.new));

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
            if (!@hasDecl(options.c_import_ns, "animal_struct")) return;
            const OldStruct = options.c_import_ns.animal_struct;
            const NewStruct = options.target_ns.Struct;
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
            const es = @typeInfo(options.error_type.new).error_set.?;
            var list: [es.len]options.error_type.new = undefined;
            for (es, 0..) |e, index| {
                list[index] = @field(options.error_type.new, e.name);
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
                    if (@hasField(options, "old_error_type_to_index")) {
                        // custom error index lookup
                        const convert = @field(options, "old_error_type_to_index");
                        return if (convert(old_rv)) |index| error_list[index] else result;
                    } else {
                        // default handling, where 0 is assumed to mean no error
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
                if (@hasField(@TypeOf(tuple), key)) {
                    const literal = @field(tuple, key);
                    break @field(options.target_ns, @tagName(literal));
                }
            } else inline for (options.substitutions) |sub| {
                // then look in global list
                const Old = @field(options.c_import_ns, @tagName(sub.old));
                const New = @field(options.target_ns, @tagName(sub.new));
                if (T == Old) break New;
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
            if (!@hasDecl(options.c_import_ns, "animal_struct")) return;
            const NewStruct = options.target_ns.Struct;
            const OldStruct = options.c_import_ns.animal_struct;
            const NewEnum = options.target_ns.Enum;
            const OldEnum = options.c_import_ns.animal_enum;
            const T1 = Substitute(OldStruct, .{}, 0, 1);
            try expect(T1 == NewStruct);
            const T2 = Substitute(OldEnum, .{ .@"0" = .Enum }, 0, 1);
            try expect(T2 == NewEnum);
            const T3 = Substitute(OldEnum, .{ .@"-1" = .Enum }, 0, 1);
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
            const T9 = Substitute(OldEnum, .{ .retval = .Enum }, null, 1);
            try expect(T9 == NewEnum);
        }
    };
}

pub fn CodeGenerator(comptime options: CodeGeneratorOptions) type {
    return struct {
        print_count: usize,
        root: *Type,
        type_map: std.StringArrayHashMap(*Type),
        writer: options.writer_type,

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
                is_volatile: bool,
                is_optional: bool,
                allows_zero: bool,
            },
            enumeration: struct {
                items: []EnumItem = &.{},
            },
            function: struct {
                parameters: []Parameter = &.{},
                return_value: []const u8,
                alignment: ?[]const u8,
            },
            unknown: []const u8,
        };
        const Parameter = struct {
            type: *Type,
            name: ?[]const u8 = null,
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
        const EnumItem = struct {
            name: []const u8,
            value: i128,
        };

        pub fn init(writer: options.writer_type) !@This() {
            var self: @This() = .{
                .print_count = 0,
                .root = undefined,
                .type_map = .init(allocator),
                .writer = writer,
            };
            self.root = try self.createType(.{ .container = .{ .kind = "struct" } });
            return self;
        }

        pub fn deinit(_: *@This()) void {
            _ = arena.reset(.free_all);
        }

        pub fn generateDiff(self: *@This()) !bool {
            // obtain declarations from header files
            try self.processHeaderFiles();
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
            // var self: @This() = try .init(std.io.getStdOut().writer());
            // defer self.deinit();
            // try self.processHeaderFiles();
            // try expect(self.root.container.decls.len > 0);
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
            const type_ptr = try self.createType(.{
                .function = .{
                    .parameters = params,
                    .return_value = nodeSlice(tree, proto.ast.return_type).?,
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
                const ptr = try self.createType(.{
                    .container = .{
                        .kind = tree.tokenSlice(decl.ast.main_token),
                        .fields = fields,
                    },
                });
                return .{ .type = ptr };
            } else if (self.detectPointerType(tree, node)) |tuple| {
                const ptr_type, const is_optional = tuple;
                const ptr = try self.createType(.{
                    .pointer = .{
                        .child_type = try self.obtainType(nodeSlice(tree, ptr_type.ast.child_type).?),
                        .sentinel = nodeSlice(tree, ptr_type.ast.sentinel),
                        .size = ptr_type.size,
                        .is_volatile = ptr_type.volatile_token != null,
                        .is_optional = is_optional,
                        .allows_zero = ptr_type.allowzero_token != null,
                        .alignment = nodeSlice(tree, ptr_type.ast.align_node),
                    },
                });
                return .{ .type = ptr };
            } else if (self.detectEnumType(tree, node)) |count| {
                // remove decls containing integers and use them as the enum values
                const index: usize = self.root.container.decls.len - count;
                var items: []EnumItem = &.{};
                for (0..count) |_| {
                    const decl = self.root.container.decls[index];
                    try append(allocator, &items, .{
                        .name = decl.name,
                        .value = std.fmt.parseInt(i128, decl.expr.unknown, 10) catch unreachable,
                    });
                    remove(allocator, &self.root.container.decls, index);
                }
                const ptr = try self.createType(.{
                    .enumeration = .{ .items = items },
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

        fn detectEnumType(self: *@This(), tree: Ast, node: Ast.Node.Index) ?usize {
            // C enums get translated as either c_uint or c_int
            const expr = nodeSlice(tree, node).?;
            if (std.mem.eql(u8, expr, "c_uint") or std.mem.eql(u8, expr, "c_int")) {
                // enum items are declared ahead of the type;
                // scan backward looking for int values
                const decls = self.root.container.decls;
                var count: usize = 0;
                while (count + 1 < decls.len) : (count += 1) {
                    const decl = decls[decls.len - count - 1];
                    if (!decl.mutable and decl.expr == .unknown) {
                        _ = std.fmt.parseInt(i128, decl.expr.unknown, 10) catch break;
                    } else break;
                }
                if (count > 0) return count;
            }
            return null;
        }

        fn addDecl(self: *@This(), decl: Decl) !void {
            var copy = decl;
            if (decl.expr == .type) {
                if (self.type_map.get(decl.name)) |type_ptr| {
                    // copy type info into the existing Type object
                    type_ptr.* = decl.expr.type.*;
                    copy.expr.type = type_ptr;
                } else {
                    // add the type under the decl name
                    try self.type_map.put(decl.name, decl.expr.type);
                }
            }
            try append(allocator, &self.root.container.decls, copy);
        }

        fn obtainTypeOrNull(self: *@This(), name: ?[]const u8) !?*Type {
            return if (name) |n| self.obtainType(n) else null;
        }

        fn createType(_: *@This(), info: Type) !*Type {
            const ptr = try allocator.create(Type);
            ptr.* = info;
            return ptr;
        }

        fn obtainType(self: *@This(), name: []const u8) !*Type {
            return self.type_map.get(name) orelse add: {
                const ptr = try allocator.create(Type);
                ptr.* = .{ .unknown = "" };
                try self.type_map.put(name, ptr);
                break :add ptr;
            };
        }

        fn nodeSlice(tree: Ast, node: Ast.Node.Index) ?[]const u8 {
            if (node == 0) return null;
            const span = tree.nodeToSpan(node);
            return tree.source[span.start..span.end];
        }

        fn printDeclarations(self: *@This()) !void {
            for (self.root.container.decls) |decl| {
                if (options.decl_filter_fn(decl.name)) {
                    std.debug.print("{s}\n", .{decl.name});
                }
            }
        }

        // fn printFunction(self: *@This(), decl: Declaration) !void {
        //     const arg_list = self.getNewArgList(decl);
        //     const name = self.name_map.get(decl.name) orelse return;
        //     if (getCurrentArgList(name)) |_| return;
        //     try self.print("pub const {s}: fn (", .{name});
        // }

        fn changeName(name: []const u8, kind: @TypeOf(.enum_literal), context: anytype) ![]const u8 {
            return if (@field(options, @tagName(kind) ++ "_name_fn")) |func| run: {
                var args: std.meta.ArgsTuple(@TypeOf(func)) = undefined;
                args[0] = allocator;
                args[1] = name;
                for (context, 0..) |arg, index| {
                    args[2 + index] = arg;
                }
                break :run @call(.auto, func, args);
            } else name;
        }

        // fn getCurrentArgList(name: []const u8) ?[]const []const u8 {
        //     return inline for (std.meta.declarations(options.target_ns)) |decl| {
        //         if (std.mem.eql(u8, name, decl.name)) {
        //             const func = @field(options.target_ns, decl.name);
        //             const info = @typeInfo(func);
        //             if (info != .@"fn") break null;
        //             return comptime get: {
        //                 const params = info.@"fn".params;
        //                 var list: [params.len][]const u8 = undefined;
        //                 for (params, 0..) |param, index| {
        //                     list[index] = getTypeName(param.type);
        //                 }
        //                 break :get &list;
        //             };
        //         }
        //     } else null;
        // }

        // fn getTypeName(comptime T: type) []const u8 {
        //     const name = @typeName(T);
        //     return if (std.mem.lastIndexOfScalar(u8, name, '.')) |index| {
        //         return name[index + 1 ..];
        //     } else name;
        // }

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

        fn print(self: *@This(), comptime fmt: []const u8, args: anytype) !void {
            self.print_count += 1;
            try self.writer.print(fmt, args);
        }
    };
}

test "CodeGenerator" {
    const ns = struct {
        fn filterDecl(name: []const u8) bool {
            return std.mem.startsWith(u8, name, "animal_");
        }

        fn nameDecl(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, 7, false);
        }
    };
    _ = CodeGenerator(.{
        .include_path = "./test/include",
        .header_paths = &.{"animal.h"},
        .writer_type = std.fs.File.Writer,
        .decl_filter_fn = ns.filterDecl,
    });
}

test "Translator" {
    const c_import_ns = @cImport(@cInclude("./test/include/animal.h"));
    const target_ns = struct {
        pub const Struct = struct {
            number1: i32,
            nunmer2: i32,
        };
        pub const Enum = enum(c_uint) { dog, cat, fox };
        pub const ErrorSet = error{ Woof, Meow, WhatDoesTheFoxSay };
    };
    _ = Translator(.{
        .substitutions = &.{
            .{ .old = .animal_struct, .new = .Struct },
        },
        .c_import_ns = c_import_ns,
        .target_ns = target_ns,
        .error_type = .{ .old = .animal_status, .new = .ErrorSet },
        .default_error = target_ns.ErrorSet.Woof,
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
