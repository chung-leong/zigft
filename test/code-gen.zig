const std = @import("std");
const api_translator = @import("api-translator.zig");

const CodeGenerator = api_translator.CodeGenerator;
const camelize = api_translator.camelize;
const snakify = api_translator.snakify;

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

        fn getDocComment(allocator: std.mem.Allocator, old_name: []const u8, new_name: []const u8) !?[]const u8 {
            return try std.fmt.allocPrint(allocator, "\n{s} -> {s}\n", .{ old_name, new_name });
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"alpha.c"},
        .c_error_type = "alpha_status",
        .filter_fn = ns.filter,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
        .doc_comment_fn = ns.getDocComment,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "alpha.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (alpha, by-value)" {
    const ns = struct {
        const prefix = "alpha_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn isByValue(name: []const u8) bool {
            return std.mem.startsWith(u8, name, "alpha_struct");
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
        .include_paths = &.{"."},
        .header_paths = &.{"alpha.c"},
        .c_error_type = "alpha_status",
        .filter_fn = ns.filter,
        .type_is_by_value_fn = ns.isByValue,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "alpha-by-value.zig" });
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
        .include_paths = &.{"."},
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
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "beta.zig" });
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

        fn isStatusRequire(_: []const u8) bool {
            return true;
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"beta.c"},
        .c_error_type = "beta_status",
        .filter_fn = ns.filter,
        .enum_is_error_fn = ns.isError,
        .status_is_returned_fn = ns.isStatusRequire,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "beta-with-status.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (gamma)" {
    const ns = struct {
        const prefix = "gamma_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"gamma.c"},
        .c_error_type = "bool",
        .filter_fn = ns.filter,
        .fn_name_fn = ns.getFnName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "gamma.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (delta)" {
    const ns = struct {
        const prefix = "delta_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"delta.c"},
        .c_error_type = "int",
        .filter_fn = ns.filter,
        .fn_name_fn = ns.getFnName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "delta.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (epsilon)" {
    const ns = struct {
        const prefix = "epsilon_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"epsilon.c"},
        .filter_fn = ns.filter,
        .fn_name_fn = ns.getFnName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "epsilon.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (zeta)" {
    const ns = struct {
        const prefix = "zeta_";

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
        .include_paths = &.{"."},
        .header_paths = &.{"zeta.c"},
        .c_error_type = "zeta_status",
        .filter_fn = ns.filter,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "zeta.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (eta)" {
    const ns = struct {
        const prefix = "eta_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn isByValue(_: []const u8) bool {
            return true;
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
        .include_paths = &.{"."},
        .header_paths = &.{"eta.c"},
        .c_error_type = "eta_status",
        .filter_fn = ns.filter,
        .type_is_by_value_fn = ns.isByValue,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "eta.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (theta)" {
    const ns = struct {
        const prefix = "theta_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn isPackedStruct(name: []const u8) bool {
            return std.mem.startsWith(u8, name, "theta_flags");
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
        .include_paths = &.{"."},
        .header_paths = &.{"theta.c"},
        .c_error_type = "theta_status",
        .filter_fn = ns.filter,
        .enum_is_packed_struct_fn = ns.isPackedStruct,
        .field_name_fn = ns.getFieldName,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "theta.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (iota)" {
    const ns = struct {
        const prefix = "iota_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn isParamSliceLen(_: []const u8, _: ?[]const u8, param_index: usize, param_type: []const u8) ?usize {
            if (std.mem.eql(u8, param_type, "usize")) return param_index - 1;
            return null;
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"iota.c"},
        .filter_fn = ns.filter,
        .fn_name_fn = ns.getFnName,
        .param_is_slice_len_fn = ns.isParamSliceLen,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{ generator.cwd, "iota.zig" });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (kappa)" {
    const ns = struct {
        const prefix = "kappa_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn getEnumName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return snakify(allocator, name, prefix.len);
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn isReturningErrorUnion(fn_name: []const u8) bool {
            return !std.mem.eql(u8, fn_name, "kappa_get_last_status");
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"kappa.c"},
        .c_error_type = "kappa_status",
        .filter_fn = ns.filter,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
        .error_union_is_returned_fn = ns.isReturningErrorUnion,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "kappa.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}
