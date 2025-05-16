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
            if (prefix.len > name.len) {
                std.debug.print("name = {s}\n", .{name});
            }
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

        fn isParamOptional(fn_name: []const u8, _: ?[]const u8, _: usize, _: []const u8) ?bool {
            return if (std.mem.eql(u8, fn_name, "iota_set_bytes")) true else null;
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"iota.c"},
        .filter_fn = ns.filter,
        .fn_name_fn = ns.getFnName,
        .param_is_slice_len_fn = ns.isParamSliceLen,
        .param_is_optional_fn = ns.isParamOptional,
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

test "CodeGenerator (lambda)" {
    const ns = struct {
        const prefix = "lambda_";

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

        fn isByValue(name: []const u8) bool {
            return std.mem.eql(u8, name, "lambda_union");
        }

        fn isParamOptional(fn_name: []const u8, _: ?[]const u8, _: usize, _: []const u8) ?bool {
            if (std.mem.eql(u8, fn_name, "lambda_accept_union")) return true;
            if (std.mem.eql(u8, fn_name, "lambda_accept_struct2")) return true;
            return null;
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"lambda.c"},
        .c_error_type = "lambda_status",
        .filter_fn = ns.filter,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
        .type_is_by_value_fn = ns.isByValue,
        .param_is_optional_fn = ns.isParamOptional,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "lambda.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (mu)" {
    const ns = struct {
        const prefix = "mu_";

        fn filter(name: []const u8) bool {
            return std.mem.startsWith(u8, name, prefix);
        }

        fn getInvalidValue(type_name: []const u8, new_type_name: []const u8) ?api_translator.InvalidValue {
            if (api_translator.ifOptionalPointer(type_name, new_type_name)) |iv| return iv;
            if (std.mem.startsWith(u8, type_name, "mu_handle")) return .{
                .err_name = "InvalidHandle",
                .err_value = "INVALID_HANDLE",
            };
            return null;
        }

        fn getFnName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, false);
        }

        fn getTypeName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, prefix.len, true);
        }

        fn isReturningError(fn_name: []const u8) bool {
            return !std.mem.endsWith(u8, fn_name, "_no_error");
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"mu.c"},
        .filter_fn = ns.filter,
        .invalid_value_fn = ns.getInvalidValue,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .error_union_is_returned_fn = ns.isReturningError,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "mu.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}

test "CodeGenerator (nu)" {
    const ns = struct {
        const prefix = "nu_";
        const enum_types = .{
            .{
                .prefix = "nu_error_",
                .type = api_translator.EnumInfo{
                    .name = "ErrorEnum",
                    .tag_type = "c_int",
                },
            },
            .{
                .prefix = "nu_option_",
                .type = api_translator.EnumInfo{
                    .name = "Options",
                    .tag_type = "u16",
                    .is_packed_struct = true,
                },
            },
        };
        const type_overrides = .{
            .options = "Options",
            .arg_options = "Options",
        };

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
            return inline for (enum_types) |t| {
                if (std.mem.startsWith(u8, name, t.prefix)) {
                    break snakify(allocator, name, t.prefix.len);
                }
            } else name;
        }

        fn getErrorName(allocator: std.mem.Allocator, name: []const u8) ![]const u8 {
            return camelize(allocator, name, 0, true);
        }

        fn isEnumItem(name: []const u8) ?api_translator.EnumInfo {
            return inline for (enum_types) |t| {
                if (std.mem.startsWith(u8, name, t.prefix)) break t.type;
            } else null;
        }

        fn overrideParam(_: []const u8, param_name: ?[]const u8, _: usize, _: []const u8) ?[]const u8 {
            return inline for (std.meta.fields(@TypeOf(type_overrides))) |field| {
                if (param_name) |name| {
                    if (std.mem.eql(u8, name, field.name)) break @field(type_overrides, field.name);
                }
            } else null;
        }

        fn overrideField(_: []const u8, field_name: []const u8, _: []const u8) ?[]const u8 {
            return inline for (std.meta.fields(@TypeOf(type_overrides))) |field| {
                if (std.mem.eql(u8, field_name, field.name)) break @field(type_overrides, field.name);
            } else null;
        }
    };
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    var generator: *CodeGenerator(.{
        .include_paths = &.{"."},
        .header_paths = &.{"nu.c"},
        .error_enum = "ErrorEnum",
        .c_error_type = "int",
        .c_root_struct = "nu",
        .filter_fn = ns.filter,
        .param_override_fn = ns.overrideParam,
        .field_override_fn = ns.overrideField,
        .type_name_fn = ns.getTypeName,
        .fn_name_fn = ns.getFnName,
        .enum_name_fn = ns.getEnumName,
        .error_name_fn = ns.getErrorName,
        .const_is_enum_item_fn = ns.isEnumItem,
    }) = try .init(gpa.allocator());
    defer generator.deinit();
    try generator.analyze();
    const path = try std.fs.path.resolve(generator.allocator, &.{
        generator.cwd,
        "nu.zig",
    });
    const file = try std.fs.createFileAbsolute(path, .{});
    try generator.print(file.writer());
}
