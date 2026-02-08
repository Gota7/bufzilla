/// Reader API
const std = @import("std");
const common = @import("common.zig");
const path = @import("path.zig");

/// A Key-value pair inside an Object.
pub const KeyValuePair = struct {
    key: common.Value,
    value: common.Value,
};

/// Iteration limits to protect against malformed input.
pub const ReadLimits = struct {
    /// Maximum nesting depth. Set to null for unlimited.
    max_depth: ?u32 = 2048,

    /// Maximum byte array length for strings/binary blobs. Set to null for unlimited.
    max_bytes_length: ?usize = null,

    /// Maximum array element count. Set to null for unlimited. Requires max_depth to be set.
    max_array_length: ?usize = null,

    /// Maximum object key-value pair count. Set to null for unlimited. Requires max_depth to be set.
    max_object_size: ?usize = null,
};

const PeekResult = struct {
    tag: std.meta.Tag(common.Value),
    data: u3,
};

/// A single path query for readPaths.
pub const PathQuery = struct {
    path: []const u8,
    value: ?common.Value = null,
    resolved: bool = false,
    orig_index: usize = 0,
};

/// Error type for read operations.
pub const Error = std.Io.Reader.Error || error{ InvalidEnumTag, InvalidSignedMagnitude, UnexpectedContainerEnd, MaxDepthExceeded, BytesTooLong, ArrayTooLarge, ObjectTooLarge };

pub fn Reader(comptime limits: ReadLimits) type {
    // Array/object limits require depth limit for counter stack allocation
    if (limits.max_depth == null) {
        if (limits.max_array_length != null) {
            @compileError("max_array_length requires max_depth to be set (non-null)");
        }
        if (limits.max_object_size != null) {
            @compileError("max_object_size requires max_depth to be set (non-null)");
        }
    }

    const needs_counters = limits.max_array_length != null or limits.max_object_size != null;
    const counter_stack_size: usize = if (needs_counters) limits.max_depth.? else 0;

    return struct {
        const Self = @This();

        // The underlying reader.
        reader: *std.Io.Reader,

        // The current traversal depth.
        depth: u32 = 0,

        // Per-depth iteration counters.
        iteration_counts: [counter_stack_size]usize = [_]usize{0} ** counter_stack_size,

        /// Initializes the reader.
        pub fn init(reader: *std.Io.Reader) Self {
            return .{ .reader = reader };
        }

        /// Reads a single data item of given type and advances the position.
        fn readBytes(self: *Self, comptime T: type) !T {
            return switch (@typeInfo(T)) {
                .int => try self.reader.takeInt(T, .little),
                .float => |float_info| @bitCast(try self.reader.takeInt(@Type(.{ .int = .{ .signedness = .signed, .bits = float_info.bits } }), .little)),
                else => @compileError("readBytes: unsupported type"),
            };
        }

        /// Reads a single data item from the underlying byte array and advances the position.
        /// If bytes are returned, they will be invalidated by the next read.
        pub fn read(self: *Self) !common.Value {
            const tag_byte = try self.readBytes(u8);

            // Decode the tag
            const decoded_tag = common.decodeTag(tag_byte);
            const val_type = try std.meta.intToEnum(std.meta.Tag(common.Value), decoded_tag.tag);

            switch (val_type) {
                .containerEnd => {
                    if (self.depth == 0) return error.UnexpectedContainerEnd;
                    self.depth -= 1;
                    return .{ .containerEnd = self.depth };
                },
                .object, .array => {
                    // Check depth limit
                    if (limits.max_depth) |max| {
                        if (self.depth >= max) return error.MaxDepthExceeded;
                    }
                    self.depth += 1;
                    // Reset iteration counter for this new container
                    if (needs_counters) {
                        self.iteration_counts[self.depth - 1] = 0;
                    }
                    return if (val_type == .object) .{ .object = self.depth } else .{ .array = self.depth };
                },
                .smallIntPositive => {
                    return .{ .i64 = @as(i64, decoded_tag.data) };
                },
                .smallIntNegative => {
                    if (decoded_tag.data == 0) return error.InvalidSignedMagnitude;
                    return .{ .i64 = -@as(i64, decoded_tag.data) };
                },
                .smallUint => {
                    return .{ .u64 = @as(u64, decoded_tag.data) };
                },
                .varIntUnsigned => {
                    const size: usize = @as(usize, decoded_tag.data) + 1;
                    const intBytes = try self.reader.take(size);
                    return .{ .u64 = common.decodeVarInt(intBytes) };
                },
                .varIntSignedPositive => {
                    const size: usize = @as(usize, decoded_tag.data) + 1;
                    const intBytes = try self.reader.take(size);
                    const magnitude = common.decodeVarInt(intBytes);
                    if (magnitude > @as(u64, @intCast(std.math.maxInt(i64)))) return error.InvalidSignedMagnitude;
                    return .{ .i64 = @intCast(magnitude) };
                },
                .varIntSignedNegative => {
                    const size: usize = @as(usize, decoded_tag.data) + 1;
                    const intBytes = try self.reader.take(size);
                    const magnitude = common.decodeVarInt(intBytes);
                    if (magnitude == 0) return error.InvalidSignedMagnitude;
                    if (magnitude == (@as(u64, 1) << 63)) return .{ .i64 = std.math.minInt(i64) };
                    if (magnitude > @as(u64, @intCast(std.math.maxInt(i64)))) return error.InvalidSignedMagnitude;
                    return .{ .i64 = -@as(i64, @intCast(magnitude)) };
                },
                .f64 => {
                    const f = try self.readBytes(f64);
                    return .{ .f64 = f };
                },
                .f32 => {
                    const f = try self.readBytes(f32);
                    return .{ .f32 = f };
                },
                .f16 => {
                    const bits = try self.readBytes(u16);
                    return .{ .f16 = @bitCast(bits) };
                },
                .i64 => {
                    const i = try self.readBytes(i64);
                    return .{ .i64 = i };
                },
                .i32 => {
                    const i = try self.readBytes(i32);
                    return .{ .i32 = i };
                },
                .i16 => {
                    const i = try self.readBytes(i16);
                    return .{ .i16 = i };
                },
                .i8 => {
                    const i = try self.readBytes(i8);
                    return .{ .i8 = i };
                },
                .u64 => {
                    const u = try self.readBytes(u64);
                    return .{ .u64 = u };
                },
                .u32 => {
                    const u = try self.readBytes(u32);
                    return .{ .u32 = u };
                },
                .u16 => {
                    const u = try self.readBytes(u16);
                    return .{ .u16 = u };
                },
                .u8 => {
                    const u = try self.readBytes(u8);
                    return .{ .u8 = u };
                },
                .null => {
                    return .{ .null = undefined };
                },
                .bool => {
                    return .{ .bool = (decoded_tag.data != 0) };
                },
                .void => {
                    return .{ .void = undefined };
                },
                .varIntBytes => {
                    const len = try self.readBytesLength(.varIntBytes, decoded_tag.data);
                    return .{ .bytes = try self.reader.take(len) };
                },
                .smallBytes => {
                    const len: usize = decoded_tag.data;

                    if (limits.max_bytes_length) |max| {
                        if (len > max) return error.BytesTooLong;
                    }

                    return .{ .bytes = try self.reader.take(len) };
                },
                .typedArray => {
                    const hdr = try self.readTypedArrayHeader(decoded_tag.data);
                    const bytes = try self.reader.take(hdr.payload_len);
                    return .{ .typedArray = .{ .elem = hdr.elem, .count = hdr.count, .bytes = bytes } };
                },
                .bytes => {
                    const len = try self.readBytesLength(.bytes, decoded_tag.data);
                    const bytes = try self.reader.take(len);
                    return .{ .bytes = bytes };
                },
            }
        }

        /// Discards data items until the target depth is reached.
        fn discardUntilDepth(self: *Self, target_depth: u32) !void {
            while (self.depth > target_depth) {
                _ = try self.read();
            }
        }

        /// Peeks at the next tag without advancing position.
        fn peekTag(self: *Self) !PeekResult {
            const tag_byte = try self.reader.peekByte();
            const decoded = common.decodeTag(tag_byte);
            const tag = try std.meta.intToEnum(std.meta.Tag(common.Value), decoded.tag);
            return .{ .tag = tag, .data = decoded.data };
        }

        /// Reads the length from a varIntBytes or bytes tag.
        inline fn readBytesLength(self: *Self, val_type: std.meta.Tag(common.Value), tag_data: u3) !usize {
            if (val_type == .varIntBytes) {
                const size_len: usize = @as(usize, tag_data) + 1;
                const bytes = try self.reader.take(size_len);
                const len = common.decodeVarInt(bytes);

                if (limits.max_bytes_length) |max| {
                    if (len > max) return error.BytesTooLong;
                }
                return len;
            } else { // .bytes
                const len = try self.reader.takeInt(u64, .little);
                if (limits.max_bytes_length) |max| {
                    if (len > max) return error.BytesTooLong;
                }
                return len;
            }
        }

        const TypedArrayHeader = struct {
            elem: common.TypedArrayElem,
            count: usize,
            payload_len: usize,
        };

        inline fn readTypedArrayHeader(self: *Self, tag_data: u3) !TypedArrayHeader {
            const elem_byte = try self.reader.takeByte();
            const elem = try std.meta.intToEnum(common.TypedArrayElem, elem_byte);

            const count_len: usize = @as(usize, tag_data) + 1;
            const count_u64 = common.decodeVarInt(try self.reader.take(count_len));

            if (count_u64 > std.math.maxInt(usize)) return error.BytesTooLong;
            const count: usize = @intCast(count_u64);

            const payload_len = std.math.mul(usize, count, common.typedArrayElemSize(elem)) catch return error.BytesTooLong;
            if (limits.max_bytes_length) |max| {
                if (payload_len > max) return error.BytesTooLong;
            }

            return .{ .elem = elem, .count = count, .payload_len = payload_len };
        }

        const SkipEvent = enum { done, enter_container, exit_container };

        inline fn skipOneValue(self: *Self, decoded: common.Tag, typ: std.meta.Tag(common.Value)) !SkipEvent {
            switch (typ) {
                // Fixed size types
                .f64, .i64, .u64 => try self.reader.discardAll(8),
                .f32, .i32, .u32 => try self.reader.discardAll(4),
                .f16 => try self.reader.discardAll(2),
                .i16, .u16 => try self.reader.discardAll(2),
                .i8, .u8 => try self.reader.discardAll(1),
                .null, .bool, .void => {},
                .smallIntPositive, .smallIntNegative, .smallUint => {},

                // Variable length integers
                .varIntUnsigned, .varIntSignedPositive, .varIntSignedNegative => {
                    const size: usize = @as(usize, decoded.data) + 1;
                    try self.reader.discardAll(size);
                },

                // Byte arrays
                .smallBytes => {
                    const len: usize = decoded.data;
                    if (limits.max_bytes_length) |max| {
                        if (len > max) return error.BytesTooLong;
                    }
                    try self.reader.discardAll(len);
                },
                .typedArray => {
                    const hdr = try self.readTypedArrayHeader(decoded.data);
                    try self.reader.discardAll(hdr.payload_len);
                },
                .varIntBytes, .bytes => {
                    const len = try self.readBytesLength(typ, decoded.data);
                    try self.reader.discardAll(len);
                },

                .array, .object => return .enter_container,
                .containerEnd => return .exit_container,
            }

            return .done;
        }

        /// Skips a single value without materializing it.
        pub fn skipValue(self: *Self) !void {
            const tag_byte = try self.reader.takeByte();

            const decoded = common.decodeTag(tag_byte);
            const val_type = try std.meta.intToEnum(std.meta.Tag(common.Value), decoded.tag);

            switch (val_type) {
                .array, .object => {
                    if (limits.max_depth) |max| {
                        if (self.depth >= max) return error.MaxDepthExceeded;
                    }

                    var nest_depth: u32 = 1;
                    while (nest_depth > 0) {
                        const inner_tag = try self.reader.takeByte();

                        const inner_decoded = common.decodeTag(inner_tag);
                        const inner_type = try std.meta.intToEnum(std.meta.Tag(common.Value), inner_decoded.tag);

                        const ev = try skipOneValue(self, inner_decoded, inner_type);
                        switch (ev) {
                            .done => {},
                            .enter_container => {
                                if (limits.max_depth) |max| {
                                    if (self.depth + nest_depth >= max) return error.MaxDepthExceeded;
                                }
                                nest_depth += 1;
                            },
                            .exit_container => nest_depth -= 1,
                        }
                    }
                },

                .containerEnd => return error.UnexpectedContainerEnd,
                else => _ = try skipOneValue(self, decoded, val_type),
            }
        }

        /// Reads the bytes content of a varIntBytes or bytes tag and returns a slice.
        fn readBytesSlice(self: *Self) ![]const u8 {
            const tag_byte = try self.reader.takeByte();

            const decoded = common.decodeTag(tag_byte);
            const val_type = try std.meta.intToEnum(std.meta.Tag(common.Value), decoded.tag);

            if (val_type != .varIntBytes and val_type != .bytes and val_type != .smallBytes) {
                return error.InvalidEnumTag;
            }

            const len = if (val_type == .smallBytes) blk: {
                const l: usize = decoded.data;
                if (limits.max_bytes_length) |max| {
                    if (l > max) return error.BytesTooLong;
                }
                break :blk l;
            } else try self.readBytesLength(val_type, decoded.data);

            const result = try self.reader.take(len);
            return result;
        }

        /// Iterates over the key-value pairs of a given Value Object.
        pub fn iterateObject(self: *Self, obj: common.Value) !?KeyValuePair {
            std.debug.assert(obj == .object);
            try self.discardUntilDepth(obj.object);

            const key = try self.read();
            if (key == .containerEnd) return null;

            const value = try self.read();

            // Check limit using per-depth counter
            if (limits.max_object_size) |max| {
                const idx = obj.object - 1;
                self.iteration_counts[idx] += 1;
                if (self.iteration_counts[idx] > max) return error.ObjectTooLarge;
            }

            return .{ .key = key, .value = value };
        }

        /// Iterates over the values of a given Value Array.
        pub fn iterateArray(self: *Self, arr: common.Value) !?common.Value {
            std.debug.assert(arr == .array);
            try self.discardUntilDepth(arr.array);

            const value = try self.read();
            if (value == .containerEnd) return null;

            // Check limit using per-depth counter
            if (limits.max_array_length) |max| {
                const idx = arr.array - 1;
                self.iteration_counts[idx] += 1;
                if (self.iteration_counts[idx] > max) return error.ArrayTooLarge;
            }

            return value;
        }

        fn lessThanByIndex(_: void, lhs: PathQuery, rhs: PathQuery) bool {
            return lhs.orig_index < rhs.orig_index;
        }

        // /// Reads multiple paths from the buffer in a single pass.
        // /// Each query's `value` is populated with the found Value or null.
        // /// Malformed paths yield null for that query.
        // /// This will not rewind the position when done, ensure the reader is at the start of the data to read.
        // /// This can only be used if the reader is from a fixed buffer.
        // pub fn readPaths(self: *Self, queries: []PathQuery) Error!void {
        //     const saved_depth = self.depth;
        //     const saved_counts = self.iteration_counts;

        //     self.depth = 0;
        //     self.iteration_counts = [_]usize{0} ** counter_stack_size;

        //     try self.readPathsInternal(queries);

        //     self.depth = saved_depth;
        //     self.iteration_counts = saved_counts;
        // }

        fn readPathsInternal(self: *Self, queries: []PathQuery) Error!void {
            if (queries.len == 0) return;

            var remaining: usize = queries.len;
            for (queries, 0..) |*q, i| {
                q.value = null;
                q.resolved = false;
                q.orig_index = i;
                if (!path.validate(q.path)) {
                    q.resolved = true;
                    remaining -= 1;
                }
            }

            if (queries.len > 1) {
                const LessSeg = struct {
                    fn lt(_: void, a: PathQuery, b: PathQuery) bool {
                        return path.lessThanPathSegments(a.path, b.path);
                    }
                };
                std.sort.pdq(PathQuery, queries, {}, LessSeg.lt);
            }

            if (remaining == 0) {
                if (queries.len > 1) {
                    const LessIdx = struct {
                        fn lt(_: void, a: PathQuery, b: PathQuery) bool {
                            return lessThanByIndex({}, a, b);
                        }
                    };
                    std.sort.pdq(PathQuery, queries, {}, LessIdx.lt);
                }
                return;
            }

            const root_peek = try self.peekTag();

            if (root_peek.tag != .object and root_peek.tag != .array) {
                if (remaining > 0) {
                    var has_empty = false;
                    for (queries) |q| {
                        if (!q.resolved and q.path.len == 0) {
                            has_empty = true;
                            break;
                        }
                    }

                    if (has_empty) {
                        const root_val = try self.read();
                        for (queries) |*q| {
                            if (!q.resolved and q.path.len == 0) {
                                q.value = root_val;
                                q.resolved = true;
                                remaining -= 1;
                            }
                        }
                    }
                }

                if (queries.len > 1) {
                    const LessIdx = struct {
                        fn lt(_: void, a: PathQuery, b: PathQuery) bool {
                            return lessThanByIndex({}, a, b);
                        }
                    };
                    std.sort.pdq(PathQuery, queries, {}, LessIdx.lt);
                }
                return;
            }

            const root_val = try self.read();
            for (queries) |*q| {
                if (!q.resolved and q.path.len == 0) {
                    q.value = root_val;
                    q.resolved = true;
                    remaining -= 1;
                }
            }

            if (remaining > 0) {
                if (root_val == .object) {
                    try self.readPathsObject(queries, 0, &remaining);
                } else {
                    try self.readPathsArray(queries, 0, &remaining);
                }
            }

            if (queries.len > 1) {
                const LessIdx = struct {
                    fn lt(_: void, a: PathQuery, b: PathQuery) bool {
                        return lessThanByIndex({}, a, b);
                    }
                };
                std.sort.pdq(PathQuery, queries, {}, LessIdx.lt);
            }
        }

        fn readPathsObject(self: *Self, queries: []PathQuery, path_depth: usize, remaining: *usize) Error!void {
            var kv_count: usize = 0;

            while (true) {
                if (remaining.* == 0) {
                    return;
                }

                const peek = try self.peekTag();
                if (peek.tag == .containerEnd) {
                    _ = try self.read();
                    return;
                }

                if (limits.max_object_size) |max| {
                    if (kv_count >= max) return error.ObjectTooLarge;
                }

                // Keys must be bytes; if not, skip key+value and continue.
                if (peek.tag != .varIntBytes and peek.tag != .bytes and peek.tag != .smallBytes) {
                    try self.skipValue();
                    try self.skipValue();
                    kv_count += 1;
                    continue;
                }

                const key_slice = try self.readBytesSlice();
                defer kv_count += 1;

                var match_start: ?usize = null;
                var match_end: usize = 0;
                var any_leaf = false;
                var any_child = false;

                for (queries, 0..) |*q, i| {
                    if (q.resolved) continue;
                    const seg = path.segmentAtDepth(q.path, path_depth) orelse {
                        q.resolved = true;
                        remaining.* -= 1;
                        continue;
                    };
                    if (seg.is_index) {
                        q.resolved = true;
                        remaining.* -= 1;
                        continue;
                    }
                    if (std.mem.eql(u8, seg.key, key_slice)) {
                        if (match_start == null) match_start = i;
                        match_end = i + 1;
                        if (seg.rest.len == 0) {
                            any_leaf = true;
                        } else {
                            any_child = true;
                        }
                    }
                }

                if (match_start == null) {
                    try self.skipValue();
                    continue;
                }

                const matching = queries[match_start.?..match_end];

                if (any_leaf) {
                    const val = try self.read();
                    for (matching) |*q| {
                        if (q.resolved) continue;
                        const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                        if (!seg.is_index and std.mem.eql(u8, seg.key, key_slice) and seg.rest.len == 0) {
                            q.value = val;
                            q.resolved = true;
                            remaining.* -= 1;
                        }
                    }

                    if (val == .object or val == .array) {
                        if (any_child) {
                            if (val == .object) {
                                try self.readPathsObject(matching, path_depth + 1, remaining);
                            } else {
                                try self.readPathsArray(matching, path_depth + 1, remaining);
                            }
                        } else {
                            const target = if (val == .object) val.object - 1 else val.array - 1;
                            try self.discardUntilDepth(target);
                        }
                    } else if (val == .typedArray) {
                        if (any_child) {
                            self.resolveTypedArrayIndexQueries(val.typedArray, matching, path_depth + 1, remaining);
                        }
                    } else if (any_child) {
                        for (matching) |*q| {
                            if (q.resolved) continue;
                            const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                            if (!seg.is_index and std.mem.eql(u8, seg.key, key_slice) and seg.rest.len > 0) {
                                q.resolved = true;
                                remaining.* -= 1;
                            }
                        }
                    }
                } else {
                    const val_peek = try self.peekTag();
                    if (val_peek.tag == .typedArray) {
                        const val = try self.read();
                        std.debug.assert(val == .typedArray);
                        self.resolveTypedArrayIndexQueries(val.typedArray, matching, path_depth + 1, remaining);
                    } else if (val_peek.tag != .object and val_peek.tag != .array) {
                        for (matching) |*q| {
                            if (q.resolved) continue;
                            const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                            if (!seg.is_index and std.mem.eql(u8, seg.key, key_slice)) {
                                q.resolved = true;
                                remaining.* -= 1;
                            }
                        }
                        try self.skipValue();
                    } else {
                        const val = try self.read();
                        if (val == .object) {
                            try self.readPathsObject(matching, path_depth + 1, remaining);
                        } else {
                            try self.readPathsArray(matching, path_depth + 1, remaining);
                        }
                    }
                }
            }
        }

        fn readPathsArray(self: *Self, queries: []PathQuery, path_depth: usize, remaining: *usize) Error!void {
            var idx: usize = 0;

            while (true) {
                if (remaining.* == 0) {
                    return;
                }

                const peek = try self.peekTag();
                if (peek.tag == .containerEnd) {
                    _ = try self.read();
                    return;
                }

                if (limits.max_array_length) |max| {
                    if (idx >= max) return error.ArrayTooLarge;
                }

                var match_start: ?usize = null;
                var match_end: usize = 0;
                var any_leaf = false;
                var any_child = false;

                for (queries, 0..) |*q, i| {
                    if (q.resolved) continue;
                    const seg = path.segmentAtDepth(q.path, path_depth) orelse {
                        q.resolved = true;
                        remaining.* -= 1;
                        continue;
                    };
                    if (!seg.is_index) {
                        q.resolved = true;
                        remaining.* -= 1;
                        continue;
                    }
                    if (seg.index == idx) {
                        if (match_start == null) match_start = i;
                        match_end = i + 1;
                        if (seg.rest.len == 0) {
                            any_leaf = true;
                        } else {
                            any_child = true;
                        }
                    }
                }

                if (match_start == null) {
                    try self.skipValue();
                    idx += 1;
                    continue;
                }

                const matching = queries[match_start.?..match_end];

                if (any_leaf) {
                    const val = try self.read();
                    for (matching) |*q| {
                        if (q.resolved) continue;
                        const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                        if (seg.is_index and seg.index == idx and seg.rest.len == 0) {
                            q.value = val;
                            q.resolved = true;
                            remaining.* -= 1;
                        }
                    }

                    if (val == .object or val == .array) {
                        if (any_child) {
                            if (val == .object) {
                                try self.readPathsObject(matching, path_depth + 1, remaining);
                            } else {
                                try self.readPathsArray(matching, path_depth + 1, remaining);
                            }
                        } else {
                            const target = if (val == .object) val.object - 1 else val.array - 1;
                            try self.discardUntilDepth(target);
                        }
                    } else if (val == .typedArray) {
                        if (any_child) {
                            self.resolveTypedArrayIndexQueries(val.typedArray, matching, path_depth + 1, remaining);
                        }
                    } else if (any_child) {
                        for (matching) |*q| {
                            if (q.resolved) continue;
                            const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                            if (seg.is_index and seg.index == idx and seg.rest.len > 0) {
                                q.resolved = true;
                                remaining.* -= 1;
                            }
                        }
                    }
                } else {
                    const val_peek = try self.peekTag();
                    if (val_peek.tag == .typedArray) {
                        const val = try self.read();
                        std.debug.assert(val == .typedArray);
                        self.resolveTypedArrayIndexQueries(val.typedArray, matching, path_depth + 1, remaining);
                    } else if (val_peek.tag != .object and val_peek.tag != .array) {
                        for (matching) |*q| {
                            if (q.resolved) continue;
                            const seg = path.segmentAtDepth(q.path, path_depth) orelse continue;
                            if (seg.is_index and seg.index == idx) {
                                q.resolved = true;
                                remaining.* -= 1;
                            }
                        }
                        try self.skipValue();
                    } else {
                        const val = try self.read();
                        if (val == .object) {
                            try self.readPathsObject(matching, path_depth + 1, remaining);
                        } else {
                            try self.readPathsArray(matching, path_depth + 1, remaining);
                        }
                    }
                }

                idx += 1;
            }
        }

        fn resolveTypedArrayIndexQueries(self: *Self, ta: common.TypedArray, queries: []PathQuery, path_depth: usize, remaining: *usize) void {
            _ = self;
            for (queries) |*q| {
                if (q.resolved) continue;
                const seg = path.segmentAtDepth(q.path, path_depth) orelse {
                    q.resolved = true;
                    remaining.* -= 1;
                    continue;
                };
                if (!seg.is_index) {
                    q.resolved = true;
                    remaining.* -= 1;
                    continue;
                }
                if (seg.index >= ta.count) {
                    q.resolved = true;
                    remaining.* -= 1;
                    continue;
                }
                if (seg.rest.len != 0) {
                    q.resolved = true;
                    remaining.* -= 1;
                    continue;
                }

                q.value = typedArrayElementToValue(ta, seg.index);
                q.resolved = true;
                remaining.* -= 1;
            }
        }

        fn typedArrayElementToValue(ta: common.TypedArray, index: usize) common.Value {
            const elem_size = common.typedArrayElemSize(ta.elem);
            const off = index * elem_size;
            const chunk = ta.bytes[off..][0..elem_size];

            return switch (ta.elem) {
                .u8 => .{ .u8 = chunk[0] },
                .i8 => .{ .i8 = @bitCast(chunk[0]) },
                .u16 => .{ .u16 = std.mem.readInt(u16, chunk[0..2], .little) },
                .i16 => .{ .i16 = std.mem.readInt(i16, chunk[0..2], .little) },
                .u32 => .{ .u32 = std.mem.readInt(u32, chunk[0..4], .little) },
                .i32 => .{ .i32 = std.mem.readInt(i32, chunk[0..4], .little) },
                .u64 => .{ .u64 = std.mem.readInt(u64, chunk[0..8], .little) },
                .i64 => .{ .i64 = std.mem.readInt(i64, chunk[0..8], .little) },
                .f32 => .{ .f32 = @bitCast(std.mem.readInt(u32, chunk[0..4], .little)) },
                .f64 => .{ .f64 = @bitCast(std.mem.readInt(u64, chunk[0..8], .little)) },
                .f16 => .{ .f16 = @bitCast(std.mem.readInt(u16, chunk[0..2], .little)) },
            };
        }

        // /// Reads a value at a given path. Path format: "key", "key.nested", "array[0]", "obj.arr[2].name"
        // /// Returns null if the path doesn't exist or points to an incompatible type.
        // /// This can only be used if the reader is from a fixed buffer.
        // pub fn readPath(self: *Self, path_str: []const u8) Error!?common.Value {
        //     var q = [_]PathQuery{.{ .path = path_str }};
        //     try self.readPaths(q[0..]);
        //     return q[0].value;
        // }
    };
}
