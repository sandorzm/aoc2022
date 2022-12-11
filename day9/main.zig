const std = @import("std");

pub fn main() !void {
    const input_path = if (std.os.argv.len >= 2) std.mem.span(std.os.argv[1]) else "input.txt";
    const input_file = (try std.fs.cwd().openFile(input_path, .{}));
    defer input_file.close();
    const input = input_file.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const N_KNOTS = 10;
    if (N_KNOTS < 2)
        @compileError("Number of knots must be >= 2");

    var part1_visited = std.AutoHashMap([2]i32, void).init(allocator);
    var part2_visited = std.AutoHashMap([2]i32, void).init(allocator);
    var knots = [_][2]i32{[2]i32{0, 0}} ** N_KNOTS;
    try part1_visited.put(knots[0], {});
    try part2_visited.put(knots[0], {});
    var line_buf = [_]u8{0} ** 16;
    while (try input.readUntilDelimiterOrEof(&line_buf, '\n')) |line| {
        try moveHead(&knots[0], line);
        try doMove(&part1_visited, &part2_visited, &knots);
    }

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Different second knot positions: {d}\n" ++
                     "Different tail positions: {d}\n",
                     .{part1_visited.count(), part2_visited.count()});
}

fn moveHead(head: *[2]i32, line: []const u8) !void {
    if (line.len == 0) return;
    var words = std.mem.tokenize(u8, line, " ");
    const direction = blk: {
        const word = words.next().?;
        if (word.len > 0) break :blk word[0] else unreachable;
    };
    const magnitude = try std.fmt.parseInt(i32, words.next().?, 10);
    switch (direction) {
        'L' => head.*[0] -= magnitude,
        'R' => head.*[0] += magnitude,
        'U' => head.*[1] += magnitude,
        'D' => head.*[1] -= magnitude,
        else => unreachable,
    }
}

fn stepTail(head: *[2]i32, tail: *[2]i32) !bool {
    const dx = head.*[0] - tail.*[0];
    const dy = head.*[1] - tail.*[1];
    if (try tailWouldNotMove(dx, dy))
        return false;
    const movex = std.math.sign(dx);
    const movey = std.math.sign(dy);
    tail.*[0] += movex;
    tail.*[1] += movey;
    return true;
}

fn tailWouldNotMove(dx: i32, dy: i32) !bool {
    return (try std.math.absInt(dx)) <= 1 and (try std.math.absInt(dy)) <= 1;
}

fn doMove(
    second_visited: *std.AutoHashMap([2]i32, void),
    tail_visited:   *std.AutoHashMap([2]i32, void),
    knots: [][2]i32,
) !void {
    outer: while (true) {
        var head = &knots[0];
        for (knots[1..]) |*tail, i| {
            defer head = tail; // Cheeky loop scope-level defer
            if (!try stepTail(head, tail)) {
                if (i == 0) return; // If the second knot doesn't move, no others will
                continue :outer;    // If not the second knot, keep going
            }
            if (i == 0) try second_visited.put(tail.*, {});
        }
        try tail_visited.put(head.*, {});
    }
}
