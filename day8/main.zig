const std = @import("std");

pub fn main() !void {
    const input_path = if (std.os.argv.len >= 2) std.mem.span(std.os.argv[1]) else "input.txt";
    const input_file = (try std.fs.cwd().openFile(input_path, .{}));
    defer input_file.close();
    const input = input_file.reader();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var forest = std.ArrayList([]const u8).init(allocator);
    // No need to deinit since ArenaAllocator.deinit() frees everything on exit

    while (try input.readUntilDelimiterOrEofAlloc(allocator, '\n', 1000)) |line| {
        for (line) |*char| char.* -= '0';
        try forest.append(line);
    }

    const n_visible = try Part1.countVisibleTrees(allocator, forest.items);
    const max_scenic = Part2.maxScenic(forest.items);

    const stdout = std.io.getStdOut().writer();
    try stdout.print("Number of trees visible: {d}\n" ++
                     "Max scenic score: {d}\n", .{n_visible, max_scenic});
}

// Clearly delimit code specific to parts 1 and 2
const Part1 = struct {
    const Visibility = enum { None, Left, Top };
    fn countVisibleTrees(allocator: std.mem.Allocator, forest: []const []const u8) !i32 {
        if (forest.len == 0) return 0;
        var count: i32 = 0;
        var visible = try allocator.alloc([]Visibility, forest.len);
        for (forest) |row, i| {
            visible[i] = try allocator.alloc(Visibility, row.len);
            std.mem.set(Visibility, visible[i], .None);
        }

        for (forest)    |_, i| count += addVisibleRowOrCol(false, forest, visible, i);
        for (forest[0]) |_, j| count += addVisibleRowOrCol(true, forest, visible, j);
        return count;
    }

    // 'comptime if' used on is_col will instantiate two versions of this function
    // at compile time, I think
    fn addVisibleRowOrCol(
        comptime is_col: bool,
        forest: []const []const u8,
        visible: []const []Visibility,
        row_col: usize,
    ) i32 {
        var count: i32 = 0;
        var max: i9 = -1;
        var i: usize = 0;
        const loop_bound = comptime if (is_col) forest.len else forest[0].len;
        const vis_first_side: Visibility = comptime if (is_col) .Top else .Left;
        while (i < loop_bound) : (i += 1) {
            const row = comptime if (is_col) i else row_col;
            const col = comptime if (is_col) row_col else i;
            addVisibleTree(forest[row][col], &visible[row][col], vis_first_side, &max, &count);
        }
        max = -1;
        i = 0;
        while (i < loop_bound) : (i += 1) {
            const i_start = loop_bound - 1 - i;
            const row = comptime if (is_col) i_start else row_col;
            const col = comptime if (is_col) row_col else i_start;
            // Visible from opposite side => everything past it is shorter and not
            // visible from this side
            if (visible[row][col] == vis_first_side) break;
            addVisibleTree(forest[row][col], &visible[row][col], vis_first_side, &max, &count);
        }
        return count;
    }

    fn addVisibleTree(height: u8, vis: *Visibility, target_vis: Visibility, max: *i9, count: *i32) void {
        if (max.* < height) {
            max.* = height;
            if (vis.* == .None) count.* += 1;
            vis.* = target_vis;
        }
    }
};

const Part2 = struct {
    fn maxScenic(forest: []const []const u8) i32 {
        var max: i32 = 0;
        for (forest) |row, i| {
            for (row) |_, j|
                max = std.math.max(max, scenicScore(forest, i, j));
        }
        return max;
    }

    const Cardinal = enum { Left, Right, Up, Down };
    fn scenicScore(forest: []const []const u8, i: usize, j: usize) i32 {
        var score: i32 = 1;
        // Unroll loop to make cardinal comile time-known. Once again, this
        // instantiates 4 versions of viewingDist!
        inline for ([_]Cardinal{.Left, .Right, .Up, .Down}) |dir|
            score *= viewingDist(dir, forest, i, j);
        return score;
    }

    fn viewingDist(comptime dir: Cardinal, forest: []const []const u8, ii: usize, jj: usize) i32 {
        var dist: i32 = 0;
        var i = ii; var j = jj;
        while (true) {
            switch (dir) {
                .Left  => { if (j == 0)                 break; j -= 1; },
                .Right => { if (j == forest[0].len - 1) break; j += 1; },
                .Up    => { if (i == 0)                 break; i -= 1; },
                .Down  => { if (i == forest.len - 1)    break; i += 1; },
            }
            dist += 1;
            if (forest[i][j] >= forest[ii][jj]) break;
        }
        return dist;
    }
};
