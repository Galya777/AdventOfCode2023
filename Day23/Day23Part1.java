package Day23;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

public class Day23Part1 {
    private static final int[][] DIRECTIONS = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}};
    private static char[][] grid;
    private static int rows, cols;
    private static int maxSteps = 0;
    private static boolean[][] visited;

    public static void main(String[] args) {
        try {
            System.out.println("Reading input file...");
            Path inputPath = Paths.get("Day23/input.txt");
            System.out.println("Input file path: " + inputPath.toAbsolutePath());
            
            if (!Files.exists(inputPath)) {
                System.err.println("Error: Input file not found at " + inputPath.toAbsolutePath());
                return;
            }
            
            List<String> lines = Files.readAllLines(inputPath);
            if (lines.isEmpty()) {
                System.err.println("Error: Input file is empty");
                return;
            }
            
            System.out.println("Read " + lines.size() + " lines from input file");
            
            // Filter out empty lines and trim each line
            List<String> nonEmptyLines = new ArrayList<>();
            for (String line : lines) {
                String trimmed = line.trim();
                if (!trimmed.isEmpty()) {
                    nonEmptyLines.add(trimmed);
                }
            }
            
            if (nonEmptyLines.isEmpty()) {
                System.err.println("Error: No valid grid data found in input file");
                return;
            }
            
            rows = nonEmptyLines.size();
            cols = nonEmptyLines.get(0).length();
            grid = new char[rows][cols];
            
            // Fill the grid
            for (int i = 0; i < rows; i++) {
                String line = nonEmptyLines.get(i);
                for (int j = 0; j < Math.min(cols, line.length()); j++) {
                    grid[i][j] = line.charAt(j);
                }
            }
            System.out.println("Grid size: " + rows + "x" + cols);
            
            visited = new boolean[rows][cols];

            // Find start position (the only '.' in the first row)
            int startX = -1;
            int startY = -1;
            for (int y = 0; y < cols; y++) {
                if (grid[0][y] == '.') {
                    startX = 0;
                    startY = y;
                    break;
                }
            }
            
            if (startX == -1 || startY == -1) {
                System.err.println("Error: Could not find start position in the first row");
                return;
            }
            System.out.println("Start position: (" + startX + ", " + startY + ")");

            // Find end position (the only '.' in the last row)
            int endX = -1;
            int endY = -1;
            for (int y = 0; y < cols; y++) {
                if (grid[rows - 1][y] == '.') {
                    endX = rows - 1;
                    endY = y;
                    break;
                }
            }
            
            if (endX == -1 || endY == -1) {
                System.err.println("Error: Could not find end position in the last row");
                return;
            }
            System.out.println("End position: (" + endX + ", " + endY + ")");

            System.out.println("Starting DFS...");
            visited[startX][startY] = true;
            dfs(startX, startY, endX, endY, 0);
            System.out.println("Longest hike: " + maxSteps + " steps");

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        } catch (Exception e) {
            System.err.println("Unexpected error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static void dfs(int x, int y, int endX, int endY, int steps) {
        if (x == endX && y == endY) {
            if (steps > maxSteps) {
                maxSteps = steps;
            }
            return;
        }

        // Check if current cell is a slope and force the direction
        if (grid[x][y] == '>') {
            int newY = y + 1;
            if (isValid(x, newY) && !visited[x][newY]) {
                visited[x][newY] = true;
                dfs(x, newY, endX, endY, steps + 1);
                visited[x][newY] = false;
            }
            return;
        } else if (grid[x][y] == 'v') {
            int newX = x + 1;
            if (isValid(newX, y) && !visited[newX][y]) {
                visited[newX][y] = true;
                dfs(newX, y, endX, endY, steps + 1);
                visited[newX][y] = false;
            }
            return;
        } else if (grid[x][y] == '<') {
            int newY = y - 1;
            if (isValid(x, newY) && !visited[x][newY]) {
                visited[x][newY] = true;
                dfs(x, newY, endX, endY, steps + 1);
                visited[x][newY] = false;
            }
            return;
        } else if (grid[x][y] == '^') {
            int newX = x - 1;
            if (isValid(newX, y) && !visited[newX][y]) {
                visited[newX][y] = true;
                dfs(newX, y, endX, endY, steps + 1);
                visited[newX][y] = false;
            }
            return;
        }

        // For normal path, try all four directions
        for (int[] dir : DIRECTIONS) {
            int newX = x + dir[0];
            int newY = y + dir[1];

            if (isValid(newX, newY) && !visited[newX][newY]) {
                visited[newX][newY] = true;
                dfs(newX, newY, endX, endY, steps + 1);
                visited[newX][newY] = false;
            }
        }
    }

    private static boolean isValid(int x, int y) {
        return x >= 0 && x < rows && y >= 0 && y < cols && grid[x][y] != '#' && !visited[x][y];
    }
}
