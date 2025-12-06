package Day21;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Queue;

public class Day21Part2 {
    private static final long TARGET_STEPS = 26501365;
    private static char[][] grid;
    private static int size;
    private static int startRow, startCol;

    public static void main(String[] args) {
        try {
            // Read input
            String inputFile = "Day21/input.txt";
            System.out.println("Reading input from: " + new java.io.File(inputFile).getAbsolutePath());

            java.util.List<String> lines = new java.util.ArrayList<>();
            try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    line = line.trim();
                    if (!line.isEmpty()) {
                        lines.add(line);
                    }
                }
            }

            if (lines.isEmpty()) {
                throw new RuntimeException("Input file is empty or contains no valid lines");
            }

            // Initialize grid
            size = lines.size();
            System.out.println("Grid size: " + size + "x" + size);
            grid = new char[size][size];

            for (int i = 0; i < size; i++) {
                String currentLine = lines.get(i);
                for (int j = 0; j < size; j++) {
                    grid[i][j] = currentLine.charAt(j);
                    if (grid[i][j] == 'S') {
                        startRow = i;
                        startCol = j;
                        grid[i][j] = '.';
                    }
                }
            }

            // The grid is a square of size 131x131
            // The starting point is at the center (65, 65)
            System.out.println("Starting position: (" + startRow + ", " + startCol + ")");

            // We'll use the fact that the number of reachable plots follows a quadratic pattern
            // We'll compute the number of reachable plots for 65, 65+131, and 65+2*131 steps
            // to find the quadratic coefficients

            long[] steps = {65, 196, 327};  // 65, 65+131, 65+2*131
            long[] values = new long[3];

            for (int i = 0; i < 3; i++) {
                values[i] = countReachablePlotsInfiniteGrid(startRow, startCol, (int)steps[i]);
                System.out.println("Reachable in " + steps[i] + " steps: " + values[i]);
            }

            // Solve for quadratic coefficients: y = ax² + bx + c
            // Using the three points (0, y0), (1, y1), (2, y2)
            long c = values[0];
            long a = (values[2] - 2 * values[1] + c) / 2;
            long b = values[1] - c - a;

            // The number of full grids is (26501365 - 65) / 131 = 202300
            long n = (TARGET_STEPS - 65) / 131;

            // Calculate the result using the quadratic formula
            long result = a * n * n + b * n + c;
            System.out.println("Number of reachable garden plots after " + TARGET_STEPS + " steps: " + result);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static long countReachablePlotsInfiniteGrid(int startRow, int startCol, int maxSteps) {
        // Use a map to track visited positions and their step counts
        java.util.Map<String, Integer> visited = new java.util.HashMap<>();
        // Directions: up, right, down, left
        int[][] directions = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

        // Queue for BFS: stores {row, col, steps}
        Queue<int[]> queue = new ArrayDeque<>();
        queue.offer(new int[]{startRow, startCol, 0});
        visited.put(startRow + "," + startCol, 0);

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = current[0];
            int col = current[1];
            int steps = current[2];

            // If we've reached max steps, don't explore further
            if (steps == maxSteps) {
                continue;
            }

            // Explore all four directions
            for (int[] dir : directions) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];
                int newSteps = steps + 1;

                // Get the actual position in the grid (handles negative coordinates)
                int actualRow = ((newRow % size) + size) % size;
                int actualCol = ((newCol % size) + size) % size;

                // Check if it's a garden plot
                if (grid[actualRow][actualCol] == '.') {
                    String key = newRow + "," + newCol;
                    if (!visited.containsKey(key) || newSteps < visited.get(key)) {
                        visited.put(key, newSteps);
                        queue.offer(new int[]{newRow, newCol, newSteps});
                    }
                }
            }
        }

        // Count all positions that can be reached in at most maxSteps steps with the same parity
        long count = 0;
        for (int steps : visited.values()) {
            if (steps <= maxSteps && (maxSteps - steps) % 2 == 0) {
                count++;
            }
        }

        return count;
    }
}