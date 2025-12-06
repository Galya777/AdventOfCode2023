package Day16;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day16Part2 {
    // Directions: 0=up, 1=right, 2=down, 3=left
    private static final int[][] DIRECTIONS = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
    private static List<String> grid;
    private static int rows, cols;

    public static void main(String[] args) {
        try {
            // Read the grid from input file
            grid = readInput("Day16/input.txt");
            if (grid.isEmpty()) {
                System.out.println("Input file is empty");
                return;
            }

            rows = grid.size();
            cols = grid.get(0).length();
            int maxEnergized = 0;

            // Test all possible starting positions on the edges
            // Top and bottom edges (moving down and up respectively)
            for (int col = 0; col < cols; col++) {
                // Top edge, moving down
                maxEnergized = Math.max(maxEnergized, countEnergized(0, col, 2));
                // Bottom edge, moving up
                maxEnergized = Math.max(maxEnergized, countEnergized(rows - 1, col, 0));
            }

            // Left and right edges (moving right and left respectively)
            for (int row = 0; row < rows; row++) {
                // Left edge, moving right
                maxEnergized = Math.max(maxEnergized, countEnergized(row, 0, 1));
                // Right edge, moving left
                maxEnergized = Math.max(maxEnergized, countEnergized(row, cols - 1, 3));
            }

            System.out.println("Maximum number of energized tiles: " + maxEnergized);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int countEnergized(int startRow, int startCol, int startDir) {
        // Track visited positions with their directions to avoid cycles
        boolean[][][] visited = new boolean[rows][cols][4];
        Set<String> energized = new HashSet<>();
        Queue<int[]> queue = new LinkedList<>();

        // Add the starting position and direction
        queue.add(new int[]{startRow, startCol, startDir});

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = current[0];
            int col = current[1];
            int dir = current[2];

            // Check if out of bounds
            if (row < 0 || row >= rows || col < 0 || col >= cols) {
                continue;
            }

            // Check if already visited this position with same direction
            if (visited[row][col][dir]) {
                continue;
            }

            // Mark as visited and energized
            visited[row][col][dir] = true;
            energized.add(row + "," + col);

            // Get next positions based on current tile and direction
            char tile = grid.get(row).charAt(col);
            List<Integer> nextDirs = getNextDirections(tile, dir);

            // Add next positions to the queue
            for (int nextDir : nextDirs) {
                int newRow = row + DIRECTIONS[nextDir][0];
                int newCol = col + DIRECTIONS[nextDir][1];
                queue.add(new int[]{newRow, newCol, nextDir});
            }
        }

        return energized.size();
    }

    private static List<String> readInput(String filePath) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    lines.add(line.trim());
                }
            }
        }
        return lines;
    }

    private static List<Integer> getNextDirections(char tile, int currentDir) {
        List<Integer> nextDirs = new ArrayList<>();

        switch (tile) {
            case '.':
                nextDirs.add(currentDir);
                break;
            case '/':
                if (currentDir == 0) nextDirs.add(1);
                else if (currentDir == 1) nextDirs.add(0);
                else if (currentDir == 2) nextDirs.add(3);
                else nextDirs.add(2);
                break;
            case '\\':
                if (currentDir == 0) nextDirs.add(3);
                else if (currentDir == 1) nextDirs.add(2);
                else if (currentDir == 2) nextDirs.add(1);
                else nextDirs.add(0);
                break;
            case '|':
                if (currentDir == 0 || currentDir == 2) {
                    nextDirs.add(currentDir);
                } else {
                    nextDirs.add(0);
                    nextDirs.add(2);
                }
                break;
            case '-':
                if (currentDir == 1 || currentDir == 3) {
                    nextDirs.add(currentDir);
                } else {
                    nextDirs.add(1);
                    nextDirs.add(3);
                }
                break;
        }

        return nextDirs;
    }
}