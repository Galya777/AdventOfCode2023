package Day16;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day16Part1 {
    // Directions: 0=up, 1=right, 2=down, 3=left
    private static final int[][] DIRECTIONS = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};

    public static void main(String[] args) {
        try {
            // Read the grid from input file
            List<String> grid = readInput("Day16/input.txt");
            if (grid.isEmpty()) {
                System.out.println("Input file is empty");
                return;
            }

            int rows = grid.size();
            int cols = grid.get(0).length();

            // Create a set to track energized tiles and their directions
            Set<String> energized = new HashSet<>();
            // Track visited positions with their directions to avoid cycles
            boolean[][][] visited = new boolean[rows][cols][4];

            // Start from top-left corner (0,0), moving right (direction 1)
            Queue<int[]> queue = new LinkedList<>();
            queue.add(new int[]{0, 0, 1});

            // Process the beam
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

            System.out.println("Number of energized tiles: " + energized.size());

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
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
                // Continue in same direction
                nextDirs.add(currentDir);
                break;

            case '/':
                // Reflect 90 degrees: 0<->1, 2<->3
                if (currentDir == 0) nextDirs.add(1);      // up -> right
                else if (currentDir == 1) nextDirs.add(0);  // right -> up
                else if (currentDir == 2) nextDirs.add(3);  // down -> left
                else nextDirs.add(2);                      // left -> down
                break;

            case '\\':
                // Reflect 90 degrees: 0<->3, 1<->2
                if (currentDir == 0) nextDirs.add(3);      // up -> left
                else if (currentDir == 1) nextDirs.add(2);  // right -> down
                else if (currentDir == 2) nextDirs.add(1);  // down -> right
                else nextDirs.add(0);                      // left -> up
                break;

            case '|':
                if (currentDir == 0 || currentDir == 2) {
                    // Moving up or down, continue straight
                    nextDirs.add(currentDir);
                } else {
                    // Moving left or right, split to up and down
                    nextDirs.add(0);  // up
                    nextDirs.add(2);  // down
                }
                break;

            case '-':
                if (currentDir == 1 || currentDir == 3) {
                    // Moving left or right, continue straight
                    nextDirs.add(currentDir);
                } else {
                    // Moving up or down, split to left and right
                    nextDirs.add(1);  // right
                    nextDirs.add(3);  // left
                }
                break;
        }

        return nextDirs;
    }
}