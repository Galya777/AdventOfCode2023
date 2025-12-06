package Day21;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.HashSet;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.Arrays;

public class Day21Part1 {
    private static final int MAX_STEPS = 64;
    private static char[][] grid;
    private static int rows, cols;
    private static int startRow = -1, startCol = -1;

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
            rows = lines.size();
            cols = lines.get(0).length();
            System.out.println("Grid size: " + rows + "x" + cols);
            grid = new char[rows][cols];

            for (int i = 0; i < rows; i++) {
                String currentLine = lines.get(i);
                for (int j = 0; j < cols; j++) {
                    grid[i][j] = currentLine.charAt(j);
                    if (grid[i][j] == 'S') {
                        startRow = i;
                        startCol = j;
                        grid[i][j] = '.'; // Mark starting position as garden plot
                    }
                }
            }

            if (startRow == -1 || startCol == -1) {
                throw new RuntimeException("Starting position 'S' not found in the input");
            }

            int result = countReachablePlots(startRow, startCol, MAX_STEPS);
            System.out.println("Number of reachable garden plots after " + MAX_STEPS + " steps: " + result);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static int countReachablePlots(int startRow, int startCol, int maxSteps) {
        // Use a 2D array to track the minimum steps to reach each position
        int[][] stepsToReach = new int[rows][cols];
        for (int i = 0; i < rows; i++) {
            Arrays.fill(stepsToReach[i], Integer.MAX_VALUE);
        }
        
        // Directions: up, right, down, left
        int[][] directions = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
        
        // Queue for BFS: stores {row, col, steps}
        Queue<int[]> queue = new ArrayDeque<>();
        queue.offer(new int[]{startRow, startCol, 0});
        stepsToReach[startRow][startCol] = 0;

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int row = current[0];
            int col = current[1];
            int steps = current[2];
            
            // If we've already found a better path, skip
            if (steps > stepsToReach[row][col]) {
                continue;
            }
            
            // If we've reached max steps, don't explore further
            if (steps == maxSteps) {
                continue;
            }

            // Explore all four directions
            for (int[] dir : directions) {
                int newRow = row + dir[0];
                int newCol = col + dir[1];
                int newSteps = steps + 1;
                
                // Check boundaries and if it's a garden plot
                if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols 
                        && grid[newRow][newCol] == '.' 
                        && newSteps < stepsToReach[newRow][newCol]) {
                    stepsToReach[newRow][newCol] = newSteps;
                    queue.offer(new int[]{newRow, newCol, newSteps});
                }
            }
        }

        // Count all positions that can be reached in exactly maxSteps steps
        int count = 0;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (stepsToReach[i][j] != Integer.MAX_VALUE && stepsToReach[i][j] <= maxSteps 
                        && (maxSteps - stepsToReach[i][j]) % 2 == 0) {
                    count++;
                }
            }
        }
        
        return count;
    }

    static class State {
        int row, col, steps;

        State(int row, int col, int steps) {
            this.row = row;
            this.col = col;
            this.steps = steps;
        }
    }
}
