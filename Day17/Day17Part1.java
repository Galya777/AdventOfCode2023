package Day17;

import java.io.*;
import java.util.*;

public class Day17Part1 {
    private static final int MAX_CONSECUTIVE = 3;
    private static final int[][] DIRECTIONS = {{0, 1}, {1, 0}, {0, -1}, {-1, 0}}; // right, down, left, up

    public static void main(String[] args) throws IOException {
        List<String> grid = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader("Day17/input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    grid.add(line);
                }
            }
        }

        int result = findMinHeatLoss(grid);
        System.out.println("Minimum heat loss: " + result);
    }

    private static int findMinHeatLoss(List<String> grid) {
        if (grid.isEmpty()) {
            throw new IllegalArgumentException("Input grid is empty");
        }
        int rows = grid.size();
        int cols = grid.get(0).length();
        if (cols == 0) {
            throw new IllegalArgumentException("First row is empty");
        }

        // Priority queue: (heatLoss, row, col, dir, consecutiveSteps)
        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));
        
        // Heat loss tracking: heatLoss[row][col][dir][steps]
        int[][][][] minHeat = new int[rows][cols][4][MAX_CONSECUTIVE + 1];
        for (int[][][] arr3d : minHeat) {
            for (int[][] arr2d : arr3d) {
                for (int[] arr : arr2d) {
                    Arrays.fill(arr, Integer.MAX_VALUE);
                }
            }
        }

        // Start from top-left (0,0), can go right or down
        // We don't add heat for the starting position as per problem statement
        // Direction: 0=right, 1=down, 2=left, 3=up
        // We can start by going right or down
        pq.offer(new int[]{0, 0, 0, 0, 1}); // right, 1 step
        pq.offer(new int[]{0, 0, 0, 1, 1}); // down, 1 step
        minHeat[0][0][0][1] = 0;
        minHeat[0][0][1][1] = 0;

        while (!pq.isEmpty()) {
            int[] current = pq.poll();
            int heatLoss = current[0];
            int r = current[1];
            int c = current[2];
            int dir = current[3];
            int steps = current[4];

            // Reached the bottom-right corner
            if (r == rows - 1 && c == cols - 1) {
                return heatLoss;
            }

            // If we already found a better way, skip
            if (heatLoss > minHeat[r][c][dir][steps]) {
                continue;
            }

            // Try all possible directions
            for (int i = 0; i < 4; i++) {
                // Can't reverse direction (180-degree turn)
                if ((dir + 2) % 4 == i) {
                    continue;
                }

                int nr = r + DIRECTIONS[i][0];
                int nc = c + DIRECTIONS[i][1];

                // Check bounds
                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                    continue;
                }

                // Debug: Check if the row exists and has enough columns
                if (nr >= grid.size()) {
                    System.err.println("Error: Row " + nr + " is out of bounds (grid size: " + grid.size() + ")");
                    continue;
                }
                String row = grid.get(nr);
                if (nc >= row.length()) {
                    System.err.println("Error: Column " + nc + " is out of bounds in row " + nr + " (row length: " + row.length() + ")");
                    continue;
                }

                int newSteps = (i == dir) ? steps + 1 : 1;
                
                // Can't move more than MAX_CONSECUTIVE steps in the same direction
                if (newSteps > MAX_CONSECUTIVE) {
                    continue;
                }

                // Debug: Print the character we're trying to access
                char ch = row.charAt(nc);
                if (ch < '0' || ch > '9') {
                    System.err.println("Warning: Non-digit character '" + ch + "' at (" + nr + "," + nc + ")");
                }
                int newHeatLoss = heatLoss + (ch - '0');
                
                // If we found a better way to reach this state, update and add to queue
                if (newHeatLoss < minHeat[nr][nc][i][newSteps]) {
                    minHeat[nr][nc][i][newSteps] = newHeatLoss;
                    pq.offer(new int[]{newHeatLoss, nr, nc, i, newSteps});
                }
            }
        }

        // If we get here, no path was found (shouldn't happen with valid input)
        return -1;
    }
}
