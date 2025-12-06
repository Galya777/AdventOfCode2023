package Day17;

import java.io.*;
import java.util.*;

public class Day17Part2 {
    private static final int MIN_STEPS = 4;
    private static final int MAX_STEPS = 10;
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
        int[][][][] minHeat = new int[rows][cols][4][MAX_STEPS + 1];
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

            // Reached the bottom-right corner and have moved at least MIN_STEPS in this direction
            if (r == rows - 1 && c == cols - 1 && steps >= MIN_STEPS) {
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

                // If changing direction, must have moved at least MIN_STEPS in current direction
                if (i != dir && steps < MIN_STEPS) {
                    continue;
                }

                // Can't move more than MAX_STEPS in the same direction
                int newSteps = (i == dir) ? steps + 1 : 1;
                if (newSteps > MAX_STEPS) {
                    continue;
                }

                int nr = r + DIRECTIONS[i][0];
                int nc = c + DIRECTIONS[i][1];

                // Check bounds
                if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                    continue;
                }

                int newHeatLoss = heatLoss + (grid.get(nr).charAt(nc) - '0');

                // If we found a better way to reach this state, update and add to queue
                if (newHeatLoss < minHeat[nr][nc][i][newSteps]) {
                    minHeat[nr][nc][i][newSteps] = newHeatLoss;
                    pq.offer(new int[]{newHeatLoss, nr, nc, i, newSteps});
                }
            }
        }

        // If we get here, no valid path was found
        return -1;
    }
}