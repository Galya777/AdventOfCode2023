package Day14;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day14Part2 {
    private static final int CYCLES = 1000000000;

    public static void main(String[] args) {
        String inputFile = "Day14/input.txt";
        List<String> platform = readInput(inputFile);
        Map<String, Integer> seen = new HashMap<>();
        boolean cycleFound = false;

        for (int cycle = 0; cycle < CYCLES; cycle++) {
            // Perform one full cycle (north, west, south, east)
            for (int i = 0; i < 4; i++) {
                tiltNorth(platform);
                platform = rotateClockwise(platform);
            }

            // Check for cycles
            String key = String.join("", platform);
            if (!cycleFound && seen.containsKey(key)) {
                int cycleLength = cycle - seen.get(key);
                int remainingCycles = (CYCLES - 1 - cycle) % cycleLength;
                cycle = CYCLES - 1 - remainingCycles;
                cycleFound = true;
            }
            seen.put(key, cycle);
        }

        int totalLoad = calculateTotalLoad(platform);
        System.out.println("Total load after " + CYCLES + " cycles: " + totalLoad);
    }

    private static List<String> readInput(String filename) {
        List<String> platform = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    platform.add(line);
                }
            }
            if (platform.isEmpty()) {
                throw new RuntimeException("Input file is empty or contains no valid lines");
            }
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
            System.exit(1);
        }
        return platform;
    }

    private static void tiltNorth(List<String> platform) {
        int rows = platform.size();
        if (rows == 0) return;
        int cols = platform.get(0).length();

        for (int col = 0; col < cols; col++) {
            int nextRockPos = 0;
            for (int row = 0; row < rows; row++) {
                String currentRow = platform.get(row);
                char c = currentRow.charAt(col);
                if (c == 'O') {
                    if (row != nextRockPos) {
                        // Remove from current position
                        platform.set(row, currentRow.substring(0, col) + '.' + currentRow.substring(col + 1));
                        // Add to new position
                        String targetRow = platform.get(nextRockPos);
                        platform.set(nextRockPos, targetRow.substring(0, col) + 'O' + targetRow.substring(col + 1));
                    }
                    nextRockPos++;
                } else if (c == '#') {
                    nextRockPos = row + 1;
                }
            }
        }
    }

    private static List<String> rotateClockwise(List<String> platform) {
        int rows = platform.size();
        if (rows == 0) return new ArrayList<>();
        int cols = platform.get(0).length();
        List<String> rotated = new ArrayList<>();

        for (int col = 0; col < cols; col++) {
            StringBuilder sb = new StringBuilder();
            for (int row = rows - 1; row >= 0; row--) {
                sb.append(platform.get(row).charAt(col));
            }
            rotated.add(sb.toString());
        }
        return rotated;
    }

    private static int calculateTotalLoad(List<String> platform) {
        int totalLoad = 0;
        int rows = platform.size();
        for (int i = 0; i < rows; i++) {
            String row = platform.get(i);
            int rockCount = (int) row.chars().filter(c -> c == 'O').count();
            totalLoad += rockCount * (rows - i);
        }
        return totalLoad;
    }
}