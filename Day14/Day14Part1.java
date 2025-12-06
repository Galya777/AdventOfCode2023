package Day14;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day14Part1 {
    public static void main(String[] args) {
        String inputFile = "Day14/input.txt";
        List<String> platform = readInput(inputFile);
        tiltNorth(platform);
        int totalLoad = calculateTotalLoad(platform);
        System.out.println("Total load on north support beams: " + totalLoad);
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

        // Validate all rows have the same length
        for (String row : platform) {
            if (row.length() != cols) {
                throw new RuntimeException("Inconsistent row lengths in platform");
            }
        }

        for (int col = 0; col < cols; col++) {
            int nextRockPos = 0; // Next position where a rock can roll to
            for (int row = 0; row < rows; row++) {
                String currentRow = platform.get(row);
                if (col >= currentRow.length()) {
                    throw new RuntimeException("Invalid column index " + col + " in row " + row);
                }
                char c = currentRow.charAt(col);
                if (c == 'O') {
                    // Move rock to next available position
                    if (row != nextRockPos) {
                        // Remove from current position
                        platform.set(row, currentRow.substring(0, col) + '.' + currentRow.substring(col + 1));
                        
                        // Add to new position
                        String targetRow = platform.get(nextRockPos);
                        platform.set(nextRockPos, targetRow.substring(0, col) + 'O' + targetRow.substring(col + 1));
                    }
                    nextRockPos++;
                } else if (c == '#') {
                    // Cube rock blocks further movement in this column
                    nextRockPos = row + 1;
                }
            }
        }
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
