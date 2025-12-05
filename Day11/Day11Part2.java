package Day11;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day11Part2 {
    private static final long EXPANSION_FACTOR = 999999; // 1,000,000 - 1

    public static void main(String[] args) {
        String inputFile = "Day11/input.txt";
        try {
            List<String> universe = readInput(inputFile);
            long sum = calculateSumOfShortestPaths(universe);
            System.out.println("Sum of shortest paths: " + sum);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static List<String> readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line.trim());
            }
        }
        return lines;
    }

    private static long calculateSumOfShortestPaths(List<String> universe) {
        // Find all galaxies and empty rows/columns
        List<int[]> galaxies = new ArrayList<>();
        Set<Integer> emptyRows = new HashSet<>();
        Set<Integer> emptyCols = new HashSet<>();
        
        // Initialize emptyCols with all column indices
        for (int j = 0; j < universe.get(0).length(); j++) {
            emptyCols.add(j);
        }
        
        // Find galaxies and mark non-empty columns
        for (int i = 0; i < universe.size(); i++) {
            String row = universe.get(i);
            boolean rowHasGalaxy = false;
            
            for (int j = 0; j < row.length(); j++) {
                if (row.charAt(j) == '#') {
                    galaxies.add(new int[]{i, j});
                    rowHasGalaxy = true;
                    emptyCols.remove(j);
                }
            }
            
            if (!rowHasGalaxy) {
                emptyRows.add(i);
            }
        }
        
        // Convert empty sets to arrays for faster access
        int[] emptyRowArray = emptyRows.stream().mapToInt(i -> i).toArray();
        int[] emptyColArray = emptyCols.stream().mapToInt(i -> i).toArray();
        
        // Calculate the sum of shortest paths with expansion
        long sum = 0;
        
        for (int i = 0; i < galaxies.size(); i++) {
            int[] g1 = galaxies.get(i);
            for (int j = i + 1; j < galaxies.size(); j++) {
                int[] g2 = galaxies.get(j);
                
                // Calculate Manhattan distance with expansion
                int minRow = Math.min(g1[0], g2[0]);
                int maxRow = Math.max(g1[0], g2[0]);
                int minCol = Math.min(g1[1], g2[1]);
                int maxCol = Math.max(g1[1], g2[1]);
                
                // Count empty rows and columns between the two galaxies
                long emptyRowsBetween = countInRange(emptyRowArray, minRow, maxRow);
                long emptyColsBetween = countInRange(emptyColArray, minCol, maxCol);
                
                // Calculate expanded distance
                long rowDiff = (maxRow - minRow) + emptyRowsBetween * EXPANSION_FACTOR;
                long colDiff = (maxCol - minCol) + emptyColsBetween * EXPANSION_FACTOR;
                
                sum += rowDiff + colDiff;
            }
        }
        
        return sum;
    }
    
    // Helper method to count how many values in the array are between min and max (exclusive)
    private static int countInRange(int[] arr, int min, int max) {
        int count = 0;
        for (int num : arr) {
            if (num > min && num < max) {
                count++;
            }
        }
        return count;
    }
}
