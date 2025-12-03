package Day3;

import java.util.ArrayList;
import java.util.List;

public class Part2 {

    public static void main(String[] args) {
        // Read schematic from file
        List<String> schema = Reader.readFromFile();
        if (schema == null || schema.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        // Remove empty lines
        schema.removeIf(String::isEmpty);
        if (schema.isEmpty()) {
            System.out.println("No valid lines in input file!");
            return;
        }

        // Convert to 2D char array
        char[][] schema2D = convertTo2DArray(schema);

        // Find all gears and compute gear ratios
        long totalGearRatio = computeGearRatios(schema2D);

        System.out.println("Sum of all gear ratios: " + totalGearRatio);
    }

    private static char[][] convertTo2DArray(List<String> schema) {
        int rows = schema.size();
        int cols = schema.get(0).length();
        char[][] array = new char[rows][cols];
        for (int i = 0; i < rows; i++) {
            String line = schema.get(i);
            for (int j = 0; j < cols; j++) {
                array[i][j] = line.charAt(j);
            }
        }
        return array;
    }

    private static long computeGearRatios(char[][] schema2D) {
        int rows = schema2D.length;
        int cols = schema2D[0].length;
        long sumRatios = 0;

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (schema2D[i][j] == '*') {
                    // Find all adjacent numbers
                    List<Integer> adjacentNumbers = getAdjacentNumbers(schema2D, i, j);

                    // Only gears with exactly two adjacent numbers count
                    if (adjacentNumbers.size() == 2) {
                        long ratio = (long) adjacentNumbers.get(0) * adjacentNumbers.get(1);
                        sumRatios += ratio;
                    }
                }
            }
        }

        return sumRatios;
    }

    private static List<Integer> getAdjacentNumbers(char[][] schema2D, int x, int y) {
        List<Integer> numbers = new ArrayList<>();

        int rows = schema2D.length;
        int cols = schema2D[0].length;

        // Check all 8 directions
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0) continue;

                int nx = x + dx;
                int ny = y + dy;

                if (nx >= 0 && nx < rows && ny >= 0 && ny < cols) {
                    char c = schema2D[nx][ny];
                    if (Character.isDigit(c)) {
                        // Collect the full number in that direction
                        int number = extractFullNumber(schema2D, nx, ny);
                        if (!numbers.contains(number)) { // prevent duplicates
                            numbers.add(number);
                        }
                    }
                }
            }
        }

        return numbers;
    }

    // Extract the full number that the digit belongs to (scan left and right horizontally)
    private static int extractFullNumber(char[][] schema2D, int x, int y) {
        int cols = schema2D[0].length;
        int start = y;
        int end = y;

        // Move left
        while (start > 0 && Character.isDigit(schema2D[x][start - 1])) {
            start--;
        }

        // Move right
        while (end < cols - 1 && Character.isDigit(schema2D[x][end + 1])) {
            end++;
        }

        // Build the number string
        StringBuilder sb = new StringBuilder();
        for (int i = start; i <= end; i++) {
            sb.append(schema2D[x][i]);
        }

        return Integer.parseInt(sb.toString());
    }
}
