package Day3;

import java.util.ArrayList;
import java.util.List;

public class Day3 {
    public static List<String> result = new ArrayList<>();

    public static void main(String[] args) {
        // Read schematic from file
        List<String> schema = Reader.readFromFile();
        if (schema == null || schema.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        // Remove any empty lines
        schema.removeIf(String::isEmpty);
        if (schema.isEmpty()) {
            System.out.println("No valid lines in input file!");
            return;
        }

        // Convert to 2D char array
        char[][] schema2D = convertTo2DArray(schema);

        // Process the schematic to find part numbers
        part1(schema2D);

        // Sum all numbers found
        int sum = 0;
        for (String s : result) {
            sum += Integer.parseInt(s);
        }

        System.out.println("Sum of all part numbers: " + sum);
    }

    private static char[][] convertTo2DArray(List<String> schema) {
        int rows = schema.size();
        int columns = schema.get(0).length();
        char[][] schema2D = new char[rows][columns];

        for (int i = 0; i < rows; i++) {
            String line = schema.get(i);
            for (int j = 0; j < columns; j++) {
                schema2D[i][j] = line.charAt(j);
            }
        }

        return schema2D;
    }

    private static void part1(char[][] schema2D) {
        int rows = schema2D.length;
        int columns = schema2D[0].length;

        for (int i = 0; i < rows; i++) {
            StringBuilder number = new StringBuilder();
            boolean isValid = false;

            for (int j = 0; j < columns; j++) {
                char c = schema2D[i][j];

                if (Character.isDigit(c)) {
                    number.append(c);
                    if (checkSurroundingPart1(schema2D, i, j)) {
                        isValid = true;
                    }
                } else {
                    if (isValid && number.length() > 0) {
                        result.add(number.toString());
                    }
                    number.setLength(0);
                    isValid = false;
                }
            }

            // Add number at end of line if valid
            if (isValid && number.length() > 0) {
                result.add(number.toString());
            }
        }
    }

    private static boolean checkSurroundingPart1(char[][] list, int x, int y) {
        int rows = list.length;
        int cols = list[0].length;

        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                if (dx == 0 && dy == 0) continue;

                int nx = x + dx;
                int ny = y + dy;

                if (nx >= 0 && nx < rows && ny >= 0 && ny < cols) {
                    char c = list[nx][ny];
                    if (!Character.isLetterOrDigit(c) && c != '.') {
                        return true;
                    }
                }
            }
        }

        return false;
    }
}
