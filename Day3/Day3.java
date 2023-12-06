package Day3;

import java.util.ArrayList;
import java.util.List;

public class Day3 {
    public static List<String> result = new ArrayList<>();
    public static void main(String[] args) {
        List<String> schema = Reader.readFromFile();
        assert schema != null;
        char[][] schema2D = convertTo2DArray(schema);
        part1(schema2D, schema.get(0).length(), schema.size());
        int sum = 0;
        for (String s : result) {
            sum += Integer.parseInt(s);
        }
        System.out.println(sum);
    }
    private static char[][] convertTo2DArray(List<String> schema) {
        char[][] schema2D = new char[schema.get(0).length()][schema.size()];
        for (int i = 0; i < schema.size(); i++) {
            for (int j = 0; j < schema.get(i).length(); j++) {
                schema2D[i][j] = schema.get(i).charAt(j);
            }
        }
        return schema2D;
    }

    private static void part1(char[][] schema2D, int columns, int rows) {
        StringBuilder number = new StringBuilder();
        boolean isValid = false;
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                if (Character.isDigit(schema2D[i][j])) {
                    number.append(schema2D[i][j]);
                    if (checkSurroundingPart1(schema2D,i,j)) {
                        isValid=true;
                    }
                } else {
                    if (isValid) {
                        result.add(number.toString());
                        isValid = false;
                    }
                    number = new StringBuilder();
                }
            }
        }
    }

    private static boolean checkSurroundingPart1(char[][] list, int x, int y) {
        for(int dx = -1; dx <= 1; dx++) {
            if ((x + dx >= 0) && (x + dx < list.length)) {
                for(int dy = -1; dy <= 1; dy++) {
                    if ((y + dy >= 0) && (y + dy < list[x + dx].length) && (!(dx == 0 && dy == 0))) {
                        if (Character.toString(list[x + dx][y + dy]).matches("[^a-zA-Z0-9.]")) {
                            return true;
                        }
                    }
                }
            }
        }
        return false;
    }
}