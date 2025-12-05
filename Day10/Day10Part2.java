package Day10;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day10Part2 {
    private static final Map<Character, int[][]> DIRECTIONS = Map.of(
        '|', new int[][]{{-1, 0}, {1, 0}},   // North, South
        '-', new int[][]{{0, -1}, {0, 1}},    // West, East
        'L', new int[][]{{-1, 0}, {0, 1}},    // North, East
        'J', new int[][]{{-1, 0}, {0, -1}},   // North, West
        '7', new int[][]{{1, 0}, {0, -1}},    // South, West
        'F', new int[][]{{1, 0}, {0, 1}},     // South, East
        'S', new int[][]{{-1, 0}, {1, 0}, {0, -1}, {0, 1}} // All directions for start
    );

    public static void main(String[] args) {
        String inputFile = "Day10/input.txt";
        try {
            char[][] grid = readInput(inputFile);
            int[] start = findStart(grid);
            int enclosedTiles = countEnclosedTiles(grid, start);
            System.out.println("Number of tiles enclosed by the loop: " + enclosedTiles);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private static char[][] readInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                lines.add(line.trim());
            }
        }
        
        char[][] grid = new char[lines.size()][];
        for (int i = 0; i < lines.size(); i++) {
            grid[i] = lines.get(i).toCharArray();
        }
        return grid;
    }

    private static int[] findStart(char[][] grid) {
        for (int i = 0; i < grid.length; i++) {
            for (int j = 0; j < grid[i].length; j++) {
                if (grid[i][j] == 'S') {
                    return new int[]{i, j};
                }
            }
        }
        throw new RuntimeException("No starting position 'S' found in the grid");
    }

    private static int countEnclosedTiles(char[][] grid, int[] start) {
        int rows = grid.length;
        int cols = grid[0].length;
        
        // Mark all loop tiles
        boolean[][] isLoop = new boolean[rows][cols];
        markLoop(grid, start, isLoop);
        
        int count = 0;
        
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (!isLoop[i][j] && isPointInPolygon(grid, isLoop, i, j)) {
                    count++;
                }
            }
        }
        
        return count;
    }
    
    private static void markLoop(char[][] grid, int[] start, boolean[][] isLoop) {
        int rows = grid.length;
        int cols = grid[0].length;
        int startX = start[0];
        int startY = start[1];
        
        // Determine the actual pipe type for 'S' based on its connections
        char startPipe = determineStartPipeType(grid, startX, startY);
        grid[startX][startY] = startPipe;
        
        Queue<int[]> queue = new LinkedList<>();
        queue.offer(new int[]{startX, startY});
        isLoop[startX][startY] = true;
        
        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int x = current[0];
            int y = current[1];
            char c = grid[x][y];
            
            int[][] dirs = DIRECTIONS.get(c);
            for (int[] dir : dirs) {
                int newX = x + dir[0];
                int newY = y + dir[1];
                
                if (newX >= 0 && newX < rows && newY >= 0 && newY < cols 
                        && !isLoop[newX][newY] && isConnected(grid, x, y, newX, newY)) {
                    isLoop[newX][newY] = true;
                    queue.offer(new int[]{newX, newY});
                }
            }
        }
    }
    
    private static boolean isPointInPolygon(char[][] grid, boolean[][] isLoop, int x, int y) {
        // Use ray casting algorithm to determine if point is inside the polygon
        int count = 0;
        for (int i = y + 1; i < grid[0].length; i++) {
            if (isLoop[x][i] && isVerticalBoundary(grid, x, i)) {
                count++;
            }
        }
        return count % 2 == 1;
    }
    
    private static boolean isVerticalBoundary(char[][] grid, int x, int y) {
        char c = grid[x][y];
        return c == '|' || c == 'L' || c == 'J' || c == 'S';
    }
    
    private static char determineStartPipeType(char[][] grid, int x, int y) {
        boolean[] connections = new boolean[4]; // N, S, W, E
        int rows = grid.length;
        int cols = grid[0].length;
        
        // Check north
        if (x > 0) {
            char north = grid[x-1][y];
            if (north == '|' || north == '7' || north == 'F') {
                connections[0] = true;
            }
        }
        
        // Check south
        if (x < rows - 1) {
            char south = grid[x+1][y];
            if (south == '|' || south == 'L' || south == 'J') {
                connections[1] = true;
            }
        }
        
        // Check west
        if (y > 0) {
            char west = grid[x][y-1];
            if (west == '-' || west == 'L' || west == 'F') {
                connections[2] = true;
            }
        }
        
        // Check east
        if (y < cols - 1) {
            char east = grid[x][y+1];
            if (east == '-' || east == 'J' || east == '7') {
                connections[3] = true;
            }
        }
        
        // Determine the pipe type based on connections
        if (connections[0] && connections[1]) return '|';
        if (connections[2] && connections[3]) return '-';
        if (connections[0] && connections[3]) return 'L';
        if (connections[0] && connections[2]) return 'J';
        if (connections[1] && connections[2]) return '7';
        if (connections[1] && connections[3]) return 'F';
        
        throw new RuntimeException("Cannot determine start pipe type");
    }
    
    private static boolean isConnected(char[][] grid, int x1, int y1, int x2, int y2) {
        char c1 = grid[x1][y1];
        char c2 = grid[x2][y2];
        
        // Check if (x2,y2) is adjacent to (x1,y1)
        if (Math.abs(x1 - x2) + Math.abs(y1 - y2) != 1) {
            return false;
        }
        
        // Check connection from (x1,y1) to (x2,y2)
        if (x1 < x2) { // (x2,y2) is below (x1,y1)
            return (c1 == '|' || c1 == '7' || c1 == 'F' || c1 == 'S') && 
                   (c2 == '|' || c2 == 'L' || c2 == 'J' || c2 == 'S');
        } else if (x1 > x2) { // (x2,y2) is above (x1,y1)
            return (c1 == '|' || c1 == 'L' || c1 == 'J' || c1 == 'S') && 
                   (c2 == '|' || c2 == '7' || c2 == 'F' || c2 == 'S');
        } else if (y1 < y2) { // (x2,y2) is to the right of (x1,y1)
            return (c1 == '-' || c1 == 'L' || c1 == 'F' || c1 == 'S') && 
                   (c2 == '-' || c2 == 'J' || c2 == '7' || c2 == 'S');
        } else { // (x2,y2) is to the left of (x1,y1)
            return (c1 == '-' || c1 == 'J' || c1 == '7' || c1 == 'S') && 
                   (c2 == '-' || c2 == 'L' || c2 == 'F' || c2 == 'S');
        }
    }
}
