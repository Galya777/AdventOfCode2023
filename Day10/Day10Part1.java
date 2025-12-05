package Day10;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day10Part1 {
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
            int maxDistance = findFarthestPoint(grid, start);
            System.out.println("Farthest point from start: " + maxDistance);
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

    private static int findFarthestPoint(char[][] grid, int[] start) {
        int rows = grid.length;
        int cols = grid[0].length;
        int startX = start[0];
        int startY = start[1];
        
        System.out.println("Starting at position: (" + startX + ", " + startY + ")");
        
        // Determine the actual pipe type for 'S' based on its connections
        char startPipe = determineStartPipeType(grid, startX, startY);
        System.out.println("Determined start pipe type: " + startPipe);
        grid[startX][startY] = startPipe; // Replace 'S' with actual pipe type
        
        // Create a distance grid initialized with -1 (unvisited)
        int[][] distance = new int[rows][cols];
        for (int[] row : distance) {
            Arrays.fill(row, -1);
        }
        
        // Mark start position as distance 0
        distance[startX][startY] = 0;
        
        // BFS queue
        Queue<int[]> queue = new LinkedList<>();
        queue.offer(new int[]{startX, startY});
        
        int maxDistance = 0;
        int[][] dirs = DIRECTIONS.get(startPipe);
        System.out.println("Starting directions: " + Arrays.deepToString(dirs));
        
        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int x = current[0];
            int y = current[1];
            char c = grid[x][y];
            
            // Get possible directions for current pipe
            dirs = DIRECTIONS.get(c);
            
            for (int[] dir : dirs) {
                int newX = x + dir[0];
                int newY = y + dir[1];
                
                // Check if new position is within bounds and is a pipe
                if (newX >= 0 && newX < rows && newY >= 0 && newY < cols 
                        && grid[newX][newY] != '.' && distance[newX][newY] == -1) {
                    
                    // Check if the new position connects back to current position
                    if (isConnected(grid, newX, newY, x, y)) {
distance[newX][newY] = distance[x][y] + 1;
                        maxDistance = Math.max(maxDistance, distance[newX][newY]);
                        queue.offer(new int[]{newX, newY});
                    }
                }
            }
        }
        
        // Print the distance grid for debugging
        System.out.println("\nDistance grid:");
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                System.out.print(String.format("%3s", distance[i][j] == -1 ? "." : distance[i][j]));
            }
            System.out.println();
        }
        
        // The farthest point is the maximum distance found in the BFS
        System.out.println("Max distance: " + maxDistance + ", Farthest point: " + maxDistance);
        return maxDistance;
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