package Day18;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Day18Part2 {
    public static void main(String[] args) {
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            
            List<Point> points = new ArrayList<>();
            long x = 0, y = 0;
            points.add(new Point(x, y));
            
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine().trim();
                if (line.isEmpty()) continue;
                
                // Extract the color code from the format like (#70c710)
                String color = line.substring(line.indexOf('#') + 1, line.indexOf(')'));
                
                // Last character is direction (0=R, 1=D, 2=L, 3=U)
                char dirCode = color.charAt(5);
                // First 5 characters are the distance in hex
                String distHex = color.substring(0, 5);
                long steps = Long.parseLong(distHex, 16);
                
                // Update position based on direction
                switch (dirCode) {
                    case '0' -> x += steps; // R
                    case '1' -> y += steps; // D
                    case '2' -> x -= steps; // L
                    case '3' -> y -= steps; // U
                }
                
                points.add(new Point(x, y));
            }
            
            // Calculate area using the shoelace formula and perimeter
            long area = 0;
            long perimeter = 0;
            
            for (int i = 0; i < points.size() - 1; i++) {
                Point p1 = points.get(i);
                Point p2 = points.get(i + 1);
                area += (p1.x * p2.y) - (p1.y * p2.x);
                perimeter += Math.abs(p2.x - p1.x) + Math.abs(p2.y - p1.y);
            }
            
            // Take absolute value and divide by 2 for the area
            area = Math.abs(area) / 2;
            
            // Use Pick's theorem to find the number of interior points
            // A = i + b/2 - 1 => i = A - b/2 + 1
            // Total points = i + b = A + b/2 + 1
            long interior = area - (perimeter / 2) + 1;
            long total = interior + perimeter;
            
            System.out.println("Cubic meters of lava: " + total);
            
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found");
            e.printStackTrace();
        }
    }
}
