package Day24;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

class Hailstone {
    long px, py, pz;
    long vx, vy, vz;
    
    public Hailstone(long px, long py, long pz, long vx, long vy, long vz) {
        this.px = px;
        this.py = py;
        this.pz = pz;
        this.vx = vx;
        this.vy = vy;
        this.vz = vz;
    }
}

public class Day24Part1 {
    private static final long MIN_POS = 200000000000000L;
    private static final long MAX_POS = 400000000000000L;
    
    public static void main(String[] args) {
        String inputFile = "Day24/input.txt";
        List<Hailstone> hailstones = parseInput(inputFile);
        int count = countIntersections(hailstones);
        System.out.println("Number of intersections within test area: " + count);
    }
    
    private static List<Hailstone> parseInput(String filename) {
        List<Hailstone> hailstones = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                
                String[] parts = line.split("@");
                String[] pos = parts[0].trim().split(",\s*");
                String[] vel = parts[1].trim().split(",\s*");
                
                long px = Long.parseLong(pos[0].trim());
                long py = Long.parseLong(pos[1].trim());
                long pz = Long.parseLong(pos[2].trim());
                long vx = Long.parseLong(vel[0].trim());
                long vy = Long.parseLong(vel[1].trim());
                long vz = Long.parseLong(vel[2].trim());
                
                hailstones.add(new Hailstone(px, py, pz, vx, vy, vz));
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return hailstones;
    }
    
    private static int countIntersections(List<Hailstone> hailstones) {
        int count = 0;
        int n = hailstones.size();
        
        for (int i = 0; i < n; i++) {
            Hailstone h1 = hailstones.get(i);
            for (int j = i + 1; j < n; j++) {
                Hailstone h2 = hailstones.get(j);
                if (willPathsIntersect(h1, h2)) {
                    count++;
                }
            }
        }
        return count;
    }
    
    private static boolean willPathsIntersect(Hailstone h1, Hailstone h2) {
        // Calculate the determinant to check if lines are parallel
        long det = h1.vx * h2.vy - h1.vy * h2.vx;
        
        if (det == 0) {
            // Lines are parallel, check if they are the same line
            // For simplicity, we'll consider them as not intersecting in the test area
            return false;
        }
        
        // Calculate the time of intersection for both hailstones
        double t1 = ((h2.px - h1.px) * h2.vy - (h2.py - h1.py) * h2.vx) / (double) det;
        double t2 = ((h2.px - h1.px) * h1.vy - (h2.py - h1.py) * h1.vx) / (double) det;
        
        // Check if intersection is in the future for both hailstones
        if (t1 < 0 || t2 < 0) {
            return false;
        }
        
        // Calculate intersection point
        double x = h1.px + h1.vx * t1;
        double y = h1.py + h1.vy * t1;
        
        // Check if intersection is within test area
        return x >= MIN_POS && x <= MAX_POS && y >= MIN_POS && y <= MAX_POS;
    }
}
