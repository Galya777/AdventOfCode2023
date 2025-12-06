package Day24;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day24Part2 {
    private static class Hailstone {
        final long px, py, pz;
        final long vx, vy, vz;

        public Hailstone(long px, long py, long pz, long vx, long vy, long vz) {
            this.px = px;
            this.py = py;
            this.pz = pz;
            this.vx = vx;
            this.vy = vy;
            this.vz = vz;
        }
    }

    public static void main(String[] args) {
        String inputFile = "Day24/input.txt";
        List<Hailstone> hailstones = parseInput(inputFile);
        
        // We'll use the first few hailstones to set up our equations
        // We need at least 3 hailstones to get enough equations
        Hailstone h1 = hailstones.get(0);
        Hailstone h2 = hailstones.get(1);
        Hailstone h3 = hailstones.get(2);
        
        // We'll use Gaussian elimination to solve the system of equations
        // The solution will give us the rock's initial position and velocity
        long[] solution = solve(h1, h2, h3);
        
        if (solution != null) {
            long px = solution[0];
            long py = solution[1];
            long pz = solution[2];
            System.out.println("Initial position: (" + px + ", " + py + ", " + pz + ")");
            System.out.println("Sum of coordinates: " + (px + py + pz));
        } else {
            System.out.println("No solution found.");
        }
    }

    private static long[] solve(Hailstone h1, Hailstone h2, Hailstone h3) {
        // We'll use the first three hailstones to set up a system of equations
        // The equations are derived from the condition that the rock's position
        // must match each hailstone's position at some time t_i
        
        // For each hailstone i, we have:
        // px + vx * t_i = h_i.px + h_i.vx * t_i
        // py + vy * t_i = h_i.py + h_i.vy * t_i
        // pz + vz * t_i = h_i.pz + h_i.vz * t_i
        
        // We can rearrange these to eliminate t_i and get equations in terms of px, py, pz, vx, vy, vz
        // We'll need at least 3 hailstones to get enough equations
        
        // Let's use the first three hailstones to set up our equations
        // We'll use Gaussian elimination to solve the system
        
        // The solution will give us the rock's initial position and velocity
        // For simplicity, we'll use a symbolic math library or implement the solver
        
        // For now, we'll use a simplified approach that works for the example input
        // and should work for the actual input given the constraints
        
        // We'll make an educated guess about the velocity range
        int maxVelocity = 500; // Adjust this based on the input
        
        for (int vx = -maxVelocity; vx <= maxVelocity; vx++) {
            for (int vy = -maxVelocity; vy <= maxVelocity; vy++) {
                // Check if this velocity is possible for the first two hailstones
                long[] solution = checkVelocity(h1, h2, vx, vy);
                if (solution != null) {
                    // Check if this solution works for the third hailstone
                    if (checkSolution(h3, solution[0], solution[1], solution[2], vx, vy, (int)solution[3])) {
                        return new long[]{solution[0], solution[1], solution[2]};
                    }
                }
            }
        }
        
        return null;
    }
    
    private static long[] checkVelocity(Hailstone h1, Hailstone h2, int vx, int vy) {
        // Check if two hailstones can be hit by a rock with the given x and y velocities
        // Return the position and time if possible, null otherwise
        
        // Equations:
        // (px - h1.px) / (h1.vx - vx) = (py - h1.py) / (h1.vy - vy) = t1
        // (px - h2.px) / (h2.vx - vx) = (py - h2.py) / (h2.vy - vy) = t2
        
        long denominator = (h1.vx - vx) * (h2.vy - vy) - (h1.vy - vy) * (h2.vx - vx);
        if (denominator == 0) {
            return null; // Parallel paths, no solution
        }
        
        long numerator = (h2.px - h1.px) * (h2.vy - vy) - (h2.py - h1.py) * (h2.vx - vx);
        if (numerator % denominator != 0) {
            return null; // Non-integer solution
        }
        
        long t1 = numerator / denominator;
        if (t1 < 0) {
            return null; // Collision in the past
        }
        
        long px = h1.px + (h1.vx - vx) * t1;
        long py = h1.py + (h1.vy - vy) * t1;
        
        // Now check the z-coordinate
        // pz + vz * t1 = h1.pz + h1.vz * t1
        // We need to find vz and pz that satisfy this for both hailstones
        
        // For the second hailstone
        long t2 = (px - h2.px) / (h2.vx - vx);
        if (t2 < 0) {
            return null; // Collision in the past
        }
        
        // Calculate vz from the first hailstone
        // pz = h1.pz + (h1.vz - vz) * t1
        // We need to find vz such that this equation holds for both hailstones
        
        // For now, we'll assume vz is in a reasonable range and check
        int maxVZ = 500; // Adjust based on input
        for (int vz = -maxVZ; vz <= maxVZ; vz++) {
            long pz = h1.pz + (h1.vz - vz) * t1;
            
            // Check if this pz and vz work for the second hailstone
            if (pz + vz * t2 == h2.pz + h2.vz * t2) {
                return new long[]{px, py, pz, t1};
            }
        }
        
        return null;
    }
    
    private static boolean checkSolution(Hailstone h, long px, long py, long pz, int vx, int vy, int vz) {
        // Check if the rock at (px, py, pz) with velocity (vx, vy, vz) will hit the hailstone h
        
        // Calculate time t when the rock and hailstone have the same x and y positions
        if (h.vx == vx) {
            // Special case: same x velocity
            if (px != h.px) {
                return false; // Will never have the same x position
            }
            // Check y position
            if (h.vy == vy) {
                return py == h.py; // Same y velocity, must start at same position
            }
            long t = (py - h.py) / (h.vy - vy);
            if (t < 0) {
                return false; // Collision in the past
            }
            // Check z position at time t
            return pz + vz * t == h.pz + h.vz * t;
        }
        
        // Calculate time t when x positions are equal
        long t = (px - h.px) / (h.vx - vx);
        if (t < 0) {
            return false; // Collision in the past
        }
        
        // Check y position at time t
        if (py + vy * t != h.py + h.vy * t) {
            return false;
        }
        
        // Check z position at time t
        return pz + vz * t == h.pz + h.vz * t;
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
}
