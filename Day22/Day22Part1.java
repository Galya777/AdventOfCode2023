package Day22;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Day22Part1 {
    public static void main(String[] args) {
        try {
            List<String> lines = Files.readAllLines(Paths.get("Day22/input.txt"));
            List<Brick> bricks = new ArrayList<>();
            
            // Parse bricks
            for (String line : lines) {
                if (!line.trim().isEmpty()) {
                    bricks.add(Brick.fromString(line));
                }
            }
            
            // Sort bricks by their lowest z-coordinate
            bricks.sort(Comparator.naturalOrder());
            
            // Let bricks fall
            simulateFall(bricks);
            
            // Count safely removable bricks
            int result = countSafelyRemovableBricks(bricks);
            
            System.out.println("Number of bricks that can be safely disintegrated: " + result);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    private static void simulateFall(List<Brick> bricks) {
        System.out.println("Simulating fall for " + bricks.size() + " bricks");
        // Sort bricks by their lowest z-coordinate
        bricks.sort(Comparator.naturalOrder());
        
        // Keep track of occupied positions (point -> brick)
        Map<Point3D, Brick> occupied = new HashMap<>();
        
        // Process each brick in order of increasing z-coordinate
        for (int i = 0; i < bricks.size(); i++) {
            if (i % 100 == 0) {
                System.out.println("Processing brick " + i + "/" + bricks.size());
            }
            Brick brick = bricks.get(i);
            boolean canFall = true;
            
            // Clear any existing support relationships
            brick.supportedBy.clear();
            
            // Remove brick from occupied positions while we move it
            for (Point3D cube : brick.getCubes()) {
                occupied.remove(cube);
            }
            
            while (canFall && !brick.isAtGround()) {
                // Try to move brick down
                Brick moved = brick.moveDown();
                
                // Check for collisions with the ground or other bricks
                boolean collision = false;
                for (Point3D cube : moved.getCubes()) {
                    Brick other = occupied.get(cube);
                    if (other != null) {
                        collision = true;
                        // Record support relationship
                        brick.supportedBy.add(other);
                        other.supports.add(brick);
                    }
                }
                
                if (collision || moved.isAtGround()) {
                    canFall = false;
                    if (moved.isAtGround()) {
                        // If we hit the ground, we need to add support from the ground
                        // We'll represent the ground as a special case with z=0
                        brick.supportedBy.add(null); // null represents the ground
                    }
                } else {
                    brick = moved;
                }
            }
            
            // Update the brick in the list
            bricks.set(i, brick);
            
            // Mark final position as occupied
            for (Point3D cube : brick.getCubes()) {
                occupied.put(cube, brick);
            }
        }
    }
    
    private static int countSafelyRemovableBricks(List<Brick> bricks) {
        int count = 0;
        
        for (Brick brick : bricks) {
            boolean canRemove = true;
            System.out.println("\nChecking brick: " + brick + " (supports " + brick.supports.size() + " bricks, supported by " + brick.supportedBy.size() + " bricks)");
            
            // Check all bricks this brick supports
            for (Brick supported : brick.supports) {
                // Count how many bricks (excluding ground) support this brick
                long supportCount = supported.supportedBy.stream()
                    .filter(b -> b != null) // Exclude ground
                    .count();
                
                // If a supported brick is only supported by this brick (and possibly the ground), we can't remove it
                System.out.println("  - Supports brick " + supported + " which is supported by " + supportCount + " other bricks");
                if (supportCount < 2) {
                    System.out.println("  ! Cannot remove this brick as it's the only support for " + supported);
                    canRemove = false;
                    break;
                }
            }
            
            if (canRemove) {
                System.out.println("  + Can safely remove this brick");
                count++;
            } else {
                System.out.println("  - Cannot remove this brick");
            }
        }
        
        return count;
    }
}
