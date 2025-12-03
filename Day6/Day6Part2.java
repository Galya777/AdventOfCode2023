package Day6;

import java.util.*;

public class Day6Part2 {
    public static void main(String[] args) {
        List<String> input = Day3.Reader.readFromFile();
        
        // Parse input and concatenate numbers
        String timeStr = input.get(0).split(":")[1].replaceAll("\\s+", "");
        String distanceStr = input.get(1).split(":")[1].replaceAll("\\s+", "");
        
        long time = Long.parseLong(timeStr);
        long recordDistance = Long.parseLong(distanceStr);
        
        // Find the first and last hold times that beat the record
        long firstWin = 0;
        long lastWin = 0;
        
        // Find first hold time that wins
        for (long holdTime = 0; holdTime <= time; holdTime++) {
            long travelTime = time - holdTime;
            long distance = holdTime * travelTime;
            if (distance > recordDistance) {
                firstWin = holdTime;
                break;
            }
        }
        
        // Find last hold time that wins
        for (long holdTime = time; holdTime >= 0; holdTime--) {
            long travelTime = time - holdTime;
            long distance = holdTime * travelTime;
            if (distance > recordDistance) {
                lastWin = holdTime;
                break;
            }
        }
        
        // Calculate total ways to win
        long totalWays = lastWin - firstWin + 1;
        
        System.out.println("Part 2: " + totalWays);
    }
}
