package Day6;

import java.util.*;

public class Day6 {
    public static void main(String[] args) {
        List<String> input = Day3.Reader.readFromFile();
        
        // Parse input
        String[] timeParts = input.get(0).split("\\s+");
        String[] distanceParts = input.get(1).split("\\s+");
        
        List<Race> races = new ArrayList<>();
        for (int i = 1; i < timeParts.length; i++) {
            int time = Integer.parseInt(timeParts[i]);
            int distance = Integer.parseInt(distanceParts[i]);
            races.add(new Race(time, distance));
        }
        
        // Calculate the product of ways to win each race
        int result = 1;
        for (Race race : races) {
            result *= race.countWaysToWin();
        }
        
        System.out.println("Part 1: " + result);
    }
    
    static class Race {
        int time;
        int recordDistance;
        
        public Race(int time, int recordDistance) {
            this.time = time;
            this.recordDistance = recordDistance;
        }
        
        public int countWaysToWin() {
            int count = 0;
            for (int holdTime = 0; holdTime <= time; holdTime++) {
                int travelTime = time - holdTime;
                int distance = holdTime * travelTime;
                if (distance > recordDistance) {
                    count++;
                }
            }
            return count;
        }
    }
}
