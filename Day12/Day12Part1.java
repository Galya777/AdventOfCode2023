package Day12;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

public class Day12Part1 {
    public static void main(String[] args) {
        try {
            File file = new File("Day12/input.txt");
            Scanner scanner = new Scanner(file);
            long totalArrangements = 0;

            int lineNumber = 0;
            while (scanner.hasNextLine()) {
                lineNumber++;
                String line = scanner.nextLine().trim();
                if (line.isEmpty()) continue;  // Skip empty lines
                
                String[] parts = line.split("\\s+");
                if (parts.length < 2) {
                    System.err.println("Warning: Invalid line format at line " + lineNumber + ": " + line);
                    continue;
                }
                
                String springs = parts[0];
                String[] groupStrs = parts[1].split(",");
                List<Integer> groups = new ArrayList<>();
                try {
                    for (String g : groupStrs) {
                        if (!g.trim().isEmpty()) {
                            groups.add(Integer.parseInt(g.trim()));
                        }
                    }
                } catch (NumberFormatException e) {
                    System.err.println("Warning: Invalid number format at line " + lineNumber + ": " + line);
                    continue;
                }
                
                Map<String, Long> memo = new HashMap<>();
                long arrangements = countArrangements(springs, groups, 0, 0, 0, memo);
                totalArrangements += arrangements;
            }

            System.out.println("Total arrangements: " + totalArrangements);
            scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("File not found");
            e.printStackTrace();
        }
    }

    private static long countArrangements(String s, List<Integer> groups, int pos, int currentGroup, int groupPos, Map<String, Long> memo) {
        // Create a memoization key
        String key = pos + "," + currentGroup + "," + groupPos;
        if (memo.containsKey(key)) {
            return memo.get(key);
        }

        // Base case: we've reached the end of the string
        if (pos == s.length()) {
            // If we've processed all groups and we're not in the middle of a group
            if (groupPos == groups.size() && currentGroup == 0) {
                return 1;
            }
            // If we're at the last group and the current group matches the required size
            if (groupPos == groups.size() - 1 && groups.get(groupPos) == currentGroup) {
                return 1;
            }
            // Otherwise, this is not a valid arrangement
            return 0;
        }

        long count = 0;
        char c = s.charAt(pos);
        
        // If the current character is or could be a working spring (.)
        if (c == '.' || c == '?') {
            if (currentGroup == 0) {
                // Not in a group, just move to the next position
                count += countArrangements(s, groups, pos + 1, 0, groupPos, memo);
            } else if (groupPos < groups.size() && currentGroup == groups.get(groupPos)) {
                // We've completed a group, move to next group and reset currentGroup
                count += countArrangements(s, groups, pos + 1, 0, groupPos + 1, memo);
            }
        }

        // If the current character is or could be a damaged spring (#)
        if (c == '#' || c == '?') {
            // Only proceed if we haven't exceeded the current group size
            if (groupPos < groups.size() && (currentGroup < groups.get(groupPos))) {
                count += countArrangements(s, groups, pos + 1, currentGroup + 1, groupPos, memo);
            }
        }

        memo.put(key, count);
        return count;
    }
}
