package Day13;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day13Part2 {
    public static void main(String[] args) {
        String inputFile = "Day13/input.txt";
        List<String> patterns = readInput(inputFile);
        int total = 0;
        
        for (String pattern : patterns) {
            total += findSmudgedReflectionValue(pattern);
        }
        
        System.out.println("Total sum with smudges: " + total);
    }
    
    private static List<String> readInput(String filename) {
        List<String> patterns = new ArrayList<>();
        StringBuilder currentPattern = new StringBuilder();
        
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.trim().isEmpty()) {
                    if (currentPattern.length() > 0) {
                        patterns.add(currentPattern.toString().trim());
                        currentPattern = new StringBuilder();
                    }
                } else {
                    currentPattern.append(line).append("\n");
                }
            }
            if (currentPattern.length() > 0) {
                patterns.add(currentPattern.toString().trim());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        return patterns;
    }
    
    private static int findSmudgedReflectionValue(String pattern) {
        String[] lines = pattern.split("\n");
        
        // Check for horizontal reflection with exactly one smudge
        int horizontal = findSmudgedHorizontalReflection(lines);
        if (horizontal > 0) {
            return 100 * horizontal;
        }
        
        // Check for vertical reflection with exactly one smudge
        int vertical = findSmudgedVerticalReflection(lines);
        if (vertical > 0) {
            return vertical;
        }
        
        return 0;
    }
    
    private static int findSmudgedHorizontalReflection(String[] lines) {
        for (int i = 1; i < lines.length; i++) {
            int smudges = 0;
            int left = i - 1;
            int right = i;
            
            while (left >= 0 && right < lines.length) {
                smudges += countDifferences(lines[left], lines[right]);
                if (smudges > 1) {
                    break;
                }
                left--;
                right++;
            }
            
            if (smudges == 1) {
                return i;
            }
        }
        
        return 0;
    }
    
    private static int findSmudgedVerticalReflection(String[] lines) {
        int width = lines[0].length();
        
        for (int col = 1; col < width; col++) {
            int totalSmudges = 0;
            boolean valid = true;
            
            for (String line : lines) {
                int left = col - 1;
                int right = col;
                int smudges = 0;
                
                while (left >= 0 && right < width) {
                    if (line.charAt(left) != line.charAt(right)) {
                        smudges++;
                        if (smudges > 1) {
                            valid = false;
                            break;
                        }
                    }
                    left--;
                    right++;
                }
                
                totalSmudges += smudges;
                if (totalSmudges > 1) {
                    valid = false;
                    break;
                }
            }
            
            if (valid && totalSmudges == 1) {
                return col;
            }
        }
        
        return 0;
    }
    
    private static int countDifferences(String s1, String s2) {
        int differences = 0;
        for (int i = 0; i < s1.length(); i++) {
            if (s1.charAt(i) != s2.charAt(i)) {
                differences++;
            }
        }
        return differences;
    }
}
