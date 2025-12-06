package Day13;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Day13Part1 {
    public static void main(String[] args) {
        String inputFile = "Day13/input.txt";
        List<String> patterns = readInput(inputFile);
        int total = 0;
        
        for (String pattern : patterns) {
            total += findReflectionValue(pattern);
        }
        
        System.out.println("Total sum: " + total);
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
            // Add the last pattern if there's no empty line at the end
            if (currentPattern.length() > 0) {
                patterns.add(currentPattern.toString().trim());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        return patterns;
    }
    
    private static int findReflectionValue(String pattern) {
        String[] lines = pattern.split("\n");
        
        // Check for horizontal reflection
        int horizontal = findHorizontalReflection(lines);
        if (horizontal > 0) {
            return 100 * horizontal;
        }
        
        // Check for vertical reflection
        int vertical = findVerticalReflection(lines);
        if (vertical > 0) {
            return vertical;
        }
        
        return 0;
    }
    
    private static int findHorizontalReflection(String[] lines) {
        for (int i = 1; i < lines.length; i++) {
            boolean isReflection = true;
            int left = i - 1;
            int right = i;
            
            while (left >= 0 && right < lines.length) {
                if (!lines[left].equals(lines[right])) {
                    isReflection = false;
                    break;
                }
                left--;
                right++;
            }
            
            if (isReflection) {
                return i;
            }
        }
        
        return 0;
    }
    
    private static int findVerticalReflection(String[] lines) {
        int width = lines[0].length();
        
        for (int col = 1; col < width; col++) {
            boolean isReflection = true;
            
            for (String line : lines) {
                int left = col - 1;
                int right = col;
                
                while (left >= 0 && right < width) {
                    if (line.charAt(left) != line.charAt(right)) {
                        isReflection = false;
                        break;
                    }
                    left--;
                    right++;
                }
                
                if (!isReflection) {
                    break;
                }
            }
            
            if (isReflection) {
                return col;
            }
        }
        
        return 0;
    }
}
