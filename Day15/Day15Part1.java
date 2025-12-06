package Day15;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Day15Part1 {
    public static void main(String[] args) {
        try {
            // Read the input file
            String input = readInput("Day15/input.txt");
            
            // Split the input by commas to get individual steps
            String[] steps = input.split(",");
            
            int totalSum = 0;
            
            // Process each step
            for (String step : steps) {
                int hashValue = calculateHash(step);
                totalSum += hashValue;
            }
            
            System.out.println("The sum of the results is: " + totalSum);
            
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }
    
    private static String readInput(String filePath) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line);
            }
        }
        return content.toString();
    }
    
    private static int calculateHash(String input) {
        int currentValue = 0;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            // Get ASCII value of the character
            int ascii = (int) c;
            
            // Update current value according to the HASH algorithm
            currentValue += ascii;
            currentValue *= 17;
            currentValue %= 256;
        }
        
        return currentValue;
    }
}
