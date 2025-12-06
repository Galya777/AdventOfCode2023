package Day15;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day15Part2 {
    static class Lens {
        String label;
        int focalLength;

        public Lens(String label, int focalLength) {
            this.label = label;
            this.focalLength = focalLength;
        }

        @Override
        public String toString() {
            return "[" + label + " " + focalLength + "]";
        }
    }

    public static void main(String[] args) {
        try {
            // Read the input file
            String input = readInput("Day15/input.txt");
            
            // Initialize 256 boxes, each containing a list of lenses
            List<List<Lens>> boxes = new ArrayList<>(256);
            for (int i = 0; i < 256; i++) {
                boxes.add(new ArrayList<>());
            }
            
            // Process each step in the initialization sequence
            String[] steps = input.split(",");
            for (String step : steps) {
                processStep(step, boxes);
            }
            
            // Calculate the total focusing power
            int totalPower = calculateFocusingPower(boxes);
            System.out.println("The total focusing power is: " + totalPower);
            
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
    
    private static int calculateHash(String s) {
        int currentValue = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            currentValue += c;
            currentValue *= 17;
            currentValue %= 256;
        }
        return currentValue;
    }
    
    private static void processStep(String step, List<List<Lens>> boxes) {
        if (step.contains("-")) {
            // Handle removal operation
            String label = step.substring(0, step.length() - 1);
            int boxIndex = calculateHash(label);
            List<Lens> box = boxes.get(boxIndex);
            
            // Remove the lens with the given label if it exists
            box.removeIf(lens -> lens.label.equals(label));
        } else if (step.contains("=")) {
            // Handle addition/update operation
            String[] parts = step.split("=");
            String label = parts[0];
            int focalLength = Integer.parseInt(parts[1]);
            int boxIndex = calculateHash(label);
            List<Lens> box = boxes.get(boxIndex);
            
            // Check if a lens with this label already exists
            boolean found = false;
            for (Lens lens : box) {
                if (lens.label.equals(label)) {
                    // Update existing lens
                    lens.focalLength = focalLength;
                    found = true;
                    break;
                }
            }
            
            // If no existing lens found, add a new one
            if (!found) {
                box.add(new Lens(label, focalLength));
            }
        }
    }
    
    private static int calculateFocusingPower(List<List<Lens>> boxes) {
        int totalPower = 0;
        
        for (int boxIndex = 0; boxIndex < boxes.size(); boxIndex++) {
            List<Lens> box = boxes.get(boxIndex);
            for (int slotIndex = 0; slotIndex < box.size(); slotIndex++) {
                Lens lens = box.get(slotIndex);
                int power = (boxIndex + 1) * (slotIndex + 1) * lens.focalLength;
                totalPower += power;
            }
        }
        
        return totalPower;
    }
}
