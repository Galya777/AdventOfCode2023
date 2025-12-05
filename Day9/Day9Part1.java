package Day9;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day9Part1 {
    public static void main(String[] args) {
        String inputFile = "Day9/input.txt";
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            long totalSum = 0;
            String line;
            
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                
                // Parse the sequence of numbers
                long[] sequence = Arrays.stream(line.split("\\s+"))
                        .mapToLong(Long::parseLong)
                        .toArray();
                
                // Calculate the next value in the sequence
                long nextValue = findNextValue(sequence);
                totalSum += nextValue;
                
                System.out.println("Sequence: " + Arrays.toString(sequence) + " -> Next value: " + nextValue);
            }
            
            System.out.println("\nSum of all extrapolated values: " + totalSum);
            
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    private static long findNextValue(long[] sequence) {
        List<long[]> sequences = new ArrayList<>();
        sequences.add(sequence);
        
        // Generate sequences of differences until we get all zeros
        while (!isAllZeros(sequences.get(sequences.size() - 1))) {
            long[] current = sequences.get(sequences.size() - 1);
            long[] next = new long[current.length - 1];
            
            for (int i = 0; i < current.length - 1; i++) {
                next[i] = current[i + 1] - current[i];
            }
            
            sequences.add(next);
        }
        
        // Work our way back up to find the next value
        // Start by adding a zero to the last sequence
        long[] lastSequence = sequences.get(sequences.size() - 1);
        long[] newLastSequence = Arrays.copyOf(lastSequence, lastSequence.length + 1);
        sequences.set(sequences.size() - 1, newLastSequence);
        
        // Process each sequence from bottom to top
        for (int i = sequences.size() - 2; i >= 0; i--) {
            long[] current = sequences.get(i);
            long[] prev = sequences.get(i + 1);
            
            // The new value is the last value of current sequence plus the last value of the sequence below it
            long newValue = current[current.length - 1] + prev[prev.length - 1];
            
            // Create a new array with one more element
            long[] newCurrent = Arrays.copyOf(current, current.length + 1);
            newCurrent[newCurrent.length - 1] = newValue;
            sequences.set(i, newCurrent);
        }      
        
        // The last value of the first sequence is our result
        return sequences.get(0)[sequences.get(0).length - 1];
    }
    
    private static boolean isAllZeros(long[] array) {
        for (long num : array) {
            if (num != 0) {
                return false;
            }
        }
        return true;
    }
}