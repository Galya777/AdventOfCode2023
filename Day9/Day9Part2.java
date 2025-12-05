package Day9;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day9Part2 {
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
                
                // Calculate the previous value in the sequence
                long prevValue = findPreviousValue(sequence);
                totalSum += prevValue;
                
                System.out.println("Sequence: " + Arrays.toString(sequence) + " -> Previous value: " + prevValue);
            }
            
            System.out.println("\nSum of all extrapolated previous values: " + totalSum);
            
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    private static long findPreviousValue(long[] sequence) {
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
        
        // Start with zero at the beginning of the last sequence
        long[] lastSequence = sequences.get(sequences.size() - 1);
        long[] newLastSequence = new long[lastSequence.length + 1];
        System.arraycopy(lastSequence, 0, newLastSequence, 1, lastSequence.length);
        sequences.set(sequences.size() - 1, newLastSequence);
        
        // Process each sequence from bottom to top
        for (int i = sequences.size() - 2; i >= 0; i--) {
            long[] current = sequences.get(i);
            long[] prev = sequences.get(i + 1);
            
            // The new first value is the first value of current sequence minus the first value of the sequence below it
            long newFirstValue = current[0] - prev[0];
            
            // Create a new array with one more element at the beginning
            long[] newCurrent = new long[current.length + 1];
            newCurrent[0] = newFirstValue;
            System.arraycopy(current, 0, newCurrent, 1, current.length);
            sequences.set(i, newCurrent);
        }
        
        // The first value of the first sequence is our result
        return sequences.get(0)[0];
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
