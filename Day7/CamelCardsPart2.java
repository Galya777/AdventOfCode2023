package Day7;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class CamelCardsPart2 {
    // J is now the weakest card
    private static final String CARD_ORDER = "AKQT98765432J";
    
    public static void main(String[] args) {
        try {
            // Read input file directly
            List<String> input = new ArrayList<>();
            try (BufferedReader reader = new BufferedReader(new FileReader("Day7/input.txt"))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    input.add(line);
                }
            } catch (IOException e) {
                System.err.println("Error reading file: " + e.getMessage());
                System.exit(1);
            }
            List<Hand> hands = new ArrayList<>();
            
            System.out.println("Read " + input.size() + " lines from input");
            
            // Parse input
            for (String line : input) {
                line = line.trim();
                if (line.isEmpty()) continue; // Skip empty lines
                
                String[] parts = line.split("\\s+");
                if (parts.length < 2) {
                    System.err.println("Skipping invalid line (not enough parts): " + line);
                    continue;
                }
                
                try {
                    String cards = parts[0];
                    int bid = Integer.parseInt(parts[1]);
                    if (cards.length() != 5) {
                        System.err.println("Skipping line with invalid hand (must be 5 cards): " + line);
                        continue;
                    }
                    hands.add(new Hand(cards, bid));
                } catch (NumberFormatException e) {
                    System.err.println("Skipping line with invalid bid: " + line);
                    continue;
                }
            }
            
            System.out.println("Successfully parsed " + hands.size() + " valid hands");
            
            // Sort hands based on their strength
            Collections.sort(hands);
            
            // Calculate total winnings
            long totalWinnings = 0;
            for (int i = 0; i < hands.size(); i++) {
                int rank = i + 1;
                Hand hand = hands.get(i);
                totalWinnings += (long) hand.bid * rank;
                
                // Debug output for first few and last few hands
                if (i < 3 || i >= hands.size() - 3) {
                    System.out.printf("Rank %4d: %s (type: %d) bid: %4d  running total: %d%n",
                            rank, hand.cards, hand.type, hand.bid, totalWinnings);
                } else if (i == 3) {
                    System.out.println("...");
                }
            }
            
            System.out.println("\nTotal winnings with jokers: " + totalWinnings);
            
        } catch (Exception e) {
            System.err.println("An error occurred: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    static class Hand implements Comparable<Hand> {
        String cards;
        int bid;
        int type;
        
        public Hand(String cards, int bid) {
            this.cards = cards;
            this.bid = bid;
            this.type = calculateTypeWithJokers();
        }
        
        private int calculateTypeWithJokers() {
            Map<Character, Integer> cardCounts = new HashMap<>();
            int jokers = 0;
            
            // Count cards and jokers
            for (char c : cards.toCharArray()) {
                if (c == 'J') {
                    jokers++;
                } else {
                    cardCounts.put(c, cardCounts.getOrDefault(c, 0) + 1);
                }
            }
            
            List<Integer> counts = new ArrayList<>(cardCounts.values());
            Collections.sort(counts, Collections.reverseOrder());
            
            // Handle all jokers case
            if (counts.isEmpty()) {
                counts.add(0);
            }
            
            // Add jokers to the highest count (to make the best possible hand)
            counts.set(0, counts.get(0) + jokers);
            
            // Determine hand type
            if (counts.get(0) == 5) return 7; // Five of a kind
            if (counts.get(0) == 4) return 6; // Four of a kind
            if (counts.get(0) == 3 && counts.get(1) == 2) return 5; // Full house
            if (counts.get(0) == 3) return 4; // Three of a kind
            if (counts.get(0) == 2 && counts.get(1) == 2) return 3; // Two pair
            if (counts.get(0) == 2) return 2; // One pair
            return 1; // High card
        }
        
        @Override
        public int compareTo(Hand other) {
            if (this.type != other.type) {
                return Integer.compare(this.type, other.type);
            }
            
            // Compare cards one by one
            for (int i = 0; i < 5; i++) {
                int thisCard = CARD_ORDER.indexOf(this.cards.charAt(i));
                int otherCard = CARD_ORDER.indexOf(other.cards.charAt(i));
                if (thisCard != otherCard) {
                    return Integer.compare(otherCard, thisCard); // Higher index means weaker card
                }
            }
            return 0;
        }
    }
}
