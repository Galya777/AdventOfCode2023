package Day4;

import Day3.Reader;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day4 {

    public static void main(String[] args) {
        // Read scratchcards input
        List<String> cards = Reader.readFromFile(); // each line: "winning | your numbers"
        if (cards == null || cards.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        // Remove blank lines
        cards.removeIf(String::isEmpty);

        int totalPoints = 0;

        for (String card : cards) {
            totalPoints += calculateCardPoints(card);
        }

        System.out.println("Total points: " + totalPoints);
    }

    private static int calculateCardPoints(String cardLine) {
        // Remove any "Card N:" prefix
        if (cardLine.contains(":")) {
            cardLine = cardLine.substring(cardLine.indexOf(":") + 1).trim();
        }

        // Split into winning numbers and your numbers
        String[] parts = cardLine.split("\\|");
        if (parts.length != 2) return 0;

        Set<Integer> winningNumbers = new HashSet<>();
        for (String s : parts[0].trim().split("\\s+")) {
            winningNumbers.add(Integer.parseInt(s));
        }

        String[] yourNumbers = parts[1].trim().split("\\s+");
        int points = 0;
        boolean firstMatch = true;

        for (String numStr : yourNumbers) {
            int num = Integer.parseInt(numStr);
            if (winningNumbers.contains(num)) {
                if (firstMatch) {
                    points += 1; // first match is 1 point
                    firstMatch = false;
                } else {
                    points *= 2; // each subsequent match doubles the points
                }
            }
        }

        return points;
    }

}
