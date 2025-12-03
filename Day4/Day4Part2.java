package Day4;

import Day3.Reader;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Day4Part2 {

    public static void main(String[] args) {
        List<String> cards = Reader.readFromFile();
        if (cards == null || cards.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        cards.removeIf(String::isEmpty);

        long totalCards = processCards(cards);

        System.out.println("Total scratchcards after all copies: " + totalCards);
    }

    private static long processCards(List<String> cards) {
        int n = cards.size();
        long[] counts = new long[n];       // total instances of each card
        long[] newInstances = new long[n]; // instances to process in this round

        // Initialize with 1 original instance each
        for (int i = 0; i < n; i++) {
            counts[i] = 1;
            newInstances[i] = 1;
        }

        boolean hasNew = true;

        while (hasNew) {
            hasNew = false;
            long[] nextRound = new long[n];

            for (int i = 0; i < n; i++) {
                if (newInstances[i] > 0) {
                    int matches = countMatches(cards.get(i));

                    for (int j = 1; j <= matches; j++) {
                        int target = i + j;
                        if (target < n) {
                            nextRound[target] += newInstances[i];
                            hasNew = true;
                        }
                    }
                }
            }

            // Update counts and newInstances for next round
            for (int i = 0; i < n; i++) {
                counts[i] += nextRound[i];
                newInstances[i] = nextRound[i];
            }
        }

        long total = 0;
        for (long c : counts) total += c;
        return total;
    }

    private static int countMatches(String cardLine) {
        if (cardLine.contains(":")) {
            cardLine = cardLine.substring(cardLine.indexOf(":") + 1).trim();
        }

        String[] parts = cardLine.split("\\|");
        if (parts.length != 2) return 0;

        Set<Integer> winningNumbers = new HashSet<>();
        for (String s : parts[0].trim().split("\\s+")) {
            winningNumbers.add(Integer.parseInt(s));
        }

        String[] yourNumbers = parts[1].trim().split("\\s+");
        int matches = 0;
        for (String numStr : yourNumbers) {
            int num = Integer.parseInt(numStr);
            if (winningNumbers.contains(num)) {
                matches++;
            }
        }

        return matches;
    }
}
