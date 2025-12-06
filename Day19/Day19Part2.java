package Day19;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day19Part2 {
    private static class Range {
        int min, max;

        Range(int min, int max) {
            this.min = min;
            this.max = max;
        }

        Range(Range other) {
            this.min = other.min;
            this.max = other.max;
        }

        long count() {
            return Math.max(0, max - min + 1);
        }
    }

    private static class PartRange {
        Range x, m, a, s;

        PartRange() {
            x = new Range(1, 4000);
            m = new Range(1, 4000);
            a = new Range(1, 4000);
            s = new Range(1, 4000);
        }

        PartRange(PartRange other) {
            this.x = new Range(other.x);
            this.m = new Range(other.m);
            this.a = new Range(other.a);
            this.s = new Range(other.s);
        }

        long combinations() {
            return x.count() * m.count() * a.count() * s.count();
        }

        void applyCondition(String condition) {
            if (condition == null || condition.isEmpty()) return;

            char category = condition.charAt(0);
            char operator = condition.charAt(1);
            int value = Integer.parseInt(condition.substring(2));

            Range range = getRange(category);
            if (operator == '<') {
                range.max = Math.min(range.max, value - 1);
            } else if (operator == '>') {
                range.min = Math.max(range.min, value + 1);
            }
        }

        Range getRange(char category) {
            return switch (category) {
                case 'x' -> x;
                case 'm' -> m;
                case 'a' -> a;
                case 's' -> s;
                default -> throw new IllegalArgumentException("Invalid category: " + category);
            };
        }
    }

    private static long countCombinations(String workflowName, PartRange partRange, Map<String, String> workflows) {
        if (workflowName.equals("A")) {
            return partRange.combinations();
        } else if (workflowName.equals("R")) {
            return 0;
        }

        long total = 0;
        String[] rules = workflows.get(workflowName).split(",");

        for (String rule : rules) {
            if (!rule.contains(":")) {
                total += countCombinations(rule, partRange, workflows);
                break;
            }

            String[] parts = rule.split(":");
            String condition = parts[0];
            String destination = parts[1];

            PartRange newRange = new PartRange(partRange);
            newRange.applyCondition(condition);
            total += countCombinations(destination, newRange, workflows);

            // Update the current range for the next rule
            partRange.applyCondition(invertCondition(condition));
        }

        return total;
    }

    private static String invertCondition(String condition) {
        if (condition == null || condition.isEmpty()) return "";

        char operator = condition.charAt(1);
        int value = Integer.parseInt(condition.substring(2));

        if (operator == '<') {
            return condition.charAt(0) + ">" + (value - 1);
        } else {
            return condition.charAt(0) + "<" + (value + 1);
        }
    }

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("Day19/input.txt"))) {
            Map<String, String> workflows = new HashMap<>();
            String line;

            // Parse workflows
            while ((line = reader.readLine()) != null && !line.isEmpty()) {
                int braceIndex = line.indexOf('{');
                String name = line.substring(0, braceIndex);
                String rules = line.substring(braceIndex + 1, line.length() - 1);
                workflows.put(name, rules);
            }

            PartRange initialRange = new PartRange();
            long result = countCombinations("in", initialRange, workflows);
            System.out.println("Total distinct combinations accepted: " + result);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}