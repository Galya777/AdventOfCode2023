package Day5;

import Day3.Reader;

import java.util.ArrayList;
import java.util.List;

public class Day5 {

    static class Range {
        long srcStart, destStart, length;

        public Range(long destStart, long srcStart, long length) {
            this.destStart = destStart;
            this.srcStart = srcStart;
            this.length = length;
        }

        public boolean contains(long num) {
            return num >= srcStart && num < srcStart + length;
        }

        public long map(long num) {
            return destStart + (num - srcStart);
        }
    }

    public static void main(String[] args) {
        List<String> lines = Reader.readFromFile();
        if (lines == null || lines.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        // Parse seeds
        List<Long> seeds = new ArrayList<>();
        int i = 0;
        while (!lines.get(i).startsWith("seeds:")) i++;
        String seedLine = lines.get(i).substring(lines.get(i).indexOf(":") + 1).trim();
        for (String s : seedLine.split("\\s+")) seeds.add(Long.parseLong(s));
        i++;

        // Parse maps
        List<List<Range>> maps = new ArrayList<>();
        while (i < lines.size()) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) {
                i++;
                continue;
            }

            if (line.endsWith("map:")) {
                i++;
                List<Range> currentMap = new ArrayList<>();
                while (i < lines.size() && !lines.get(i).trim().endsWith("map:") && !lines.get(i).trim().isEmpty()) {
                    String[] parts = lines.get(i).trim().split("\\s+");
                    if (parts.length == 3) {
                        long destStart = Long.parseLong(parts[0]);
                        long srcStart = Long.parseLong(parts[1]);
                        long length = Long.parseLong(parts[2]);
                        currentMap.add(new Range(destStart, srcStart, length));
                    }
                    i++;
                }
                maps.add(currentMap);
            } else {
                i++;
            }
        }

        // Map seeds through all ranges
        List<Long> locations = new ArrayList<>();
        for (long seed : seeds) {
            long value = seed;
            for (List<Range> map : maps) {
                boolean mapped = false;
                for (Range r : map) {
                    if (r.contains(value)) {
                        value = r.map(value);
                        mapped = true;
                        break;
                    }
                }
                if (!mapped) value = value; // identity if no range matches
            }
            locations.add(value);
        }

        // Find minimum location
        long minLocation = locations.stream().min(Long::compareTo).orElse(-1L);
        System.out.println("Lowest location number: " + minLocation);
    }
}
