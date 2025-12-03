package Day5;

import Day3.Reader;

import java.util.ArrayList;
import java.util.List;

public class Day5Part2 {

    // Range [start, end]
    static class Range {
        long start, end;
        public Range(long start, long end) { this.start = start; this.end = end; }
    }

    // Mapping: source range -> destination range
    static class MapRange {
        long srcStart, destStart, length;
        public MapRange(long destStart, long srcStart, long length) {
            this.srcStart = srcStart;
            this.destStart = destStart;
            this.length = length;
        }
        // Return overlap with a given range
        public Range overlap(Range r) {
            long oStart = Math.max(r.start, srcStart);
            long oEnd = Math.min(r.end, srcStart + length - 1);
            if (oStart <= oEnd) return new Range(oStart, oEnd);
            return null;
        }
        // Map an overlapping range to destination
        public Range map(Range r) {
            long offset = r.start - srcStart;
            return new Range(destStart + offset, destStart + offset + (r.end - r.start));
        }
    }

    public static void main(String[] args) {
        List<String> lines = Reader.readFromFile();
        if (lines == null || lines.isEmpty()) {
            System.out.println("Input file is empty or missing!");
            return;
        }

        // --- Parse seed ranges ---
        List<Range> seedRanges = new ArrayList<>();
        int i = 0;
        while (!lines.get(i).startsWith("seeds:")) i++;
        String seedLine = lines.get(i).substring(lines.get(i).indexOf(":") + 1).trim();
        String[] parts = seedLine.split("\\s+");
        for (int j = 0; j < parts.length; j += 2) {
            long start = Long.parseLong(parts[j]);
            long length = Long.parseLong(parts[j + 1]);
            seedRanges.add(new Range(start, start + length - 1));
        }
        i++;

        // --- Parse maps ---
        List<List<MapRange>> maps = new ArrayList<>();
        while (i < lines.size()) {
            String line = lines.get(i).trim();
            if (line.isEmpty()) { i++; continue; }
            if (line.endsWith("map:")) {
                i++;
                List<MapRange> currentMap = new ArrayList<>();
                while (i < lines.size() && !lines.get(i).trim().endsWith("map:") && !lines.get(i).trim().isEmpty()) {
                    String[] mapParts = lines.get(i).trim().split("\\s+");
                    if (mapParts.length == 3) {
                        long destStart = Long.parseLong(mapParts[0]);
                        long srcStart = Long.parseLong(mapParts[1]);
                        long lengthMap = Long.parseLong(mapParts[2]);
                        currentMap.add(new MapRange(destStart, srcStart, lengthMap));
                    }
                    i++;
                }
                maps.add(currentMap);
            } else i++;
        }

        // --- Propagate seed ranges through all maps ---
        List<Range> currentRanges = new ArrayList<>(seedRanges);

        for (List<MapRange> map : maps) {
            List<Range> nextRanges = new ArrayList<>();
            
            // Sort map ranges by source start for efficient processing
            map.sort((a, b) -> Long.compare(a.srcStart, b.srcStart));
            
            for (Range r : currentRanges) {
                long current = r.start;
                long end = r.end;
                
                // Process the current range through all possible mappings
                for (MapRange mr : map) {
                    if (current >= end) break;
                    
                    // Check if there's a gap before this mapping
                    if (current < mr.srcStart) {
                        nextRanges.add(new Range(current, Math.min(mr.srcStart - 1, end)));
                        current = mr.srcStart;
                        if (current > end) break;
                    }
                    
                    // Check for overlap with current mapping
                    if (current < mr.srcStart + mr.length && current <= end) {
                        long overlapEnd = Math.min(mr.srcStart + mr.length - 1, end);
                        long offset = current - mr.srcStart;
                        nextRanges.add(new Range(
                            mr.destStart + offset,
                            mr.destStart + (overlapEnd - mr.srcStart)
                        ));
                        current = overlapEnd + 1;
                    }
                }
                
                // Add any remaining unmapped range
                if (current <= end) {
                    nextRanges.add(new Range(current, end));
                }
            }
            
            // Prepare for next mapping stage
            currentRanges = nextRanges;
        }

        // --- Find minimum location ---
        long minLocation = Long.MAX_VALUE;
        for (Range r : currentRanges) {
            if (r.start < minLocation) minLocation = r.start;
        }

        System.out.println("Lowest location number: " + minLocation);
    }
}
