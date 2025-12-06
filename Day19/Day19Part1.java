package Day19;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day19Part1 {
    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new FileReader("Day19/input.txt"))) {
            Map<String, Workflow> workflows = new HashMap<>();
            List<Part> parts = new ArrayList<>();
            
            // Parse workflows
            String line;
            while ((line = reader.readLine()) != null && !line.isEmpty()) {
                // Format: px{a<2006:qkq,m>2090:A,rfg}
                int braceIndex = line.indexOf('{');
                String name = line.substring(0, braceIndex);
                String rulesStr = line.substring(braceIndex + 1, line.length() - 1);
                workflows.put(name, new Workflow(name, rulesStr));
            }
            
            // Parse parts
            while ((line = reader.readLine()) != null) {
                if (!line.trim().isEmpty()) {
                    parts.add(Part.fromString(line.trim()));
                }
            }
            
            // Process parts
            int total = 0;
            for (Part part : parts) {
                String current = "in";
                
                while (!current.equals("A") && !current.equals("R")) {
                    Workflow workflow = workflows.get(current);
                    current = workflow.process(part);
                }
                
                if (current.equals("A")) {
                    total += part.getRatingSum();
                }
            }
            
            System.out.println("Total sum of accepted parts: " + total);
            
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
