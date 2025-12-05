package Day8;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day8Part1 {
    public static void main(String[] args) {
        String inputFile = "Day8/input.txt";
        try (BufferedReader reader = new BufferedReader(new FileReader(inputFile))) {
            String instructions = reader.readLine().trim();
            
            // Skip empty line
            reader.readLine();
            
            // Read all nodes
            Map<String, NetworkNode> nodeMap = new HashMap<>();
            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                
                String[] parts = line.split("\\s*=\\s*");
                if (parts.length != 2) {
                    System.err.println("Skipping malformed line: " + line);
                    continue;
                }
                
                String nodeName = parts[0].trim();
                String[] connections = parts[1].replace("(", "").replace(")", "").split(",\\s*");
                if (connections.length != 2) {
                    System.err.println("Skipping node with invalid connections: " + line);
                    continue;
                }
                
                String leftNode = connections[0].trim();
                String rightNode = connections[1].trim();
                
                nodeMap.put(nodeName, new NetworkNode(nodeName, leftNode, rightNode));
            }
            
            // Link nodes together
            for (NetworkNode node : nodeMap.values()) {
                String leftNodeName = node.getLeftNodeName();
                String rightNodeName = node.getRightNodeName();
                
                NetworkNode leftNode = nodeMap.get(leftNodeName);
                NetworkNode rightNode = nodeMap.get(rightNodeName);
                
                if (leftNode == null) {
                    System.err.println("Warning: Left node " + leftNodeName + " not found for node " + node.getName());
                }
                if (rightNode == null) {
                    System.err.println("Warning: Right node " + rightNodeName + " not found for node " + node.getName());
                }
                
                node.setLeft(leftNode);
                node.setRight(rightNode);
            }
            
            // Start at AAA and follow instructions until ZZZ
            NetworkNode currentNode = nodeMap.get("AAA");
            if (currentNode == null) {
                throw new RuntimeException("Starting node 'AAA' not found in the network!");
            }
            long steps = 0;
            int instructionIndex = 0;
            
            while (!currentNode.getName().equals("ZZZ")) {
                char direction = instructions.charAt(instructionIndex);
                currentNode = (direction == 'L') ? currentNode.getLeft() : currentNode.getRight();
                
                steps++;
                instructionIndex = (instructionIndex + 1) % instructions.length();
                
                // Safety check to prevent infinite loops
                if (steps > 1_000_000) {
                    System.out.println("Possible infinite loop detected. Exiting.");
                    break;
                }
            }
            
            if (currentNode.getName().equals("ZZZ")) {
                System.out.println("Steps to reach ZZZ: " + steps);
            } else {
                System.out.println("Failed to reach ZZZ.");
            }
            
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
