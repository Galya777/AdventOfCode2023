package Day8;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

public class Day8Part2 {
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
                String[] connections = parts[1].replace("(", "").replace(")", "").replace(" ", "").split(",");
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
            
            // Find all starting nodes (ending with 'A')
            List<NetworkNode> currentNodes = nodeMap.values().stream()
                    .filter(node -> node.getName().endsWith("A"))
                    .collect(Collectors.toList());
            
            if (currentNodes.isEmpty()) {
                throw new RuntimeException("No starting nodes found (nodes ending with 'A')");
            }
            
            System.out.println("Starting nodes: " + 
                currentNodes.stream().map(NetworkNode::getName).collect(Collectors.joining(", ")));
            
            // Calculate steps for each starting node to reach a node ending with 'Z'
            List<Long> stepsList = new ArrayList<>();
            
            for (NetworkNode startNode : currentNodes) {
                long steps = 0;
                NetworkNode currentNode = startNode;
                int instructionIndex = 0;
                
                while (!currentNode.getName().endsWith("Z")) {
                    char direction = instructions.charAt(instructionIndex);
                    currentNode = (direction == 'L') ? currentNode.getLeft() : currentNode.getRight();
                    
                    steps++;
                    instructionIndex = (instructionIndex + 1) % instructions.length();
                    
                    // Safety check to prevent infinite loops
                    if (steps > 1_000_000) {
                        System.out.println("Possible infinite loop detected for node " + startNode.getName());
                        break;
                    }
                }
                
                if (currentNode.getName().endsWith("Z")) {
                    System.out.println("Node " + startNode.getName() + " reaches " + currentNode.getName() + " in " + steps + " steps");
                    stepsList.add(steps);
                }
            }
            
            // Calculate the least common multiple of all step counts
            long lcm = calculateLCM(stepsList);
            System.out.println("\nAll paths reach Z nodes after " + lcm + " steps");
            
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    // Helper method to calculate the least common multiple of a list of numbers
    private static long calculateLCM(List<Long> numbers) {
        return numbers.stream()
                .reduce(1L, (a, b) -> (a * b) / calculateGCD(a, b));
    }
    
    // Helper method to calculate the greatest common divisor using Euclidean algorithm
    private static long calculateGCD(long a, long b) {
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }
}
