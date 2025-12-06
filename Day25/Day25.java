package Day25;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class Day25 {
    static class Edge {
        String u, v;
        Edge(String u, String v) {
            this.u = u;
            this.v = v;
        }
    }

    static Map<String, Set<String>> graph = new HashMap<>();
    static List<Edge> edges = new ArrayList<>();

    public static void main(String[] args) {
        String inputFile = "Day25/input.txt";
        parseInput(inputFile);
        
        // We'll use Karger's algorithm to find the minimum cut
        // Since the minimum cut is expected to be 3, we'll keep trying until we find it
        int minCut = 0;
        int size1 = 0, size2 = 0;
        
        while (minCut != 3) {
            Map<String, Set<String>> tempGraph = new HashMap<>();
            for (Map.Entry<String, Set<String>> entry : graph.entrySet()) {
                tempGraph.put(entry.getKey(), new HashSet<>(entry.getValue()));
            }
            
            // Run Karger's algorithm
            int[] result = kargersAlgorithm(tempGraph);
            minCut = result[0];
            size1 = result[1];
            size2 = result[2];
        }
        
        System.out.println("Minimum cut size: " + minCut);
        System.out.println("Size of first component: " + size1);
        System.out.println("Size of second component: " + size2);
        System.out.println("Product of sizes: " + (size1 * size2));
    }

    private static int[] kargersAlgorithm(Map<String, Set<String>> tempGraph) {
        // Implement Karger's algorithm to find the minimum cut
        // This is a simplified version for the problem
        // We'll contract edges until only two nodes remain
        // The number of edges between them is the cut size
        
        // For simplicity, we'll use a list to represent the graph for contraction
        List<Edge> tempEdges = new ArrayList<>(edges);
        Map<String, List<String>> contracted = new HashMap<>();
        
        // Initialize each node as its own component
        for (String node : graph.keySet()) {
            List<String> component = new ArrayList<>();
            component.add(node);
            contracted.put(node, component);
        }
        
        Random rand = new Random();
        
        // Contract edges until we have two components left
        while (contracted.size() > 2) {
            if (tempEdges.isEmpty()) break;
            
            // Pick a random edge
            Edge edge = tempEdges.remove(rand.nextInt(tempEdges.size()));
            String u = edge.u;
            String v = edge.v;
            
            // Skip if u and v are already in the same component
            if (contracted.get(u) == contracted.get(v)) {
                continue;
            }
            
            // Merge the components of u and v
            List<String> componentU = contracted.get(u);
            List<String> componentV = contracted.get(v);
            componentU.addAll(componentV);
            
            // Update all nodes in componentV to point to componentU
            for (String node : componentV) {
                contracted.put(node, componentU);
            }
        }
        
        // Count the number of edges between the two remaining components
        int cutSize = 0;
        for (Edge e : edges) {
            if (contracted.get(e.u) != contracted.get(e.v)) {
                cutSize++;
            }
        }
        
        // Return the cut size and the sizes of the two components
        Set<List<String>> uniqueComponents = new HashSet<>(contracted.values());
        if (uniqueComponents.size() != 2) {
            return new int[]{Integer.MAX_VALUE, 0, 0};
        }
        
        Iterator<List<String>> it = uniqueComponents.iterator();
        int size1 = it.next().size();
        int size2 = it.next().size();
        
        return new int[]{cutSize, size1, size2};
    }

    private static void parseInput(String filename) {
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;
                
                String[] parts = line.split(":");
                String component = parts[0].trim();
                String[] connections = parts[1].trim().split("\\s+");
                
                graph.putIfAbsent(component, new HashSet<>());
                for (String conn : connections) {
                    graph.putIfAbsent(conn, new HashSet<>());
                    graph.get(component).add(conn);
                    graph.get(conn).add(component);
                    edges.add(new Edge(component, conn));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
