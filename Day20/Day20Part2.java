package Day20;

import java.io.*;
import java.util.*;

public class Day20Part2 {
    private static abstract class Module {
        String name;
        List<String> destinations;

        Module(String name, List<String> destinations) {
            this.name = name;
            this.destinations = destinations;
        }

        abstract List<Pulse> processPulse(Pulse pulse, String source);
    }

    private static class Broadcaster extends Module {
        Broadcaster(String name, List<String> destinations) {
            super(name, destinations);
        }

        @Override
        List<Pulse> processPulse(Pulse pulse, String source) {
            List<Pulse> result = new ArrayList<>();
            for (String dest : destinations) {
                result.add(new Pulse(pulse.type, name, dest));
            }
            return result;
        }
    }

    private static class FlipFlop extends Module {
        boolean isOn;

        FlipFlop(String name, List<String> destinations) {
            super(name, destinations);
            this.isOn = false;
        }

        @Override
        List<Pulse> processPulse(Pulse pulse, String source) {
            if (pulse.type == PulseType.HIGH) {
                return Collections.emptyList();
            }

            List<Pulse> result = new ArrayList<>();
            PulseType sendType = isOn ? PulseType.LOW : PulseType.HIGH;
            isOn = !isOn;

            for (String dest : destinations) {
                result.add(new Pulse(sendType, name, dest));
            }
            return result;
        }
    }

    private static class Conjunction extends Module {
        Map<String, PulseType> memory = new HashMap<>();

        Conjunction(String name, List<String> destinations) {
            super(name, destinations);
        }

        void addInput(String input) {
            memory.put(input, PulseType.LOW);
        }

        @Override
        List<Pulse> processPulse(Pulse pulse, String source) {
            memory.put(source, pulse.type);

            PulseType sendType = PulseType.LOW;
            for (PulseType type : memory.values()) {
                if (type != PulseType.HIGH) {
                    sendType = PulseType.HIGH;
                    break;
                }
            }

            List<Pulse> result = new ArrayList<>();
            for (String dest : destinations) {
                result.add(new Pulse(sendType, name, dest));
            }
            return result;
        }
    }

    private enum PulseType {
        LOW, HIGH
    }

    private static class Pulse {
        PulseType type;
        String from;
        String to;

        Pulse(PulseType type, String from, String to) {
            this.type = type;
            this.from = from;
            this.to = to;
        }
    }

    private static long gcd(long a, long b) {
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return a;
    }

    private static long lcm(long a, long b) {
        return (a * b) / gcd(a, b);
    }

    public static void main(String[] args) throws IOException {
        Map<String, Module> modules = new HashMap<>();
        Map<String, List<String>> inputMap = new HashMap<>();

        // Read input
        try (BufferedReader br = new BufferedReader(new FileReader("Day20/input.txt"))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                String[] parts = line.split(" -> ");
                if (parts.length < 2) {
                    System.err.println("Invalid line format: " + line);
                    continue;
                }

                String name = parts[0];
                List<String> destinations = Arrays.asList(parts[1].split(",\\s*"));

                if (name.equals("broadcaster")) {
                    modules.put(name, new Broadcaster(name, destinations));
                } else if (name.startsWith("%")) {
                    String moduleName = name.substring(1);
                    modules.put(moduleName, new FlipFlop(moduleName, destinations));
                } else if (name.startsWith("&")) {
                    String moduleName = name.substring(1);
                    modules.put(moduleName, new Conjunction(moduleName, destinations));
                }
            }
        }

        // Initialize conjunction memories
        for (Map.Entry<String, Module> entry : modules.entrySet()) {
            String moduleName = entry.getKey();
            Module module = entry.getValue();
            for (String dest : module.destinations) {
                Module destModule = modules.get(dest);
                if (destModule instanceof Conjunction) {
                    ((Conjunction) destModule).addInput(moduleName);
                }
            }
        }

        // Find the parent of "rx" (should be a conjunction)
        String rxParent = null;
        for (Map.Entry<String, Module> entry : modules.entrySet()) {
            if (entry.getValue().destinations.contains("rx")) {
                rxParent = entry.getKey();
                break;
            }
        }

        if (rxParent == null) {
            System.err.println("No parent found for rx");
            return;
        }

        // Find all modules that feed into rxParent
        List<String> rxParentInputs = new ArrayList<>();
        for (Map.Entry<String, Module> entry : modules.entrySet()) {
            if (entry.getValue().destinations.contains(rxParent)) {
                rxParentInputs.add(entry.getKey());
            }
        }

        // Track the first time each input sends a high pulse to rxParent
        Map<String, Long> highPulseTimes = new HashMap<>();
        long buttonPresses = 0;

        while (highPulseTimes.size() < rxParentInputs.size()) {
            buttonPresses++;
            Queue<Pulse> queue = new LinkedList<>();
            queue.add(new Pulse(PulseType.LOW, "button", "broadcaster"));

            while (!queue.isEmpty()) {
                Pulse current = queue.poll();

                // Check if this pulse is a high pulse to rxParent
                if (current.to.equals(rxParent) && current.type == PulseType.HIGH) {
                    if (!highPulseTimes.containsKey(current.from)) {
                        highPulseTimes.put(current.from, buttonPresses);
                        System.out.println("Module " + current.from + " sent high to " + rxParent + " at button press " + buttonPresses);

                        // If we've found all cycles, calculate the LCM
                        if (highPulseTimes.size() == rxParentInputs.size()) {
                            long result = 1;
                            for (long cycle : highPulseTimes.values()) {
                                result = lcm(result, cycle);
                            }
                            System.out.println("Minimum button presses required: " + result);
                            return;
                        }
                    }
                }

                Module module = modules.get(current.to);
                if (module == null) {
                    continue;
                }

                List<Pulse> newPulses = module.processPulse(current, current.from);
                queue.addAll(newPulses);
            }

            // Safety check to prevent infinite loops
            if (buttonPresses > 100000) {
                System.err.println("Exceeded maximum button presses");
                break;
            }
        }
    }
}