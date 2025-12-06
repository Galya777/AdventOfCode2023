package Day19;

public class Rule {
    private final Character category;
    private final char operator;
    private final int value;
    private final String destination;
    private final boolean isDefault;

    public Rule(String ruleStr) {
        if (!ruleStr.contains(":")) {
            this.isDefault = true;
            this.destination = ruleStr;
            this.category = null;
            this.operator = ' ';
            this.value = 0;
        } else {
            this.isDefault = false;
            String[] parts = ruleStr.split(":");
            this.destination = parts[1];
            this.category = parts[0].charAt(0);
            this.operator = parts[0].charAt(1);
            this.value = Integer.parseInt(parts[0].substring(2));
        }
    }

    public boolean matches(Part part) {
        if (isDefault) return true;
        
        int partValue = switch (category) {
            case 'x' -> part.x();
            case 'm' -> part.m();
            case 'a' -> part.a();
            case 's' -> part.s();
            default -> throw new IllegalArgumentException("Unknown category: " + category);
        };

        return switch (operator) {
            case '>' -> partValue > value;
            case '<' -> partValue < value;
            default -> throw new IllegalArgumentException("Unknown operator: " + operator);
        };
    }

    public String getDestination() {
        return destination;
    }

    public boolean isDefault() {
        return isDefault;
    }
}
