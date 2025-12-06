package Day19;

import java.util.ArrayList;
import java.util.List;

public class Workflow {
    private final String name;
    private final List<Rule> rules;

    public Workflow(String name, String rulesStr) {
        this.name = name;
        this.rules = new ArrayList<>();
        
        String[] ruleParts = rulesStr.split(",");
        for (String ruleStr : ruleParts) {
            rules.add(new Rule(ruleStr));
        }
    }

    public String process(Part part) {
        for (Rule rule : rules) {
            if (rule.matches(part)) {
                return rule.getDestination();
            }
        }
        throw new IllegalStateException("No matching rule found for part in workflow: " + name);
    }

    public String getName() {
        return name;
    }
}
