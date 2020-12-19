package days.day19;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MultiRule {
    private final static Map<Integer, MultiRule> multiRuleMap = new HashMap<>();

    private final int id;
    private List<SingleRule> subRules;

    public MultiRule(int id) {
        this.id = id;
        multiRuleMap.put(id, this);
    }

    public int getId() {
        return id;
    }

    public void setSubRules(List<SingleRule> subRules) {
        this.subRules = subRules;
    }

    public static MultiRule getMultiRule(int id) {
        return multiRuleMap.get(id);
    }

    public boolean matches(String message) {
        return !getMatchesWrapper(message).isEmpty();
    }

    private List<String> getMatchesWrapper(String message) {
        List<String> matches = getMatches(message, 0);
        List<String> completelyMatched = new ArrayList<>();
        for (String match : matches) {
            if (match.length() == message.length()) {
                completelyMatched.add(match);
            }
        }
        return completelyMatched;
    }

    public List<String> getMatches(String message, int fromIndex) {
        List<String> matches = new ArrayList<>();
        for (SingleRule subRule : subRules) {
            matches.addAll(subRule.getMatches(message, fromIndex));
        }
        return matches;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(id).append(": ");
        for (SingleRule subRule : subRules) {
            sb.append(subRule.toString()).append(" | ");
        }
        sb.delete(sb.length() - 3, sb.length());
        return sb.toString();
    }
}
