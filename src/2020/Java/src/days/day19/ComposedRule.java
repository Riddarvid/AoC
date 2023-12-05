package days.day19;

import java.util.ArrayList;
import java.util.List;

public class ComposedRule implements SingleRule {
    private final List<MultiRule> pattern;

    public ComposedRule(List<Integer> ids) {
        pattern = new ArrayList<>();
        for (int id : ids) {
            pattern.add(MultiRule.getMultiRule(id));
        }
    }

    @Override
    public List<String> getMatches(String message, int fromIndex) {
        return getMatches(message, fromIndex, 0);
    }

    private List<String> getMatches(String message, int fromIndex, int patternIndex) {
        List<String> matches = new ArrayList<>();
        if (patternIndex == pattern.size()) {
            if (fromIndex <= message.length()) {
                matches.add("");
            }
            return matches;
        }
        if (fromIndex >= message.length()) {
            return matches;
        }
        List<String> subMatches = pattern.get(patternIndex).getMatches(message, fromIndex);
        for (String subMatch : subMatches) {
            List<String> nextMatches = getMatches(message, fromIndex + subMatch.length(), patternIndex + 1);
            for (String nextMatch : nextMatches) {
                matches.add(subMatch + nextMatch);
            }
        }
        return matches;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (MultiRule multiRule : pattern) {
            sb.append(multiRule.getId()).append(" ");
        }
        sb.deleteCharAt(sb.length() - 1);
        return sb.toString();
    }
}
