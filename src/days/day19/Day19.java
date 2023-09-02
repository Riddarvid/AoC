package days.day19;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day19 extends Day {
    private List<MultiRule> multiRules;
    private List<String> messages;

    public static void main(String[] args) {
        new Day19().runAndPrint();
    }

    @Override
    public long part1() {
        MultiRule ruleZero = MultiRule.getMultiRule(0);
        int numberOfMatches = 0;
        for (String message : messages) {
            if (ruleZero.matches(message)) {
                numberOfMatches++;
            }
        }
        return numberOfMatches;
    }

    @Override
    public long part2() {
        parseMultiRule("8: 42 | 42 8");
        parseMultiRule("11: 42 31 | 42 11 31");
        MultiRule ruleZero = MultiRule.getMultiRule(0);
        int numberOfMatches = 0;
        for (String message : messages) {
            if (ruleZero.matches(message)) {
                numberOfMatches++;
            }
        }
        return numberOfMatches;
    }

    @Override
    public void setup() {
        int index = 0;
        multiRules = new ArrayList<>();
        while (!lines.get(index).equals("")) {
            List<Integer> integers = ParsingUtils.getIntegers(lines.get(index));
            multiRules.add(new MultiRule(integers.get(0)));
            index++;
        }
        index = 0;
        while (!lines.get(index).equals("")) {
            parseMultiRule(lines.get(index));
            index++;
        }
        index++;
        messages = new ArrayList<>();
        while (index < lines.size()) {
            messages.add(lines.get(index));
            index++;
        }
    }

    private void parseMultiRule(String line) {
        List<Integer> integers = ParsingUtils.getIntegers(line);
        int id = integers.get(0);
        MultiRule multiRule = MultiRule.getMultiRule(id);
        int i = 0;
        while (line.charAt(i) != ':') {
            i++;
        }
        line = line.substring(i + 1);
        List<SingleRule> subRules = parseSingleRules(line);
        multiRule.setSubRules(subRules);
    }

    private List<SingleRule> parseSingleRules(String line) {
        List<String> tokens = ParsingUtils.getTokens(line, '|');
        List<SingleRule> singleRules = new ArrayList<>();
        for (String token : tokens) {
            singleRules.add(parseSingleRule(token));
        }
        return singleRules;
    }

    private SingleRule parseSingleRule(String line) {
        line = line.strip();
        if (line.charAt(0) == '"') {
            return new CharRule(line.charAt(1));
        }
        List<Integer> integers = ParsingUtils.getIntegers(line);
        return new ComposedRule(integers);
    }
}
