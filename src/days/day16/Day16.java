package days.day16;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.math.MathUtils;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.*;

public class Day16 extends Day {
    private List<Rule> rules;
    private List<Ticket> nearbyTickets;
    private Ticket myTicket;

    public static void main(String[] args) {
        new Day16().runAndPrint();
    }

    @Override
    public long part1() {
        return getErrorRate(nearbyTickets, rules);
    }

    private long getErrorRate(List<Ticket> nearbyTickets, List<Rule> rules) {
        List<Integer> errors = new ArrayList<>();
        for (Ticket ticket : nearbyTickets) {
            errors.addAll(ticket.getErrors(rules));
        }
        return MathUtils.sum(errors);
    }

    @Override
    public long part2() {
        List<Ticket> validTickets = getValidTickets(nearbyTickets);
        Map<Integer, String> indexToField = mapIndexesToFields(validTickets, myTicket.numberOfFields());
        long product = 1;
        for (int i = 0; i < myTicket.numberOfFields(); i++) {
            if (indexToField.get(i).startsWith("departure")) {
                product *= myTicket.getValue(i);
            }
        }
        return product;
    }

    private List<Ticket> getValidTickets(List<Ticket> nearbyTickets) {
        List<Ticket> validTickets = new ArrayList<>();
        for (Ticket ticket : nearbyTickets) {
            if (ticket.isValid(rules)) {
                validTickets.add(ticket);
            }
        }
        return validTickets;
    }

    private Map<Integer, String> mapIndexesToFields(List<Ticket> validTickets, int numberOfFields) {
        Map<Integer, List<String>> indexToFields = new HashMap<>();
        for (int i = 0; i < numberOfFields; i++) {
            indexToFields.put(i, getFields(i, validTickets));
        }
        Map<Integer, String> indexToField = new HashMap<>();
        while (indexToField.size() < myTicket.numberOfFields()) {
            Set<String> mappedThisRound = new HashSet<>();
            for (int index : indexToFields.keySet()) {
                if (indexToFields.get(index).size() == 1) {
                    indexToField.put(index, indexToFields.get(index).get(0));
                    mappedThisRound.add(indexToField.get(index));
                }
            }
            removeAlreadyMapped(indexToFields, mappedThisRound);
        }
        return indexToField;
    }

    private void removeAlreadyMapped(Map<Integer, List<String>> indexToFields, Set<String> mappedThisRound) {
        for (int index : indexToFields.keySet()) {
            indexToFields.get(index).removeAll(mappedThisRound);
        }
    }

    private List<String> getFields(int i, List<Ticket> validTickets) {
        List<String> fields = new ArrayList<>();
        for (Rule rule : rules) {
            if (allIsValid(rule, i, validTickets)) {
                fields.add(rule.getField());
            }
        }
        return fields;
    }

    private boolean allIsValid(Rule rule, int i, List<Ticket> validTickets) {
        for (Ticket ticket : validTickets) {
            if (!rule.isValid(ticket.getValue(i))) {
                return false;
            }
        }
        return true;
    }

    @Override
    public void setup() {
        int i = 0;
        rules = new ArrayList<>();
        while (!lines.get(i).equals("")) {
            List<String> tokens = ParsingUtils.getTokens(lines.get(i), ':');
            List<Range> ranges = parseRanges(tokens.get(1));
            rules.add(new Rule(tokens.get(0), ranges));
            i++;
        }
        i += 2;
        myTicket = parseTicket(lines.get(i));
        i += 3;
        nearbyTickets = new ArrayList<>();
        while (i < lines.size()) {
            nearbyTickets.add(parseTicket(lines.get(i)));
            i++;
        }
    }

    private Ticket parseTicket(String string) {
        return new Ticket(ParsingUtils.getIntegers(string));
    }

    private List<Range> parseRanges(String string) {
        List<Integer> values = ParsingUtils.getIntegers(string);
        List<Range> ranges = new ArrayList<>();
        for (int i = 0; i < values.size(); i += 2) {
            ranges.add(new Range(values.get(i), values.get(i + 1)));
        }
        return ranges;
    }
}
