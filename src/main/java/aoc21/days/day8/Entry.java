package aoc21.days.day8;

import aoc.combinatorics.CombinatoricsUtils;

import java.util.*;

public class Entry {
    private final static Map<Set<Integer>, Integer> segmentsToNumber = new HashMap<>();

    static {
        Set<Integer> segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(4);
        segmentsSet.add(5);
        segmentsToNumber.put(segmentsSet, 0);

        segmentsSet = new HashSet<>();
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsToNumber.put(segmentsSet, 1);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(3);
        segmentsSet.add(4);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 2);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 3);

        segmentsSet = new HashSet<>();
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsSet.add(5);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 4);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(5);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 5);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(4);
        segmentsSet.add(5);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 6);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsToNumber.put(segmentsSet, 7);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(4);
        segmentsSet.add(5);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 8);

        segmentsSet = new HashSet<>();
        segmentsSet.add(0);
        segmentsSet.add(1);
        segmentsSet.add(2);
        segmentsSet.add(3);
        segmentsSet.add(5);
        segmentsSet.add(6);
        segmentsToNumber.put(segmentsSet, 9);
    }

    private final Set<Set<Character>> numbersSet;
    private final List<String> allValues;
    private final List<String> output;

    public Entry(List<String> allValues, List<String> output) {
        this.numbersSet = new HashSet<>();
        this.allValues = new ArrayList<>(allValues);
        for (String value : allValues) {
            Set<Character> characterSet = new HashSet<>();
            for (char c : value.toCharArray()) {
                characterSet.add(c);
            }
            this.numbersSet.add(characterSet);
        }
        this.output = new ArrayList<>(output);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (String value : allValues) {
            sb.append(value).append(' ');
        }
        sb.append('|');
        for (String value : output) {
            sb.append(' ').append(value);
        }
        return sb.toString();
    }

    public int getNumberOfEasyOutputs() {
        int sum = 0;
        for (String value : output) {
            int length = value.length();
            if (length == 2 || length == 3 || length == 4 || length == 7) {
                sum++;
            }
        }
        return sum;
    }

    public List<Character> findMapping() {
        List<Character> elements = new ArrayList<>();
        elements.add('a');
        elements.add('b');
        elements.add('c');
        elements.add('d');
        elements.add('e');
        elements.add('f');
        elements.add('g');
        List<List<Character>> possibleMappings = CombinatoricsUtils.getPermutations(elements);
        for (List<Character> mapping : possibleMappings) {
            if (doesMappingWork(mapping)) {
                return mapping;
            }
        }
        throw new IllegalArgumentException("No mapping found");
    }

    private boolean doesMappingWork(List<Character> mapping) {
        Set<Set<Character>> targetSet = new HashSet<>();

        Set<Character> numberSegments = new HashSet<>();//0
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(4));
        numberSegments.add(mapping.get(5));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//1
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//2
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(4));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//3
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//4
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(5));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//5
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(5));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//6
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(4));
        numberSegments.add(mapping.get(5));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//7
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//8
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(4));
        numberSegments.add(mapping.get(5));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        numberSegments = new HashSet<>();//9
        numberSegments.add(mapping.get(0));
        numberSegments.add(mapping.get(1));
        numberSegments.add(mapping.get(2));
        numberSegments.add(mapping.get(3));
        numberSegments.add(mapping.get(5));
        numberSegments.add(mapping.get(6));
        targetSet.add(numberSegments);

        return targetSet.equals(numbersSet);
    }

    public int getOutputValue() {
        List<Character> mapping = findMapping();
        Map<Character, Integer> charToIntMap = new HashMap<>();
        for (int i = 0; i < mapping.size(); i++) {
            charToIntMap.put(mapping.get(i), i);
        }
        StringBuilder outputValue = new StringBuilder();
        for (String outputString : output) {
            Set<Integer> segments = new HashSet<>();
            for (char c : outputString.toCharArray()) {
                segments.add(charToIntMap.get(c));
            }
            outputValue.append(segmentsToNumber.get(segments));
        }
        return Integer.parseInt(outputValue.toString());
    }
}
