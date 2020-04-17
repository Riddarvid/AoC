package aoc.days.day7;

import aoc.utils.input.InputUtils;

import java.util.ArrayList;
import java.util.List;

public class Address {
    private final String value;
    private final List<String> hypernetSequences;
    private final List<String> supernetSequences;

    public Address(String value) {
        this.value = value;
        hypernetSequences = new ArrayList<>();
        supernetSequences = new ArrayList<>();
        List<String> tokens = InputUtils.getTokens(value, "[");
        if (tokens.get(0).charAt(0) != '[') {
            supernetSequences.add(tokens.get(0));
            tokens.remove(0);
        }
        for (String string : tokens) {
            String hypernetSequence = string.substring(0, string.indexOf(']'));
            if (!hypernetSequence.isEmpty()) {
                hypernetSequences.add(hypernetSequence);
            }
            String supernetSequence = string.substring(string.indexOf(']') + 1);
            if (!supernetSequence.isEmpty()) {
                supernetSequences.add(supernetSequence);
            }
        }
    }

    public boolean supportsSSL() {
        for (String supernetSequence : supernetSequences) {
            if (supportsSSL(supernetSequence)) {
                return true;
            }
        }
        return false;
    }

    private boolean supportsSSL(String supernetSequence) {
        for (int i = 0; i < supernetSequence.length() - 2; i++) {
            if (isAba(supernetSequence, i)) {
                if (containsBab(supernetSequence.substring(i, i + 3))) {
                    return true;
                }
            }
        }
        return false;
    }

    private boolean containsBab(String aba) {
        String bab = "" + aba.charAt(1) + aba.charAt(0) + aba.charAt(1);
        for (String hypernetSequence : hypernetSequences) {
            if (hypernetSequence.contains(bab)) {
                return true;
            }
        }
        return false;
    }

    public boolean supportsTLS() {
        return containsAbba(supernetSequences) && !containsAbba(hypernetSequences);
    }

    private boolean containsAbba(List<String> sequences) {
        for (String sequence : sequences) {
            if (containsAbba(sequence)) {
                return true;
            }
        }
        return false;
    }

    private boolean containsAbba(String value) {
        for (int i = 0; i < value.length() - 3; i++) {
            if (isAbba(value, i)) {
                return true;
            }
        }
        return false;
    }

    private boolean isAba(String value, int index) {
        return value.charAt(index) == value.charAt(index + 2) && value.charAt(index) != value.charAt(index + 1);
    }

    private boolean isAbba(String value, int index) {
        return value.charAt(index) == value.charAt(index + 3) && value.charAt(index + 1) == value.charAt(index + 2) && value.charAt(index) != value.charAt(index + 1);
    }
}
