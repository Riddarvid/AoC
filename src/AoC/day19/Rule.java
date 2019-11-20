package AoC.day19;

public class Rule implements Comparable<Rule> {
    private final String key;
    private final String value;

    public Rule(String key, String value) {
        this.key = key;
        this.value = value;
    }

    public String getKey() {
        return key;
    }

    public String getValue() {
        return value;
    }

    @Override
    public String toString() {
        return key + " => " + value;
    }

    @Override
    public int compareTo(Rule o) {
        return value.length() - o.value.length();
    }
}
