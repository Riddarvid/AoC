package days.day16;

import java.util.ArrayList;
import java.util.List;

public class Ticket {
    private final List<Integer> values;

    public Ticket(List<Integer> values) {
        this.values = values;
    }

    public List<Integer> getErrors(List<Rule> rules) {
        List<Integer> errors = new ArrayList<>();
        for (int value : values) {
            if (!isValueValid(value, rules)) {
                errors.add(value);
            }
        }
        return errors;
    }

    private static boolean isValueValid(int value, List<Rule> rules) {
        for (Rule rule : rules) {
            if (rule.isValid(value)) {
                return true;
            }
        }
        return false;
    }

    public int numberOfFields() {
        return values.size();
    }

    public int getValue(int i) {
        return values.get(i);
    }

    public boolean isValid(List<Rule> rules) {
        for (int value : values) {
            if (!isValueValid(value, rules)) {
                return false;
            }
        }
        return true;
    }
}
