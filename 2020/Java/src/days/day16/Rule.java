package days.day16;

import java.util.List;

public class Rule {
    private final String field;
    private final List<Range> ranges;

    public Rule(String field, List<Range> ranges) {
        this.field = field;
        this.ranges = ranges;
    }

    public boolean isValid(int value) {
        for (Range range : ranges) {
            if (range.contains(value)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(field).append(": ");
        for (Range range : ranges) {
            sb.append(range).append(" or ");
        }
        sb.delete(sb.length() - 4, sb.length());
        return sb.toString();
    }

    public String getField() {
        return field;
    }
}
