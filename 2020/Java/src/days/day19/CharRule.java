package days.day19;

import java.util.ArrayList;
import java.util.List;

public class CharRule implements SingleRule {
    private final char letter;

    public CharRule(char letter) {
        this.letter = letter;
    }

    @Override
    public List<String> getMatches(String message, int fromIndex) {
        List<String> matches = new ArrayList<>();
        if (message.charAt(fromIndex) == letter) {
            matches.add("" + letter);
        }
        return matches;
    }

    @Override
    public String toString() {
        return "\"" + letter + "\"";
    }
}
