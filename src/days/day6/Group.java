package days.day6;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Group {
    private List<String> answers = new ArrayList<>();

    public void addAnswer(String answer) {
        answers.add(answer);
    }

    public int getNumberOfAnswers() {
        return getUniqueAnswers().size();
    }

    private Set<Character> getUniqueAnswers() {
        Set<Character> uniqueAnswers = new HashSet<>();
        for (String answer : answers) {
            for (char c : answer.toCharArray()) {
                uniqueAnswers.add(c);
            }
        }
        return uniqueAnswers;
    }

    public int getNumberEveryone() {
        String s = answers.get(0);
        int sum = 0;
        for (char c : s.toCharArray()) {
            boolean allContain = true;
            for (String answer : answers) {
                if (!answer.contains("" + c)) {
                    allContain = false;
                    break;
                }
            }
            if (allContain) {
                sum++;
            }
        }
        return sum;
    }
}
