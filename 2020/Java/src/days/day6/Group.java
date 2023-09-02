package days.day6;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Group {
    private List<Set<Character>> answers = new ArrayList<>();

    public void addAnswer(String answerString) {
        Set<Character> answer = new HashSet<>();
        for (char c : answerString.toCharArray()) {
            answer.add(c);
        }
        addAnswer(answer);
    }

    public void addAnswer(Set<Character> answer) {
        answers.add(answer);
    }

    public int getNumberOfAnswers() {
        return getUniqueAnswers().size();
    }

    private Set<Character> getUniqueAnswers() {
        Set<Character> uniqueAnswers = new HashSet<>();
        for (Set<Character> answer : answers) {
            uniqueAnswers.addAll(answer);
        }
        return uniqueAnswers;
    }

    public int getNumberEveryone() {
        Set<Character> baseAnswer = answers.get(0);
        int sum = 0;
        for (char c : baseAnswer) {
            boolean allContain = true;
            for (Set<Character> answer : answers) {
                if (!answer.contains(c)) {
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
