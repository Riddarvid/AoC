package aoc.days.day9;

import java.util.ArrayList;
import java.util.List;

public class Group {
    private List<Group> children;
    private static int garbageAmount = 0;

    public Group() {
        children = new ArrayList<>();
    }

    public int init(String string, int index) {
        boolean garbage = false;
        while (string.charAt(index) != '}' || garbage) {
            char c = string.charAt(index);
            switch (c) {
                case '<':
                    if (garbage) {
                        garbageAmount++;
                    } else {
                        garbage = true;
                    }
                    index++;
                    break;
                case '>':
                    garbage = false;
                    index++;
                    break;
                case '{':
                    if (garbage) {
                        garbageAmount++;
                        index++;
                        break;
                    }
                    Group child = new Group();
                    children.add(child);
                    index++;
                    index = child.init(string, index);
                    break;
                case '!':
                    index += 2;
                    break;
                default:
                    if (garbage) {
                        garbageAmount++;
                    }
                    index++;
                    break;
            }
        }
        return index + 1;
    }

    public int getScore(int ownScore) {
        int score = ownScore;
        for (Group child : children) {
            score += child.getScore(ownScore + 1);
        }
        return score;
    }

    public static int getGarbageAmount() {
        return garbageAmount;
    }
}
