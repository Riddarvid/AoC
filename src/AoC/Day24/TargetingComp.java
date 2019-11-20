package AoC.Day24;

import java.util.Comparator;

public class TargetingComp implements Comparator<Group> {
    @Override
    public int compare(Group o1, Group o2) {
        int score = 2 * (o2.getEffectiveAttackDamage() - o1.getEffectiveAttackDamage());
        if (o2.getInitiative() > o1.getInitiative()) {
            score++;
        }
        return score;
    }
}
