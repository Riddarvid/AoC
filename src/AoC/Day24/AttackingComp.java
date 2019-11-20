package AoC.Day24;

import java.util.Comparator;

public class AttackingComp implements Comparator<Group> {
    @Override
    public int compare(Group o1, Group o2) {
        return o2.getInitiative() - o1.getInitiative();
    }
}
