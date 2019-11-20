package AoC.day22.spells;

import AoC.day22.Unit;

public class MagicMissile {
    public static int cast(Unit target) {
        target.takeDamage(4);
        return 53;
    }
}
