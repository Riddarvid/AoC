package AoC.day22.spells;

import AoC.day22.Unit;

public class Drain {
    public static int cast(Unit caster, Unit target) {
        caster.heal(2);
        target.takeDamage(2);
        return 73;
    }
}
