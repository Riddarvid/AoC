package AoC.Day24;

import java.util.List;
import java.util.Objects;

class Group {
    private Faction faction;
    private int numberOfUnits;
    private final int hitPoints;
    private final List<DamageType> immunities;
    private final List<DamageType> weaknesses;
    private final int attackDamage;
    private final DamageType damageType;
    private final int initiative;
    private Group target;
    private final int id;

    Group(Faction faction, int numberOfUnits, int hitPoints, List<DamageType> immunities, List<DamageType> weaknesses, int attackDamage, DamageType damageType, int initiative, int id) {
        this.faction = faction;
        this.numberOfUnits = numberOfUnits;
        this.hitPoints = hitPoints;
        this.immunities = immunities;
        this.weaknesses = weaknesses;
        this.attackDamage = attackDamage;
        this.damageType = damageType;
        this.initiative = initiative;
        this.id = id;
    }

    int getEffectiveAttackDamage() {
        return numberOfUnits * attackDamage;
    }

    public Faction getFaction() {
        return faction;
    }

    public void setFaction(Faction faction) {
        this.faction = faction;
    }

    public int getNumberOfUnits() {
        return numberOfUnits;
    }

    public void setNumberOfUnits(int numberOfUnits) {
        this.numberOfUnits = numberOfUnits;
    }

    public int getHitPoints() {
        return hitPoints;
    }

    List<DamageType> getImmunities() {
        return immunities;
    }

    List<DamageType> getWeaknesses() {
        return weaknesses;
    }

    public int getAttackDamage() {
        return attackDamage;
    }

    public DamageType getDamageType() {
        return damageType;
    }

    int getInitiative() {
        return initiative;
    }

    public Group getTarget() {
        return target;
    }

    public void setTarget(Group target) {
        this.target = target;
    }

    public int getId() {
        return id;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (faction == Faction.IMMUNE_SYSTEM) {
            sb.append("Immune System ");
        } else {
            sb.append("Infection ");
        }
        sb.append("group ");
        sb.append(id);
        sb.append(" containing ");
        sb.append(numberOfUnits).append(" units with ");
        sb.append(hitPoints).append(" HP. ");
        sb.append("Group is weak to ");
        if (weaknesses.isEmpty()) {
            sb.append("nothing ");
        }
        for (DamageType damageType : weaknesses) {
            sb.append(damageType).append(", ");
        }
        sb.append(" and immune to ");
        if (immunities.isEmpty()) {
            sb.append("nothing ");
        }
        for (DamageType damageType : immunities) {
            sb.append(damageType).append(", ");
        }
        sb.append(". The units deal ");
        sb.append(attackDamage).append(" ").append(damageType).append(" damageAmount, with an effective attack power of ");
        sb.append(getEffectiveAttackDamage()).append(".");
        return sb.toString();
    }

    public void target(List<Group> potentialTargets) {
        Group bestTarget = potentialTargets.get(0);
        for (Group group : potentialTargets) {
            if (Day24.debug) {
                System.out.println(faction + " group " + id + " would deal defending group " + group.getId() + " " + damageAmount(group) + " damage");
            }
            if (damageAmount(group) > damageAmount(bestTarget)) {
                bestTarget = group;
            } else if (damageAmount(group) == damageAmount(bestTarget)) {
                if (group.getEffectiveAttackDamage() > bestTarget.getEffectiveAttackDamage()) {
                    bestTarget = group;
                } else if (group.getEffectiveAttackDamage() == bestTarget.getEffectiveAttackDamage()) {
                    if (group.getInitiative() > bestTarget.getInitiative()) {
                        bestTarget = group;
                    }
                }
            }
        }
        if (damageAmount(bestTarget) == 0) {
            target = null;
        } else {
            target = bestTarget;
            potentialTargets.remove(target);
        }
    }

    private int damageAmount(Group target) {
        if (target.getImmunities().contains(damageType)) {
            return 0;
        }
        if (target.getWeaknesses().contains(damageType)) {
            return getEffectiveAttackDamage() * 2;
        }
        return getEffectiveAttackDamage();
    }

    void attack() {
        if (target != null && numberOfUnits > 0) {
            int damage = damageAmount(target);
            target.setNumberOfUnits(target.getNumberOfUnits() - damage / target.getHitPoints());
        }
        target = null;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Group group = (Group) o;
        return numberOfUnits == group.numberOfUnits &&
                hitPoints == group.hitPoints &&
                attackDamage == group.attackDamage &&
                initiative == group.initiative &&
                id == group.id &&
                faction == group.faction &&
                Objects.equals(immunities, group.immunities) &&
                Objects.equals(weaknesses, group.weaknesses) &&
                damageType == group.damageType &&
                Objects.equals(target, group.target);
    }

    @Override
    public int hashCode() {
        return Objects.hash(faction, numberOfUnits, hitPoints, immunities, weaknesses, attackDamage, damageType, initiative, target, id);
    }

    Group getCopy() {
        return new Group(faction, numberOfUnits, hitPoints, immunities, weaknesses, attackDamage, damageType, initiative, id);
    }
}
