package AoC.Day15.FightModel;

import java.util.Comparator;

public class ReadComparator implements Comparator<IPositionable> {
    @Override
    public int compare(IPositionable o1, IPositionable o2) {
        return (o1.getY() * 100 + o1.getX()) - (o2.getY() * 100 + o2.getX());
    }
}
