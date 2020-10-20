package AoC.day19;

import java.util.Comparator;

public class MoleculeComparator implements Comparator<Molecule> {
    @Override
    public int compare(Molecule m1, Molecule m2) {
        return m1.numberOfAtoms() - m2.numberOfAtoms();
    }
}
