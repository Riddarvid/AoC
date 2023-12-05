package aoc15.days.day19;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

public class Molecule {
    private final List<Atom> composition;

    public Molecule(String composition) {
        this.composition = Atom.stringToAtoms(composition);
    }

    public Molecule(List<Atom> composition) {
        this.composition = new LinkedList<>(composition);
    }

    public int numberOfAtoms() {
        return composition.size();
    }

    public Atom getAtom(int i) {
        return composition.get(i);
    }

    public Molecule replace(int i, Molecule to) {
        Molecule molecule = new Molecule(composition);
        molecule.composition.remove(i);
        molecule.composition.addAll(i, new ArrayList<>(to.composition));
        return molecule;
    }

    public Molecule replace(int i, int numberOfAtoms, Atom from) {
        Molecule molecule = new Molecule(composition);
        for (int j = 0; j < numberOfAtoms; j++) {
            molecule.composition.remove(i);
        }
        molecule.composition.add(i, from);
        return molecule;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Molecule molecule = (Molecule) o;
        return Objects.equals(composition, molecule.composition);
    }

    @Override
    public int hashCode() {
        return Objects.hash(composition);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (Atom atom : composition) {
            sb. append(atom);
        }
        return sb.toString();
    }

    public Molecule getSubMolecule(int i, int numberOfAtoms) {
        return new Molecule(composition.subList(i, i + numberOfAtoms));
    }
}
