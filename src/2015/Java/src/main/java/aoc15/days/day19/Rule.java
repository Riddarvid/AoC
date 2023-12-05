package aoc15.days.day19;

import java.util.HashSet;
import java.util.Set;

public class Rule {
    private final Atom from;
    private final Molecule to;

    public Rule(Atom from, Molecule to) {
        this.from = from;
        this.to = to;
    }

    public Rule(String from, String to) {
        this.from = new Atom(from);
        this.to = new Molecule(to);
    }

    public Set<Molecule> apply(Molecule molecule) {
        Set<Molecule> replacements = new HashSet<>();
        for (int i = 0; i < molecule.numberOfAtoms(); i++) {
            if (from.equals(molecule.getAtom(i))) {
                replacements.add(molecule.replace(i, to));
            }
        }
        return replacements;
    }

    public Set<Molecule> applyBackwards(Molecule molecule) {
        Set<Molecule> replacements = new HashSet<>();
        for (int i = 0; i < molecule.numberOfAtoms() - to.numberOfAtoms() + 1; i++) {
            if (to.equals(molecule.getSubMolecule(i, to.numberOfAtoms()))) {
                replacements.add(molecule.replace(i, to.numberOfAtoms(), from));
            }
        }
        return replacements;
    }

    @Override
    public String toString() {
        return from.toString() + " => " + to.toString();
    }
}
