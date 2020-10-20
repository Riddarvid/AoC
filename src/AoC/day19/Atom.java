package AoC.day19;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;

public class Atom {
    private final String name;

    public Atom(String name) {
        this.name = name;
    }

    public Atom(char name) {
        this.name = Character.toString(name);
    }

    public static List<Atom> stringToAtoms(String string) {
        List<Atom> atoms = new LinkedList<>();
        int i = 0;
        while (i < string.length()) {
            if (i == string.length() - 1 || Character.isUpperCase(string.charAt(i + 1)) || string.charAt(i) == 'e') {
                atoms.add(new Atom(string.charAt(i)));
                i++;
            } else {
                atoms.add(new Atom(string.substring(i, i + 2)));
                i += 2;
            }
        }
        return atoms;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Atom atom = (Atom) o;
        return Objects.equals(name, atom.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
