package aoc.days.day24;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Bridge {
    private final List<Component> components;
    private final int endPins;

    public Bridge() {
        components = new ArrayList<>();
        endPins = 0;
    }

    private Bridge(List<Component> components, int endPins, Component extension) {
        this.components = new ArrayList<>(components);
        this.components.add(extension);
        if (endPins == extension.getLeft()) {
            this.endPins = extension.getRight();
        } else {
            this.endPins = extension.getLeft();
        }
    }

    public int strength() {
        int sum = 0;
        for (Component c : components) {
            sum += c.getLeft() + c.getRight();
        }
        return sum;
    }

    public Bridge extend(Component extension) {
        return new Bridge(components, endPins, extension);
    }

    public Set<Component> getValidExtensions(Set<Component> remaining) {
        Set<Component> validExtensions = new HashSet<>();
        for (Component c : remaining) {
            if (c.canConnectTo(endPins)) {
                validExtensions.add(c);
            }
        }
        return validExtensions;
    }

    public int length() {
        return components.size();
    }
}
