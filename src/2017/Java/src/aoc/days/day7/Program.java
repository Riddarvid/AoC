package aoc.days.day7;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Program {
    private final String name;
    private final int weight;
    private int totalWeight;
    private Program parent;
    private final List<Program> children;

    public Program(String name, int weight) {
        this.name = name;
        this.weight = weight;
        children = new ArrayList<>();
    }

    public void addParent(Program parent) {
        this.parent = parent;
    }

    public void addChild(Program child) {
        children.add(child);
    }

    public List<Program> getChildren() {
        return children;
    }

    public String getName() {
        return name;
    }

    public int getWeight() {
        return weight;
    }

    public Program getParent() {
        return parent;
    }

    public void calculateTotalWeight() {
        int sum = weight;
        for (Program child : children) {
            child.calculateTotalWeight();
            sum += child.getTotalWeight();
        }
        totalWeight = sum;
    }

    public int getTotalWeight() {
        return totalWeight;
    }

    @Override
    public String toString() {
        return name + " (" + weight + ") Weight: " + totalWeight;
    }

    public boolean isBalanced() {
        if (children.size() < 2) {
            return true;
        }
        int initWeight = children.get(0).totalWeight;
        for (Program child : children) {
            if (child.totalWeight != initWeight) {
                return false;
            }
        }
        return true;
    }

    public Program getUnbalancedChild() {
        if (children.size() < 3) {
            throw new InputMismatchException();
        }
        int weight0 = children.get(0).totalWeight;
        int weight1 = children.get(1).totalWeight;
        int weight2 = children.get(2).totalWeight;
        int expected;
        if (weight0 == weight1) {
            expected = weight0;
        } else if (weight0 == weight2) {
            expected = weight0;
        } else {
            expected = weight1;
        }
        for (Program child : children) {
            if (child.totalWeight != expected) {
                return child;
            }
        }
        throw new InputMismatchException();
    }

    public int getExpectedWeight() {
        Program unbalanced = getUnbalancedChild();
        for (Program child : children) {
            if (child != unbalanced) {
                return child.totalWeight;
            }
        }
        throw new InputMismatchException();
    }
}
