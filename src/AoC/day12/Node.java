package AoC.day12;

import java.util.ArrayList;
import java.util.List;

public class Node {
    private int value;
    private Node parent;
    private List<Node> children;
    private boolean red = false;

    public Node() {
        value = 0;
        children = new ArrayList<>();
    }

    public Node(Node parent) {
        this.parent = parent;
        parent.children.add(this);
        value = 0;
        children = new ArrayList<>();
    }

    public void add(int a) {
        value += a;
    }

    public Node getParent() {
        return parent;
    }

    public void setRed() {
        red = true;
    }

    public int getTotalValue() {
        if (red) {
            return 0;
        }
        int sum = value;
        for (Node child : children) {
            sum += child.getTotalValue();
        }
        return sum;
    }
}
