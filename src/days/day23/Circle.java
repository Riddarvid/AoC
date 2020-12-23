package days.day23;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Circle {
    private Node current;
    private final Node[] nodes;
    private final int lowest;
    private final int highest;

    public Circle(List<Integer> values) {
        this(values, values.size());
    }

    public Circle(List<Integer> values, int total) {
        List<Node> nodeList = new ArrayList<>();
        nodes = new Node[total + 1];
        int lowest = values.get(0);
        int highest = values.get(0);
        for (int value : values) {
            Node node = new Node(value);
            nodeList.add(node);
            nodes[value] = node;
            if (value > highest) {
                highest = value;
            }
            if (value < lowest) {
                lowest = value;
            }
        }
        for (int i = highest + 1; i <= total; i++) {
            Node node = new Node(i);
            nodeList.add(node);
            nodes[i] = node;
        }
        this.lowest = lowest;
        this.highest = total;
        for (int i = 0; i < nodeList.size() - 1; i++) {
            nodeList.get(i).next = nodeList.get(i + 1);
        }
        nodeList.get(nodeList.size() - 1).next = nodeList.get(0);
        for (int i = 1; i < nodeList.size(); i++) {
            nodeList.get(i).previous = nodeList.get(i - 1);
        }
        nodeList.get(0).previous = nodeList.get(nodeList.size() - 1);
        current = nodeList.get(0);
    }

    public void move() {
        Node nextThree = unlink(1, 4);
        Node destination = getDestination(nextThree);
        insert(destination, nextThree);
        current = current.next;
    }

    private void insert(Node destination, Node held) {
        Node after = destination.next;
        Node last = held;
        while (last.next != null) {
            last = last.next;
        }
        held.previous = destination;
        destination.next = held;
        after.previous = last;
        last.next = after;
    }

    private Node getDestination(Node held) {
        int destinationLabel = current.label - 1;
        if (destinationLabel < lowest) {
            destinationLabel = highest;
        }
        Set<Integer> heldLabels = new HashSet<>();
        Node node = held;
        while (node != null) {
            heldLabels.add(node.label);
            node = node.next;
        }
        while (heldLabels.contains(destinationLabel)) {
            destinationLabel--;
            if (destinationLabel < lowest) {
                destinationLabel = highest;
            }
        }
        return nodes[destinationLabel];
    }

    private Node unlink(int begin, int end) {
        Node beginNode = current;
        for (int i = 0; i < begin; i++) {
            beginNode = beginNode.next;
        }
        Node before = beginNode.previous;
        beginNode.previous = null;
        Node endNode = beginNode;
        for (int i = begin; i < end - 1; i++) {
            endNode = endNode.next;
        }
        Node next = endNode.next;
        endNode.next = null;
        before.next = next;
        next.previous = before;
        return beginNode;
    }

    public List<Integer> toList(int label) {
        Node start = current;
        while (start.label != label) {
            start = start.next;
        }
        Node node = start.next;
        List<Integer> list = new ArrayList<>();
        while (node.label != label) {
            list.add(node.label);
            node = node.next;
        }
        return list;
    }

    public long[] getNextLabels(int label, int size) {
        Node node = nodes[label];
        long[] labels = new long[size];
        for (int i = 0; i < size; i++) {
            labels[i] = node.next.label;
            node = node.next;
        }
        return labels;
    }

    private static class Node {
        private final int label;
        private Node next;
        private Node previous;

        private Node(int label) {
            this.label = label;
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(current.label);
        Node node = current.next;
        while (node != current) {
            sb.append(node.label);
            node = node.next;
        }
        return sb.toString();
    }
}
