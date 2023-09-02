package aoc.days.day19;

import java.util.ArrayList;
import java.util.List;

public class Circle {
    private Node current;
    private int size;

    public Circle(int n) {
        size = n;
        List<Node> nodes = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            nodes.add(new Node(i));
        }
        current = nodes.get(0);
        for (int i = 1; i < nodes.size() - 1; i++) {
            Node node = nodes.get(i);
            node.previous = nodes.get(i - 1);
            node.next = nodes.get(i + 1);
        }
        nodes.get(0).next = nodes.get(1);
        nodes.get(0).previous = nodes.get(nodes.size() - 2);
        nodes.get(nodes.size() - 1).next = nodes.get(0);
        nodes.get(nodes.size() - 1).previous = nodes.get(nodes.size() - 2);
    }

    private static class Node {
        public Node(int id) {
            this.id = id;
        }

        private final int id;
        private Node previous;
        private Node next;
    }

    public void steal() {
        current.next = current.next.next;
        current.next.previous = current;
        current = current.next;
        size--;
    }

    public int getSize() {
        return size;
    }

    public int getCurrent() {
        return current.id;
    }
}
