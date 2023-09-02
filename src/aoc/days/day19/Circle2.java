package aoc.days.day19;

import java.util.ArrayList;
import java.util.List;

public class Circle2 {
    private Node current;
    private Node opposite;
    private int size;

    public Circle2(int n) {
        size = n;
        List<Node> nodes = new ArrayList<>();
        int oppositePos = n / 2 + 1;
        for (int i = 1; i <= n; i++) {
            Node node = new Node(i);
            nodes.add(node);
            if (i == oppositePos) {
                opposite = node;
            }
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
        opposite.next.previous = opposite.previous;
        opposite.previous.next = opposite.next;
        current = current.next;
        if (size % 2 == 1) {
            opposite = opposite.next.next;
        } else {
            opposite = opposite.next;
        }
        size--;
    }

    public int getSize() {
        return size;
    }

    public int getCurrent() {
        return current.id;
    }
}
