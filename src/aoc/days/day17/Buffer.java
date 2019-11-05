package aoc.days.day17;

public class Buffer {
    private Node current;
    private Node root;

    private class Node {
       private Node next;
       private Node previous;
       private int value;

        public Node(int value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return previous.value + " - " + value + " - " + next.value;
        }
    }

    public Buffer(int root) {
        this.root = new Node(root);
        this.current = this.root;
        current.next = current;
        current.previous = current;
    }

    public void step(int n) {
        for (int i = 0; i < n; i++) {
            current = current.next;
        }
    }

    public void insert(int toInsert) {
        Node before = current;
        Node after = current.next;
        Node newNode = new Node(toInsert);
        before.next = newNode;
        after.previous = newNode;
        newNode.previous = before;
        newNode.next = after;
        current = newNode;
    }

    public int getCurrent() {
        return current.value;
    }

    public void goToRoot() {
        current = root;
    }
}
