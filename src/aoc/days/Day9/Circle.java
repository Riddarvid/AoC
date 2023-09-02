package aoc.days.Day9;

public class Circle {
    private Node current;

    public Circle() {
        this.current = new Node();
    }

    protected class Node {
        private Node next;
        private Node previous;
        private int value;

        public Node(int value) {
            this.value = value;
        }

        private Node() {
            value = 0;
            previous = this;
            next = this;
        }

        @Override
        public String toString() {
            return "Value=" + value;
        }
    }

    public void addMarble(int value, Player player) {
        Node toBePlaced = new Node(value);
        if (toBePlaced.value % 23 == 0) {
            player.addScore(toBePlaced.value);
            changeCurrent(-6);
            Node beforeRemoved = current.previous.previous;
            Node afterRemoved = current;
            player.addScore(current.previous.value);
            beforeRemoved.next = afterRemoved;
            afterRemoved.previous = beforeRemoved;
        } else {
            Node beforePlaced = current.next;
            Node afterPlaced = current.next.next;
            toBePlaced.next = afterPlaced;
            toBePlaced.previous = beforePlaced;
            beforePlaced.next = toBePlaced;
            afterPlaced.previous = toBePlaced;
            current = toBePlaced;
        }
    }

    public void changeCurrent(int offset) {
        if (offset >= 0) {
            for (int i = 0; i < offset; i++) {
                current = current.next;
            }
        } else {
            for (int i = 0; i > offset; i--) {
                current = current.previous;
            }
        }
    }
}
