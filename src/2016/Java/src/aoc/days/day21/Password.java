package aoc.days.day21;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class Password {
    private Node zeroNode;
    private final int length;

    public Password(String string) {
        length = string.length();
        List<Node> nodes = new ArrayList<>();
        for (char c : string.toCharArray()) {
            nodes.add(new Node(c));
        }
        zeroNode = nodes.get(0);
        for (int i = 0; i < nodes.size(); i++) {
            if (i != 0) {
                nodes.get(i).previous = nodes.get(i - 1);
            }
            if (i != nodes.size() - 1) {
                nodes.get(i).next = nodes.get(i + 1);
            }
        }
        nodes.get(0).previous = nodes.get(nodes.size() - 1);
        nodes.get(nodes.size() - 1).next = nodes.get(0);
    }

    private static class Node {
        private char value;
        private Node previous;
        private Node next;

        private Node(char value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return value + "";
        }

        public void reverse() {
            Node temp = previous;
            previous = next;
            next = temp;
        }
    }

    private Node getNodeByIndex(int index) {
        Node node = zeroNode;
        for (int i = 0; i < index; i++) {
            node = node.next;
        }
        return node;
    }

    private Node getNodeByValue(char value) {
        Node node = zeroNode;
        while (node.value != value) {
            node = node.next;
        }
        return node;
    }

    private int getIndex(char value) {
        int index = 0;
        Node node = zeroNode;
        while (node.value != value) {
            node = node.next;
            index++;
        }
        return index;
    }

    private Node removeNode(int index) {
        Node toRemove = getNodeByIndex(index);
        Node previous = toRemove.previous;
        Node next = toRemove.next;
        previous.next = next;
        next.previous = previous;
        if (index == 0) {
            zeroNode = next;
        }
        return toRemove;
    }

    private void insertNode(Node toInsert, int index) {
        Node next = getNodeByIndex(index);
        Node previous = next.previous;
        toInsert.next = next;
        toInsert.previous = previous;
        next.previous = toInsert;
        previous.next = toInsert;
        if (index == 0) {
            zeroNode = toInsert;
        }
    }

    public void unscramble(List<Instruction> instructions) {
        for (int i = instructions.size() - 1; i >= 0; i--) {
            Instruction instruction = instructions.get(i);
            List<String> parameters = instruction.getParameters();
            switch (instruction.getOperation()) {
                case SWAP_POSITION:
                    swapPosition(Integer.parseInt(parameters.get(0)), Integer.parseInt(parameters.get(1)));
                    break;
                case SWAP_LETTER:
                    swapLetter(parameters.get(0).charAt(0), parameters.get(1).charAt(0));
                    break;
                case ROTATE_STEPS:
                    if (parameters.get(0).equals("right")) {
                        rotateLeft(Integer.parseInt(parameters.get(1)));
                    } else {
                        rotateRight(Integer.parseInt(parameters.get(1)));
                    }
                    break;
                case ROTATE_LETTER:
                    reverseRotateLetter(parameters.get(0).charAt(0));
                    break;
                case REVERSE:
                    reverse(Integer.parseInt(parameters.get(0)), Integer.parseInt(parameters.get(1)));
                    break;
                case MOVE:
                    move(Integer.parseInt(parameters.get(1)), Integer.parseInt(parameters.get(0)));
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
    }

    private void reverseRotateLetter(char x) {
        int index = getIndex(x);
        int steps;
        if (index == 0) {
            steps = 9;
        } else if (index % 2 == 0) {
            steps = (index + 8) / 2 + 1;
        } else {
            steps = (index + 1) / 2;
        }
        rotateLeft(steps);
    }

    public void scramble(List<Instruction> instructions) {
        for (Instruction instruction : instructions) {
            List<String> parameters = instruction.getParameters();
            switch (instruction.getOperation()) {
                case SWAP_POSITION:
                    swapPosition(Integer.parseInt(parameters.get(0)), Integer.parseInt(parameters.get(1)));
                    break;
                case SWAP_LETTER:
                    swapLetter(parameters.get(0).charAt(0), parameters.get(1).charAt(0));
                    break;
                case ROTATE_STEPS:
                    if (parameters.get(0).equals("right")) {
                        rotateRight(Integer.parseInt(parameters.get(1)));
                    } else {
                        rotateLeft(Integer.parseInt(parameters.get(1)));
                    }
                    break;
                case ROTATE_LETTER:
                    rotateLetter(parameters.get(0).charAt(0));
                    break;
                case REVERSE:
                    reverse(Integer.parseInt(parameters.get(0)), Integer.parseInt(parameters.get(1)));
                    break;
                case MOVE:
                    move(Integer.parseInt(parameters.get(0)), Integer.parseInt(parameters.get(1)));
                    break;
                default:
                    throw new InputMismatchException();
            }
        }
    }

    private void move(int x, int y) {
        Node node = removeNode(x);
        insertNode(node, y);
    }

    private void reverse(int x, int y) {
        Node first = getNodeByIndex(x);
        Node beforeFirst = first.previous;
        Node node = first.next;
        int index = x + 1;
        while (index < y) {
            Node next = node.next;
            node.reverse();
            node = next;
            index++;
        }
        Node afterLast = node.next;
        first.previous = first.next;
        first.next = afterLast;
        node.next = node.previous;
        node.previous = beforeFirst;
        beforeFirst.next = node;
        afterLast.previous = first;
        if (x == 0) {
            zeroNode = node;
        }
    }

    private void rotateLetter(char x) {
        int index = getIndex(x);
        int steps = index + 1;
        if (index >= 4) {
            steps++;
        }
        rotateRight(steps);
    }

    private void rotateRight(int steps) {
        Node node = zeroNode;
        for (int i = 0; i < steps; i++) {
            node = node.previous;
        }
        zeroNode = node;
    }

    private void rotateLeft(int steps) {
        Node node = zeroNode;
        for (int i = 0; i < steps; i++) {
            node = node.next;
        }
        zeroNode = node;
    }

    private void swapLetter(char x, char y) {
        Node xNode = getNodeByValue(x);
        Node yNode = getNodeByValue(y);
        xNode.value = y;
        yNode.value = x;
    }

    private void swapPosition(int x, int y) {
        Node xNode = getNodeByIndex(x);
        Node yNode = getNodeByIndex(y);
        char temp = xNode.value;
        xNode.value = yNode.value;
        yNode.value = temp;
    }

    @Override
    public String toString() {
        if (zeroNode == null) {
            return "Password not initialized";
        } else {
            StringBuilder sb = new StringBuilder();
            Node current = zeroNode;
            do {
                sb.append(current.value);
                current = current.next;
            } while (current != zeroNode && sb.length() < length);
            return sb.toString();
        }
    }
}
