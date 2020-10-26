package aoc.days.Day8;

import java.util.ArrayList;
import java.util.List;

public class Tree {
    private Node root;
    private int i = 0;

    protected static class Node {
        private int nChildren;
        private int nData;
        private final List<Node> children = new ArrayList<>();
        private final List<Integer> data = new ArrayList<>();

        public Node(int nChildren, int nData) {
            this.nChildren = nChildren;
            this.nData = nData;
        }

        public int getnChildren() {
            return nChildren;
        }

        public int getnData() {
            return nData;
        }

        public List<Node> getChildren() {
            return children;
        }

        public List getData() {
            return data;
        }

        public void addData(Integer data) {
            this.data.add(data);
        }

        public void addChild(Node n) {
            children.add(n);
        }
    }

    //Wrapper
    public void fillTree(Integer[] input) {
        root = new Node(input[0], input[1]);
        i = 2;
        for (int j = 0; j < root.nChildren; j++) {
            root.addChild(fillTreeRecursive(input));
        }
        for (int j = i + root.nData; i < j; i++) {
            root.addData(input[i]);
        }
    }

    private Node fillTreeRecursive(Integer[] input) {
        Node localRoot = new Node(input[i], input[i + 1]);
        i += 2;
        for (int j = 0; j < localRoot.nChildren; j++) {
            localRoot.addChild(fillTreeRecursive(input));
        }
        for (int j = i + localRoot.nData; i < j; i++) {
            localRoot.addData(input[i]);
        }
        return localRoot;
    }

    public int getValue() {
        return getValue(root);
    }

    private int getValue(Node localRoot) {
        if (localRoot == null) {
            return 0;
        } else if (localRoot.nChildren == 0) {
            return sumOfList(localRoot.data);
        } else {
            int sum = 0;
            for (Integer index : localRoot.data) {
                if (index <= localRoot.nChildren && index > 0) {
                    sum += getValue(localRoot.children.get(index - 1));
                }
            }
            return sum;
        }
    }


    public int getTotalDataSum() {
        return getTotalDataSum(root);
    }

    private int getTotalDataSum(Node localRoot) {
        if (localRoot == null) {
            return 0;
        }
        int sum = 0;
        for (Node child : localRoot.children) {
            sum += getTotalDataSum(child);
        }
        return sum + sumOfList(localRoot.data);
    }

    private int sumOfList(List<Integer> list) {
        int sum = 0;
        for (Integer element : list) {
            sum += element;
        }
        return sum;
    }

    public Tree(Node root) {
        this.root = root;
    }

    public Tree() {
        root = null;
    }

    public Tree getChildTree(int i) {
        return new Tree((Node) root.children.get(i));
    }
}
