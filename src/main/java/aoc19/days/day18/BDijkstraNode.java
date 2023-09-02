package aoc19.days.day18;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class BDijkstraNode {
    private final Set<Node> nodes; //Should always contain four nodes
    private final Set<Character> keys;

    public BDijkstraNode(Set<Node> nodes, Set<Character> keys) {
        this.nodes = new HashSet<>(nodes);
        this.keys = new HashSet<>(keys);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        BDijkstraNode that = (BDijkstraNode) o;
        return Objects.equals(nodes, that.nodes) &&
                Objects.equals(keys, that.keys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(nodes, keys);
    }

    public Set<Node> getNodes() {
        return new HashSet<>(nodes);
    }

    public Set<Character> getKeys() {
        return new HashSet<>(keys);
    }

    public BDijkstraNode createNeighbour(Node newNode, Node oldNode, Set<Character> keys) {
        Set<Node> oldNodes = new HashSet<>(nodes);
        oldNodes.remove(oldNode);
        oldNodes.add(newNode);
        return new BDijkstraNode(oldNodes, keys);
    }
}
