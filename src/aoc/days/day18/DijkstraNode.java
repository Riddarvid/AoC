package aoc.days.day18;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

public class DijkstraNode {
    private final Node node;
    private final Set<Character> keys;

    public DijkstraNode(Node node, Set<Character> keys) {
        this.node = node;
        this.keys = new HashSet<>(keys);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DijkstraNode that = (DijkstraNode) o;
        return Objects.equals(node, that.node) &&
                Objects.equals(keys, that.keys);
    }

    @Override
    public int hashCode() {
        return Objects.hash(node, keys);
    }

    public Node getNode() {
        return node;
    }

    public Set<Character> getKeys() {
        return new HashSet<>(keys);
    }

    @Override
    public String toString() {
        if (keys.isEmpty()) {
            return node.toString();
        } else {
            StringBuilder keyString = new StringBuilder();
            for (Character key : keys) {
                keyString.append(key).append(", ");
            }
            String ks = keyString.toString();
            ks = ks.substring(0, ks.length() - 2);
            return node.getPosition().toString() + " Keys: " + ks;
        }
    }
}
