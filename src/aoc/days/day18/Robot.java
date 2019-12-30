package aoc.days.day18;

import aoc.utils.geometry.Direction;
import aoc.utils.geometry.Point2D;

import java.util.*;

public class Robot {
    private Node start;
    private Map<Node, Set<Edge>> connections;
    private Map<Character, Set<Character>> dependencies;

    public int dijkstra() {
        Map<DijkstraNode, Integer> distances = new HashMap<>();
        Map<DijkstraNode, DijkstraNode> previous = new HashMap<>();
        Map<DijkstraNode, DijkstraNode> discovered = new HashMap<>();
        Set<DijkstraNode> processed = new HashSet<>();
        Queue<DijkstraNode> unexplored = new PriorityQueue<>(new DijkstraComparator(distances));
        DijkstraNode dStart = new DijkstraNode(start, new HashSet<>());
        discovered.put(dStart, dStart);
        unexplored.add(dStart);
        distances.put(dStart, 0);
        previous.put(dStart, null);
        while (!unexplored.isEmpty()) {
            DijkstraNode current = unexplored.poll();
            //System.out.println(current);
            processed.add(current);
            if (current.getKeys().size() == dependencies.size()) {
                break;
            }
            process(current, discovered, distances, previous, processed, unexplored);
        }
        DijkstraNode best = getBest(processed, distances);
        System.out.println("Iterations: " + processed.size());
        System.out.println("Nodes: " + discovered.size());
        return distances.get(best);
    }

    private DijkstraNode getBest(Set<DijkstraNode> processed, Map<DijkstraNode, Integer> distances) {
        Set<DijkstraNode> allKeys = getAllKeys(processed);
        DijkstraNode best = allKeys.iterator().next();
        for (DijkstraNode dn : allKeys) {
            if (distances.get(dn) < distances.get(best)) {
                best = dn;
            }
        }
        return best;
    }

    private Set<DijkstraNode> getAllKeys(Set<DijkstraNode> processed) {
        Set<DijkstraNode> allKeys = new HashSet<>();
        for (DijkstraNode dn : processed) {
            if (dn.getKeys().size() == dependencies.size()) {
                allKeys.add(dn);
            }
        }
        return allKeys;
    }

    private void process(DijkstraNode current, Map<DijkstraNode, DijkstraNode> discovered, Map<DijkstraNode, Integer> distances, Map<DijkstraNode, DijkstraNode> previous, Set<DijkstraNode> processed, Queue<DijkstraNode> unexplored) {
        for (Edge edge : connections.get(current.getNode())) {
            Node neighbour = edge.getEnd();
            Set<Character> keysNeeded = dependencies.get(neighbour.getKey());
            if (keysNeeded != null) {
                if (!current.getKeys().containsAll(keysNeeded)) {
                    continue;
                }
            }
            Set<Character> keys = new HashSet<>(current.getKeys());
            if (neighbour.isKey()) {
                keys.add(neighbour.getKey());
            }
            DijkstraNode dn = new DijkstraNode(neighbour, keys);
            if (!processed.contains(dn)) { //Om vi inte redan är klara med denna nod
                if (discovered.containsKey(dn)) {
                    int oldDistance = distances.get(dn);
                    int newDistance = distances.get(current) + edge.getLength();
                    if (newDistance < oldDistance) {
                        distances.put(dn, newDistance);
                        previous.put(dn, current);
                    }
                } else {
                    discovered.put(dn, dn);
                    distances.put(dn, distances.get(current) + edge.getLength());
                    unexplored.add(dn);
                    previous.put(dn, current);
                }
            }
        }
    }

    public Robot(int x, int y, List<String> lines) {
        start = new Node(x, y, '@');
        connections = generateConnections(x, y, lines);
        while (hasDeadEnds(connections) || hasRedundantNodes(connections)) {
            if (hasDeadEnds(connections)) {
                removeDeadEnds(connections);
            }
            if (hasRedundantNodes(connections)) {
                removeRedundantNodes(connections);
            }
        }
        dependencies = generateDependencies(connections, x, y);
        removeDoors(connections);
    }

    private void removeDoors(Map<Node, Set<Edge>> connections) {
        while (hasDoors(connections)) {
            removeDoor(connections);
        }
        while (hasDeadEnds(connections) || hasRedundantNodes(connections)) {
            if (hasDeadEnds(connections)) {
                removeDeadEnds(connections);
            }
            if (hasRedundantNodes(connections)) {
                removeRedundantNodes(connections);
            }
        }
    }

    private void removeDoor(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            if (node.isDoor()) {
                removeDoor(node, connections);
                break;
            }
        }
    }

    private void removeDoor(Node node, Map<Node, Set<Edge>> connections) {
        Node doorLess = new Node(node.getPosition());
        for (Edge edge : connections.get(node)) {
            Edge incoming = new Edge(edge.getEnd(), doorLess, edge.getLength());
            connections.get(edge.getEnd()).add(incoming);
        }
        connections.put(doorLess, connections.get(node));
        connections.remove(node);
        removeInvalidEdges(connections);
    }

    private boolean hasDoors(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            if (node.isDoor()) {
                return true;
            }
        }
        return false;
    }

    private Map<Character, Set<Character>> generateDependencies(Map<Node, Set<Edge>> connections, int x, int y) {
        Map<Character, Set<Character>> dependencies = new HashMap<>();
        Node start = new Node(x, y, '@');
        generateDependencies(new HashSet<>(), start, start, connections, dependencies, new HashSet<>());
        return dependencies;
    }

    private void generateDependencies(Set<Character> doors, Node current, Node previous, Map<Node, Set<Edge>> connections, Map<Character, Set<Character>> dependencies, Set<Node> visited) {
        if (!visited.contains(current)) {
            doors = new HashSet<>(doors);
            visited.add(current);
            if (current.isKey()) {
                dependencies.put(current.getKey(), doors);
            }
            if (current.isDoor()) {
                doors.add(Character.toLowerCase(current.getDoor()));
            }
            Set<Edge> edges = connections.get(current);
            if (edges.size() == 1) {
                return;
            }
            for (Edge edge : edges) {
                if (!edge.getEnd().equals(previous)) {
                    generateDependencies(doors, edge.getEnd(), current, connections, dependencies, visited);
                }
            }
        }
    }

    private void removeRedundantNodes(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            if (isRedundant(node, connections)) {
                removeRedundant(node, connections);
                break;
            }
        }
    }

    private void removeRedundant(Node node, Map<Node, Set<Edge>> connections) {
        Set<Edge> edges = connections.get(node);
        Iterator<Edge> it = edges.iterator();
        Edge e1 = it.next();
        Edge e2 = it.next();
        Node n1 = e1.getEnd();
        Node n2 = e2.getEnd();
        int distance = e1.getLength() + e2.getLength();
        connections.get(n1).add(new Edge(n1, n2, distance));
        connections.get(n2).add(new Edge(n2, n1, distance));
        connections.remove(node);
        removeInvalidEdges(connections);
    }

    private boolean hasRedundantNodes(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            if (isRedundant(node, connections)) {
                return true;
            }
        }
        return false;
    }

    private boolean isRedundant(Node node, Map<Node, Set<Edge>> connections) {
        return !node.isSpecial() && connections.get(node).size() == 2;
    }

    private void removeDeadEnds(Map<Node, Set<Edge>> connections) {
        Map<Node, Set<Edge>> reducedConnections = new HashMap<>();
        Set<Node> deadEnds = new HashSet<>();
        for (Node node : connections.keySet()) {
            if (isDeadEnd(node, connections)) {
                deadEnds.add(node);
            }
        }
        for (Node node : connections.keySet()) {
            if (!deadEnds.contains(node)) {
                reducedConnections.put(node, connections.get(node));
            }
        }
        removeInvalidEdges(reducedConnections);
        connections.clear();
        connections.putAll(reducedConnections);
    }

    private void removeInvalidEdges(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            Set<Edge> edges = connections.get(node);
            Set<Edge> validEdges = new HashSet<>();
            for (Edge edge : edges) {
                Node end = edge.getEnd();
                if (connections.containsKey(end)) {
                    validEdges.add(edge);
                }
            }
            edges.clear();
            edges.addAll(validEdges);
        }
    }

    private boolean hasDeadEnds(Map<Node, Set<Edge>> connections) {
        for (Node node : connections.keySet()) {
            if (isDeadEnd(node, connections)) {
                return true;
            }
        }
        return false;
    }

    private boolean isDeadEnd(Node node, Map<Node, Set<Edge>> connections) {
        return connections.get(node).size() == 1 && !node.isSpecial();
    }

    private Map<Node, Set<Edge>> generateConnections(int x, int y, List<String> map) {
        Map<Node, Set<Edge>> connections = new HashMap<>();
        Queue<Node> toExplore = new LinkedList<>();
        Set<Edge> found = new HashSet<>();
        Set<Node> explored = new HashSet<>();
        Node start = new Node(x, y);
        toExplore.add(start);
        while (!toExplore.isEmpty()) { // As long as there ar nodes left to explore, explore the next node
            Node current = toExplore.poll();
            Set<Edge> outgoing = findOutgoing(current, found, map); //Find all outgoing edges from the current node, add them if they don't already exist. Handled automatically by Set.
            connections.put(current, outgoing);
            found.addAll(outgoing);
            for (Edge edge : outgoing) {
                Node end = edge.getEnd();
                if (!toExplore.contains(end) && !explored.contains(end)) {
                    toExplore.add(edge.getEnd());
                }
            }
            explored.add(current);
        }
        return connections;
    }

    private Set<Edge> findOutgoing(Node current, Set<Edge> found, List<String> map) {
        Set<Edge> edges = new HashSet<>();
        for (int i = 0; i < 4; i++) {
            Direction direction = Direction.values()[i];
            if (existsEdgeInDirection(current, direction, map)) {
                edges.add(findEdgeInDirection(current, direction, map));
            }
        }
        Set<Edge> validEdges = new HashSet<>();
        for (Edge edge : edges) {
            if (!found.contains(edge)) {
                validEdges.add(edge);
            }
        }
        return validEdges;
    }

    private Edge findEdgeInDirection(Node start, Direction direction, List<String> map) {
        return findNextEdge(start, start.getPosition(), start.getPosition().moveBy(direction), 1, map);
    }

    private Edge findNextEdge(Node start, Point2D lastPosition, Point2D position, int distanceSoFar, List<String> map) {
        char c = map.get((int) position.getY()).charAt((int) position.getX());
        if (c != '.') { //Key or door or start
            Node end = new Node(position, c);
            return new Edge(start, end, distanceSoFar);
        }
        List<Point2D> neighbours = getValidNeighbours(position, map);
        if (neighbours.size() != 2) { //Dead end or intersection
            Node end = new Node(position);
            return new Edge(start, end, distanceSoFar);
        }
        //Node is normal path
        if (neighbours.get(0).equals(lastPosition)) {
            return findNextEdge(start, position, neighbours.get(1), distanceSoFar + 1, map);
        } else {
            return findNextEdge(start, position, neighbours.get(0), distanceSoFar + 1, map);
        }
    }

    private List<Point2D> getValidNeighbours(Point2D position, List<String> map) {
        Set<Point2D> neighbours = position.neighbours();
        List<Point2D> validNeighbours = new ArrayList<>();
        for (Point2D neighbour : neighbours) {
            if (isValid(neighbour, map)) {
                validNeighbours.add(neighbour);
            }
        }
        return validNeighbours;
    }

    private boolean isValid(Point2D position, List<String> map) {
        return map.get((int) position.getY()).charAt((int) position.getX()) != '#';
    }

    private boolean existsEdgeInDirection(Node current, Direction direction, List<String> map) {
        int x, y;
        switch (direction) {
            case NORTH:
                x = current.getX();
                y = current.getY() - 1;
                break;
            case SOUTH:
                x = current.getX();
                y = current.getY() + 1;
                break;
            case WEST:
                x = current.getX() - 1;
                y = current.getY();
                break;
            case EAST:
                x = current.getX() + 1;
                y = current.getY();
                break;
            default:
                throw new InputMismatchException();
        }
        return map.get(y).charAt(x) != '#';
    }
}
