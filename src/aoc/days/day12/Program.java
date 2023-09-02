package aoc.days.day12;

import java.util.ArrayList;
import java.util.List;

public class Program {
    private int id;
    private List<Program> connections;
    private boolean searched;

    public Program(int id) {
        this.id = id;
        connections = new ArrayList<>();
        searched = false;
    }

    public void addConnection(Program other) {
        connections.add(other);
    }

    public int getNumberConnected() {
        if (searched) {
            return 0;
        }
        searched = true;
        int connected = 0;
        for (Program program : connections) {
            connected += program.getNumberConnected();
        }
        return 1 + connected;
    }

    public int getId() {
        return id;
    }

    public boolean isSearched() {
        return searched;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(id).append(" -> ");
        for (Program program : connections) {
            sb.append(program.getId()).append(", ");
        }
        sb.deleteCharAt(sb.length() - 1);
        return sb.toString();
    }

    public void reset() {
        searched = false;
    }

    public void markGroup() {
        if (!searched) {
            searched = true;
            for (Program program : connections) {
                program.markGroup();
            }
        }
    }
}
