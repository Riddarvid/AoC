package AoC.Day22;

import java.util.Objects;

public class Region implements Comparable<Region> {
    private int x;
    private int y;
    private int geologicalIndex;
    private int erosionLevel;
    private int riskLevel;

    private int distance = 1000000000;
    private Region previous;
    private Tool tool;
    boolean visited = false;

    public Region(int geologicalIndex, int x, int y, int depth) {
        this.x = x;
        this.y = y;
        this.geologicalIndex = geologicalIndex;
        erosionLevel = (geologicalIndex + depth) % 20183;
        riskLevel = erosionLevel % 3;
        if (riskLevel == 0) {//Region is rocky
            tool = Tool.TORCH;
        } else if (riskLevel == 1) {//Region is wet
            tool = Tool.CLIMB;
        } else {//Region is narrow
            tool = Tool.NONE;
        }
    }

    public Region(int x, int y, int geologicalIndex, int erosionLevel, int riskLevel, int distance, Region previous, Tool tool) {
        this.x = x;
        this.y = y;
        this.geologicalIndex = geologicalIndex;
        this.erosionLevel = erosionLevel;
        this.riskLevel = riskLevel;
        this.distance = distance;
        this.previous = previous;
        this.tool = tool;
    }

    public int getRiskLevel() {
        return riskLevel;
    }

    public int getErosionLevel() {
        return erosionLevel;
    }

    public void setErosionLevel(int erosionLevel) {
        this.erosionLevel = erosionLevel;
    }

    public int getGeologicalIndex() {
        return geologicalIndex;
    }

    public void setGeologicalIndex(int geologicalIndex) {
        this.geologicalIndex = geologicalIndex;
    }

    public int getX() {
        return x;
    }

    public void setX(int x) {
        this.x = x;
    }

    public int getY() {
        return y;
    }

    public void setY(int y) {
        this.y = y;
    }

    public void setRiskLevel(int riskLevel) {
        this.riskLevel = riskLevel;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public Region getPrevious() {
        return previous;
    }

    public void setPrevious(Region previous) {
        this.previous = previous;
    }

    public Tool getTool() {
        return tool;
    }

    public void setTool(Tool tool) {
        this.tool = tool;
    }

    public Region getCopyWithOtherTool() {
        Tool otherTool;
        if (riskLevel == 0) {//Region is rocky
            otherTool = Tool.CLIMB;
        } else if (riskLevel == 1) {//Region is wet
            otherTool = Tool.NONE;
        } else {//Region is narrow
            otherTool = Tool.TORCH;
        }
        return new Region(x, y, geologicalIndex, erosionLevel, riskLevel, distance, previous, otherTool);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Region region = (Region) o;
        return x == region.x &&
                y == region.y &&
                tool == region.tool;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y, tool);
    }

    @Override
    public String toString() {
        String type = null;
        switch (riskLevel) {
            case 0:
                type = "Rocky ";
                break;
            case 1:
                type = "Wet   ";
                break;
            case 2:
                type = "Narrow";
                break;
        }
        return "x = " + x + ", y = " + y + ", Type = " + type + ", Tool = " + tool + ", Distance = " + distance;
    }

    @Override
    public int compareTo(Region o) {
        return distance - o.distance;
    }
}
