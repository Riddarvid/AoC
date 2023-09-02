package aoc.days.Day15.FightModel;

import aoc.days.Day15.Logger.Logger;
import aoc.days.Day15.Logger.Observer;

import java.awt.*;
import java.util.*;
import java.util.List;

public class Unit implements IPositionable {
    private static List<Unit> units;
    private static Observer logger = new Logger();
    private static boolean combatEndedEarly = false;

    private Point position;
    private Color team;
    private int damage = 3;
    private int health = 200;
    private boolean dead = false;

    public Unit(Point position, Color team, int atkPwr) {
        this.position = position;
        this.team = team;
        damage = atkPwr;
    }

    public Color getTeam() {
        return team;
    }

    public int getX() {
        return position.getX();
    }

    public int getY() {
        return position.getY();
    }

    public boolean isInRange() {
        for (Unit o : units) {
            if (!o.isDead() && team != o.team) {
                if (north() != null && north().equals(o.position)) {
                    return true;
                }
                if (west() != null && west().equals(o.position)) {
                    return true;
                }
                if (east() != null && east().equals(o.position)) {
                    return true;
                }
                if (south() != null && south().equals(o.position)) {
                    return true;
                }
            }
        }
        return false;
    }

    public void attack() {
        List<Unit> inRange = getInRange();
        PriorityQueue<Unit> hpQueue = new PriorityQueue<>(new HPComparator());
        hpQueue.addAll(inRange);
        Unit lowest = hpQueue.poll();
        PriorityQueue<Unit> readQueue = new PriorityQueue<>(new ReadComparator());
        readQueue.add(lowest);
        while (!hpQueue.isEmpty()) {
            Unit u = hpQueue.poll();
            if (u.health == lowest.health) {
                readQueue.add(u);
            }
        }
        Unit target = readQueue.poll();
        target.hit(damage);
        logger.actOnAttack(this, target);
    }

    private void hit(int damage) {
        health -= damage;
        if (health <= 0) {
            dead = true;
        }
    }

    private List<Unit> getInRange() {
        List<Unit> inRange = new ArrayList<>();
        for (Unit o : units) {
            if (!o.isDead() && team != o.team) {
                if (north() != null && north().equals(o.position)) {
                    inRange.add(o);
                }
                if (west() != null && west().equals(o.position)) {
                    inRange.add(o);
                }
                if (east() != null && east().equals(o.position)) {
                    inRange.add(o);
                }
                if (south() != null && south().equals(o.position)) {
                    inRange.add(o);
                }
            }
        }
        return inRange;
    }

    public void move() {
        List<Point> potentialTargets = findPotentialTargets();
        if (potentialTargets.isEmpty()) {
            combatEndedEarly = true;
        }
        List<Point> nonOccupiedTargets = findNonOccupiedTargets(potentialTargets);
        List<Point> reachableTargets = findReachableTargets(nonOccupiedTargets);
        if (!reachableTargets.isEmpty()) {
            List<TargetEntry> closestTargets = findClosestTargets(reachableTargets);
            PriorityQueue<TargetEntry> pointQueue = new PriorityQueue<>(new ReadComparator());
            pointQueue.addAll(closestTargets);
            Point target = pointQueue.poll().getTarget();
            step(target);
            logger.actOnMove(this);
        }
    }

    private void step(Point target) {
        Point shortest = null;
        int distance = Integer.MAX_VALUE;
        TargetEntry tested;
        if (north() != null && !isOccupied(north())) {
            tested = findShortestPath(target, north());
            if (tested.getDistance() < distance) {
                shortest = north();
                distance = tested.getDistance();
            }
        }
        if (west() != null && !isOccupied(west())) {
            tested = findShortestPath(target, west());
            if (tested.getDistance() < distance) {
                shortest = west();
                distance = tested.getDistance();
            }
        }
        if (east() != null && !isOccupied(east())) {
            tested = findShortestPath(target, east());
            if (tested.getDistance() < distance) {
                shortest = east();
                distance = tested.getDistance();
            }
        }
        if (south() != null && !isOccupied(south())) {
            tested = findShortestPath(target, south());
            if (tested.getDistance() < distance) {
                shortest = south();
            }
        }
        if (shortest == null) {
            System.out.println("wtf");
        }
        position = shortest;
    }

    private static TargetEntry findShortestPath(Point target, Point start) {
        List<Point> visited = new ArrayList<>();
        List<Point> discoveredLastLoop = new ArrayList<>();
        Queue<Point> discoveredThisLoop = new ArrayDeque<>();
        discoveredLastLoop.add(start);
        int count = 0;
        while (!discoveredLastLoop.isEmpty()) {
            count++;
            for (Point p : discoveredLastLoop) {
                if (target.equals(p)) {
                    return new TargetEntry(p, count);
                }
                visited.add(p);
                addNeighbours(discoveredThisLoop, visited, p);
            }
            discoveredLastLoop.clear();
            discoveredLastLoop.addAll(discoveredThisLoop);
            discoveredThisLoop.clear();
        }
        return new TargetEntry(null, 100000);
    }

    private List<TargetEntry> findClosestTargets(List<Point> reachableTargets) {
        List<TargetEntry> closestTargets = new ArrayList<>();
        boolean done = false;
        List<Point> visited = new ArrayList<>();
        Queue<Point> discoveredLastLoop = new ArrayDeque<>();
        Queue<Point> discoveredThisLoop = new ArrayDeque<>();
        discoveredLastLoop.add(this.position);
        int count = 0;
        while (!done) {
            count++;
            for (Point p : discoveredLastLoop) {
                if (reachableTargets.contains(p)) {
                    done = true;
                    closestTargets.add(new TargetEntry(p, count));
                }
                visited.add(p);
                addNeighbours(discoveredThisLoop, visited, p);
            }
            discoveredLastLoop.clear();
            discoveredLastLoop.addAll(discoveredThisLoop);
            discoveredThisLoop.clear();
        }
        return closestTargets;
    }

    private List<Point> findReachableTargets(List<Point> nonOccupiedTargets) {
        List<Point> reachableTargets = new ArrayList<>();
        List<Point> visited = new ArrayList<>();
        Queue<Point> discovered = new ArrayDeque<>();
        discovered.add(this.position);
        while (!discovered.isEmpty()) {
            Point p = discovered.poll();
            if (nonOccupiedTargets.contains(p)) {
                reachableTargets.add(p);
            }
            visited.add(p);
            addNeighbours(discovered, visited, p);
            discovered.remove(p);
        }
        return reachableTargets;
    }

    private static void addNeighbours(Queue<Point> discovered, List<Point> visited, Point p) {
        if (p.north != null && !visited.contains(p.north) && !discovered.contains(p.north) && !isOccupied(p.north)) {
            discovered.add(p.north);
        }
        if (p.west != null && !visited.contains(p.west) && !discovered.contains(p.west) && !isOccupied(p.west)) {
            discovered.add(p.west);
        }
        if (p.east != null && !visited.contains(p.east) && !discovered.contains(p.east) && !isOccupied(p.east)) {
            discovered.add(p.east);
        }
        if (p.south != null && !visited.contains(p.south) && !discovered.contains(p.south) && !isOccupied(p.south)) {
            discovered.add(p.south);
        }
    }

    private List<Point> findNonOccupiedTargets(List<Point> potentialTargets) {
        List<Point> nonOccupiedTargets = new ArrayList<>();
        for (Point p : potentialTargets) {
            if (!isOccupied(p)) {
                nonOccupiedTargets.add(p);
            }
        }
        return nonOccupiedTargets;
    }

    private static boolean isOccupied(Point p) {
        for (Unit u : units) {
            if (u.position == p && !u.isDead()) {
                return true;
            }
        }
        return false;
    }

    private List<Point> findPotentialTargets() {
        List<Point> potentialTargets = new ArrayList<>();
        for (Unit u : units) {
            if (team != u.team && !u.isDead()) {
                if (u.north() != null) {
                    potentialTargets.add(u.north());
                }
                if (u.west() != null) {
                    potentialTargets.add(u.west());
                }
                if (u.east() != null) {
                    potentialTargets.add(u.east());
                }
                if (u.south() != null) {
                    potentialTargets.add(u.south());
                }
            }
        }
        return potentialTargets;
    }

    public static void setUnits(List<Unit> units) {
        Unit.units = units;
    }

    public Point north() {
        return position.north;
    }

    public Point west() {
        return position.west;
    }

    public Point east() {
        return position.east;
    }

    public Point south() {
        return position.south;
    }

    public int getHealth() {
        return health;
    }

    public int getDamage() {
        return damage;
    }

    public boolean isDead() {
        return dead;
    }

    public static void setCombatEndedEarly(boolean combatEndedEarly) {
        Unit.combatEndedEarly = combatEndedEarly;
    }

    @Override
    public String toString() {
        if (team == Color.RED) {
            return "Elf";
        } else {
            return "Goblin";
        }
    }

    public static boolean isCombatEndedEarly() {
        return combatEndedEarly;
    }
}
