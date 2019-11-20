package AoC.day13;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class Person {
    private static List<Person> allPeople;

    private Map<String, Integer> distancesOneWay = new HashMap<>();
    private Map<String, Integer> distancesTwoWay = new HashMap<>();
    private String name;
    private boolean visited = false;
    private static Person first;

    public static void setAllPeople(List<Person> allPeople) {
        Person.allPeople = allPeople;
    }

    public Person(String name) {
        this.name = name;
    }

    public void setOneWay(String name, int distance) {
        distancesOneWay.put(name, distance);
    }

    public String getName() {
        return name;
    }

    public void setTwoWay() {
        for (Person other : allPeople) {
            if (other != this) {
                int d1 = distancesOneWay.get(other.name);
                int d2 = other.distancesOneWay.get(name);
                distancesTwoWay.put(other.name, d1 + d2);
            }
        }
    }

    public static void setFirst(Person first) {
        Person.first = first;
    }

    public int getMaxHappiness() {
        visited = true;
        if (areAllSeated()) {
            visited = false;
            return distancesTwoWay.get(first.name);
        } else {
            int maxHappiness = Integer.MIN_VALUE;
            for (Person p : allPeople) {
                if (!p.visited) {
                    int current = distancesTwoWay.get(p.name) + p.getMaxHappiness();
                    if (current > maxHappiness) {
                        maxHappiness = current;
                    }
                }
            }
            visited = false;
            return maxHappiness;
        }
    }

    private boolean areAllSeated() {
        for (Person other : allPeople) {
            if (!other.visited) {
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Person person = (Person) o;
        return Objects.equals(name, person.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public String toString() {
        return name;
    }
}
