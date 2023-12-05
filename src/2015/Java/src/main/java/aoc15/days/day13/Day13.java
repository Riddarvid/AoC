package aoc15.days.day13;

import aoc.days.Day;
import aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day13 extends Day {
    private List<Person> allPeople;

    @Override
    public long part1() {
        Person.setFirst(allPeople.get(0));
        return allPeople.get(0).getMaxHappiness();
    }

    @Override
    public long part2() {
        Person me = new Person("Me");
        for (Person other : allPeople) {
            other.setOneWay(me.getName(), 0);
            me.setOneWay(other.getName(), 0);
        }
        allPeople.add(me);
        for (Person person : allPeople) {
            person.setTwoWay();
        }
        Person.setFirst(allPeople.get(0));
        return allPeople.get(0).getMaxHappiness();
    }

    @Override
    public void setup() {
        allPeople = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            Person person = new Person(tokens.get(0));
            if (allPeople.contains(person)) {
                person = allPeople.get(allPeople.indexOf(person));
            } else {
                allPeople.add(person);
            }
            Person other = new Person(tokens.get(10).substring(0, tokens.get(10).length() - 1));
            if (allPeople.contains(other)) {
                other = allPeople.get(allPeople.indexOf(other));
            } else {
                allPeople.add(other);
            }
            int distance = Integer.parseInt(tokens.get(3));
            if (tokens.get(2).equals("lose")) {
                distance = -distance;
            }
            person.setOneWay(other.getName(), distance);
        }
        Person.setAllPeople(allPeople);
        for (Person person : allPeople) {
            person.setTwoWay();
        }
    }
}
