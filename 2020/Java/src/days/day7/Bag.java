package days.day7;

import java.util.HashMap;
import java.util.Map;

public class Bag {
    private final String name;
    private final Map<Bag, Integer> contents;

    public Bag(String name) {
        this.name = name;
        contents = new HashMap<>();
    }

    public void addContents(Bag inner, int amount) {
        contents.put(inner, amount);
    }

    public boolean contains(Bag bag) {
        return contents.containsKey(bag);
    }

    public long getNumberOfBags() {
        return getNumberOfBagsHelper() - 1;
    }

    private long getNumberOfBagsHelper() {
        long numberOfBags = 1;
        for (Bag bag : contents.keySet()) {
            numberOfBags += bag.getNumberOfBagsHelper() * contents.get(bag);
        }
        return numberOfBags;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(name).append(" bags contain");
        if (contents.isEmpty()) {
            sb.append(" no other bags.");
        } else {
            for (Bag content : contents.keySet()) {
                sb.append(" ").append(contents.get(content)).append(" ").append(content.name).append(" bags,");
            }
            sb.deleteCharAt(sb.length() - 1);
            sb.append(".");
        }
        return sb.toString();
    }
}
