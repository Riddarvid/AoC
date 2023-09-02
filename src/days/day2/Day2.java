package days.day2;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day2 extends Day {
    private List<Password> passwords;

    public static void main(String[] args) {
        new Day2();
    }

    @Override
    public long part1() {
        int sum = 0;
        for (Password p : passwords) {
            if (p.fulfillsSledPolicy()) {
                sum++;
            }
        }
        return sum;
    }

    @Override
    public long part2() {
        int sum = 0;
        for (Password p : passwords) {
            if (p.fulfillsShopPolicy()) {
                sum++;
            }
        }
        return sum;
    }

    @Override
    public void setup() {
        passwords = new ArrayList<>();
        for (String s : lines) {
            List<String> tokens = ParsingUtils.getTokens(s, ' ');
            List<Integer> limits = ParsingUtils.getIntegers(tokens.get(0));
            char letter = tokens.get(1).charAt(0);
            SledPolicy sledPolicy = new SledPolicy(letter, limits.get(0), limits.get(1));
            ShopPolicy shopPolicy = new ShopPolicy(letter, limits.get(0), limits.get(1));
            Password password = new Password(tokens.get(2), sledPolicy, shopPolicy);
            passwords.add(password);
        }
    }
}
