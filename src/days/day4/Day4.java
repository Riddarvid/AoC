package days.day4;

import riddarvid.aoc.days.Day;
import riddarvid.aoc.parsing.ParsingUtils;

import java.util.ArrayList;
import java.util.List;

public class Day4 extends Day {
    private List<Passport> passports;

    public static void main(String[] args) {
        new Day4();
    }

    @Override
    protected void part1() {
        int sum = 0;
        for (Passport passport : passports) {
            if (passport.allFieldsPresent()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void part2() {
        int sum = 0;
        for (Passport passport : passports) {
            if (passport.isValid()) {
                sum++;
            }
        }
        System.out.println(sum);
    }

    @Override
    protected void setup() {
        passports = new ArrayList<>();
        Passport current = new Passport();
        for (String line : lines) {
            if (line.equals("")) {
                passports.add(current);
                current = new Passport();
            } else {
                parseLine(current, line);
            }
        }
        passports.add(current);
    }

    private void parseLine(Passport passport, String line) {
        List<String> properties = ParsingUtils.getTokens(line, ' ');
        for (String property : properties) {
            parseProperty(passport, property);
        }
    }

    private void parseProperty(Passport passport, String property) {
        List<String> tokens = ParsingUtils.getTokens(property, ':');
        String key = tokens.get(0);
        switch (key) {
            case "byr" -> passport.setBirthYear(Integer.parseInt(tokens.get(1)));
            case "iyr" -> passport.setIssueYear(Integer.parseInt(tokens.get(1)));
            case "eyr" -> passport.setExpirationYear(Integer.parseInt(tokens.get(1)));
            case "hgt" -> passport.setHeight(tokens.get(1));
            case "hcl" -> passport.setHairColor(tokens.get(1));
            case "ecl" -> passport.setEyeColor(tokens.get(1));
            case "pid" -> passport.setPassportID(tokens.get(1));
            case "cid" -> passport.setCountryID(Integer.parseInt(tokens.get(1)));
        }
    }
}
