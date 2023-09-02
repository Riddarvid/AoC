package aoc.days.day5;

import aoc.days.Day;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Random;

public class Day5 extends Day {
    private String doorID;

    public static void main(String[] args) {
        new Day5();
    }

    @Override
    protected void part1() {
        try {
            System.out.println(generatePassword1());
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    private String generatePassword1() throws NoSuchAlgorithmException {
        StringBuilder password = new StringBuilder();
        View view = new View(password);
        int i = 0;
        MessageDigest md = MessageDigest.getInstance("MD5");
        while (password.length() < 8) {
            byte[] input = (doorID + i).getBytes();
            byte[] hash = md.digest(input);
            if (hash[0] == 0 && hash[1] == 0 && hash[2] < 16 && hash[2] >= 0) {
                if (hash[2] > 9) {
                    password.append((char)('a' + (hash[2] - 10)));
                } else {
                    password.append(hash[2]);
                }
            }
            i++;
            view.update();
        }
        return password.toString();
    }


    @Override
    protected void part2() {
        try {
            System.out.println(generatePassword2());
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        }
    }

    private String generatePassword2() throws NoSuchAlgorithmException {
        char[] password = {'_', '_', '_', '_', '_', '_', '_', '_'};
        int i = 0;
        MessageDigest md = MessageDigest.getInstance("MD5");
        while (!isDone(password)) {
            byte[] input = (doorID + i).getBytes();
            byte[] hash = md.digest(input);
            if (hash[0] == 0 && hash[1] == 0 && hash[2] < 16 && hash[2] >= 0) {
                if (hash[2] < 8) {
                    int index = hash[2];
                    if (password[index] == '_') {
                        int value = hash[3];
                        if (value < 0) {
                            value = 256 + value;
                        }
                        value = value / 16;
                        if (value > 9) {
                            password[index] = (char)('a' + (value - 10));
                        } else {
                            password[index] = (char)('0' + value);
                        }
                    }
                }
            }
            i++;
        }
        return new String(password);
    }

    private boolean isDone(char[] password) {
        for (char c : password) {
            if (c == '_') {
                return false;
            }
        }
        return true;
    }

    @Override
    protected void setup() {
        doorID = lines.get(0);
    }
}
