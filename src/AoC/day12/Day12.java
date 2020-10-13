package AoC.day12;

import AoC.FileUtilities;
import riddarvid.aoc.days.Day;

import java.util.List;
import java.util.Stack;

public class Day12 extends Day {
    private String input;

    public static void main(String[] args) {
        new Day12();
    }

    @Override
    protected void part1() {
        List<Integer> ints = FileUtilities.getIntsNegative(input);
        System.out.println(sum(ints));
    }

    private int sum(List<Integer> ints) {
        int sum = 0;
        for (int i : ints) {
            sum += i;
        }
        return sum;
    }

    @Override
    protected void part2() {
        Stack<Character> stack = new Stack<>();
        int i = 1;
        Node current = new Node();
        Node root = current;
        stack.push('{');
        char[] chars = input.toCharArray();
        while (i < chars.length) {
            char c = chars[i];
            switch (c) {
                case '[':
                    stack.push('[');
                    break;
                case ']':
                    stack.pop();
                    break;
                case '{':
                    stack.push('{');
                    current = new Node(current);
                    break;
                case '}':
                    if (stack.isEmpty()) {
                        System.out.println("hm");
                    }
                    stack.pop();
                    current = current.getParent();
                    break;
            }
            if (c == '-' || Character.isDigit(c)) {
                int start = i;
                int end = i + 1;
                while (end < chars.length && Character.isDigit(chars[end])) {
                    end++;
                }
                int val = Integer.parseInt(input.substring(start, end));
                current.add(val);
                i = end - 1;
            } else if (i < chars.length - 2 && input.substring(i, i + 3).equals("red")) {
                if (stack.peek() != '[') {
                    current.setRed();
                }
            }
            i++;
        }
        System.out.println(root.getTotalValue());
    }

    @Override
    protected void setup() {
        input = lines.get(0);
    }
}
