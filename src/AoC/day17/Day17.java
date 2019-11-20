package AoC.day17;

import AoC.Day;

import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Queue;

public class Day17 extends Day {
    private List<Integer> containers;
    private List<ContainerCombo> validContainerCombos;

    public static void main(String[] args) {
        new Day17();
    }

    @Override
    protected void part1() {
        int nCombinations = 0;
        validContainerCombos = new ArrayList<>();
        for (int i = 0; i < Math.pow(2, containers.size()); i++) {
            List<Integer> combinations = getComb(i);
            if (sum(combinations) == 150) {
                nCombinations++;
                ContainerCombo c = new ContainerCombo(combinations);
                validContainerCombos.add(c);
            }
        }
        System.out.println(nCombinations);
    }

    private int sum(List<Integer> comb) {
        int sum = 0;
        for (int volume : comb) {
            sum += volume;
        }
        return sum;
    }

    private List<Integer> getComb(int comb) {
        List<Integer> combination = new ArrayList<>();
        for (int i = 0; i < containers.size(); i++) {
            int offset = 1 << i;
            if ((comb & offset) != 0) {
                combination.add(containers.get(i));
            }
        }
        return combination;
    }

    @Override
    protected void part2() {
        Queue<ContainerCombo> pq = new PriorityQueue<>(validContainerCombos);
        int min = pq.peek().getCount();
        int amount = 0;
        while (!pq.isEmpty() && pq.peek().getCount() == min) {
            amount++;
            pq.poll();
        }
        System.out.println(amount);
    }

    @Override
    protected void setup() {
        containers = new ArrayList<>();
        for (String s : lines) {
            containers.add(Integer.parseInt(s));
        }
    }
}
