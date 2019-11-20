package AoC.Day161921;

import java.util.ArrayList;
import java.util.List;

public class Day19 {
    public static void main(String[] args) {
        new Day19();
    }

    public Day19() {
        /*
        Map<String, OpcodeFunction> operations = new HashMap<>();
        operations.put("addi", new Addi());
        operations.put("addr", new Addr());
        operations.put("bani", new Bani());
        operations.put("banr", new Banr());
        operations.put("bori", new Bori());
        operations.put("borr", new Borr());
        operations.put("eqir", new Eqir());
        operations.put("eqri", new Eqri());
        operations.put("eqrr", new Eqrr());
        operations.put("gtir", new Gtir());
        operations.put("gtri", new Gtri());
        operations.put("gtrr", new Gtrr());
        operations.put("muli", new Muli());
        operations.put("mulr", new Mulr());
        operations.put("seti", new Seti());
        operations.put("setr", new Setr());

        String[] input = FileUtilities.getArrayFromFile(new File("src/AoC/Day161921/Input19.txt"));
        int instructionPointerRegister = RegEx.getIntegers(input[0]).get(0);
        int instructionPointer = 0;
        KnownInstruction[] program = new KnownInstruction[input.length - 1];
        for (int i = 1; i < input.length; i++) {
            String s = input[i];
            OpcodeFunction operation = operations.get(s.substring(0, 4));
            List<Integer> values = RegEx.getIntegers(s);
            program[i - 1] = new KnownInstruction(operation, values.get(0), values.get(1), values.get(2));
        }
        Memory memory = new Memory(1, 0, 0, 0, 0, 0);
        while (instructionPointer < program.length && instructionPointer >= 0) {
            System.out.println(instructionPointer);
            memory.getRegisters()[instructionPointerRegister] = instructionPointer;
            memory = program[instructionPointer].apply(memory);
            instructionPointer = memory.getRegisters()[instructionPointerRegister];
            instructionPointer++;
        }
        System.out.println(memory.getRegisters()[0]);
        */
        //Programmet summerar faktorerna i d.
        List<Integer> factors = new ArrayList<>();
        int target = 10551320;
        for (int i = 1; i <= target; i++) {
            if (target % i == 0) {
                factors.add(i);
            }
        }
        int sum = 0;
        for (Integer i : factors) {
            sum += i;
        }
        System.out.println("Number of factors = " + factors.size());
        System.out.println(factors.toString());
        System.out.println("Sum of factors = " + sum);
    }
}
