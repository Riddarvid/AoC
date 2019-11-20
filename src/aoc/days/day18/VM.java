package aoc.days.day18;

import aoc.days.day18.instructions.Instruction;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

public class VM implements Runnable {
    private Map<Character, Long> registers;
    private long lastPlayedSound;
    private int instructionPointer;
    private List<Instruction> instructions;
    private boolean isRunning;
    private VM other;
    private LinkedList<Long> messages;

    public void run() {
        isRunning = true;
        while (isRunning) {
            execute();
        }
    }

    public void execute() {
        instructions.get(instructionPointer).execute(this);
    }

    public void setInstructionPointer(int newIP) {
        instructionPointer = newIP;
    }

    public void incInstructionPointer() {
        instructionPointer++;
    }

    public VM(Map<Character, Long> registers, List<Instruction> instructions) {
        this.registers = registers;
        this.instructions = instructions;
        lastPlayedSound = 0;
        instructionPointer = 0;
        messages = new LinkedList<>();
    }

    public void setRegister(char register, long value) {
        registers.put(register, value);
    }

    public long getRegister(char register) {
        return registers.get(register);
    }

    public void playSound(long frequency) {
        lastPlayedSound = frequency;
    }

    public long getLastPlayedSound() {
        return lastPlayedSound;
    }

    public int getInstructionPointer() {
        return instructionPointer;
    }

    public void stop() {
        isRunning = false;
    }

    public void addMessage(long message) {
        messages.addLast(message);
    }

    public void sendMessage(long message) {
        other.addMessage(message);
    }

    public boolean hasMessage() {
        return !messages.isEmpty();
    }

    public long getLastMessage() {
        return messages.removeLast();
    }
}
