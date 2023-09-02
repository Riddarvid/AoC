package aoc.days.day18;

import java.util.LinkedList;
import java.util.Map;

public class VM {
    private Map<Character, Long> registers;
    private LinkedList<Long> messages;
    private int instructionPointer;
    private boolean isRunning;
    private VM other;
    private int id;
    private int sendCount;

    public VM(Map<Character, Long> registers, int id) {
        this.registers = registers;
        sendCount = 0;
        messages = new LinkedList<>();
        instructionPointer = 0;
        isRunning = true;
        this.id = id;
    }

    public void setOther(VM other) {
        this.other = other;
    }

    public long get(char register) {
        return registers.get(register);
    }

    public void put(char register, long value) {
        registers.put(register, value);
    }

    public int getInstructionPointer() {
        return instructionPointer;
    }

    public void setInstructionPointer(int instructionPointer) {
        this.instructionPointer = instructionPointer;
    }

    public void incInstructionPointer() {
        instructionPointer++;
    }

    public void send(long message) {
        sendCount++;
        synchronized (other) {
            other.messages.addLast(message);
            other.notifyAll();
        }
    }

    public boolean isRunning() {
        return isRunning;
    }

    public void receive(char register) {
        synchronized (this) {
            while (messages.isEmpty()) {
                try {
                    //System.out.println("Waiting for " + other);
                    wait(100);
                    if (messages.isEmpty()) {
                        System.out.println(this + "timed out");
                        isRunning = false;
                        System.out.println(this + " send count: " + sendCount);
                        return;
                    }
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            System.out.println(this + " received " + messages.getFirst() + " from " + other);
            registers.put(register, messages.getFirst());
            messages.removeFirst();
        }
    }

    @Override
    public String toString() {
        return "VM" + id;
    }
}
