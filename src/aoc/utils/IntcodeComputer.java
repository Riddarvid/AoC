package aoc.utils;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class IntcodeComputer {
    private Controller controller;

    public IntcodeComputer(Controller controller) {
        this.controller = controller;
    }

    public void execute(int[] program) {
        InstructionPointer ip = new InstructionPointer();
        while (true) {
            String instruction = Integer.toString(program[ip.getPosition()]);
            int opCode;
            String paramModesString;
            if (instruction.length() == 1) {
                opCode = Integer.parseInt(instruction.substring(instruction.length() - 1));
                paramModesString = "";
            } else {
                opCode = Integer.parseInt(instruction.substring(instruction.length() - 2));
                paramModesString = instruction.substring(0, instruction.length() - 2);
            }
            if (opCode == 99) {
                break;
            }
            switch (opCode) {
                case 1:
                    add(paramModesString, ip, program);
                    break;
                case 2:
                    mul(paramModesString, ip, program);
                    break;
                case 3:
                    input(ip, program);
                    break;
                case 4:
                    output(paramModesString, ip, program);
                    break;
                case 5:
                    jumpIfTrue(paramModesString, ip, program);
                    break;
                case 6:
                    jumpIfFalse(paramModesString, ip, program);
                    break;
                case 7:
                    lessThan(paramModesString, ip, program);
                    break;
                case 8:
                    intcodeEquals(paramModesString, ip, program);
                    break;
                default:
                    throw new InputMismatchException("Unsupported opCode " + opCode);
            }
        }
    }

    private static List<ParamMode> getParamModes(String paramModesString, int nParams) {
        List<ParamMode> paramModes = new ArrayList<>();
        for (int i = paramModesString.length() - 1; i >= 0; i--) {
            char paramMode = paramModesString.charAt(i);
            switch (paramMode) {
                case '0':
                    paramModes.add(ParamMode.POSITION);
                    break;
                case '1':
                    paramModes.add(ParamMode.IMMEDIATE);
                    break;
                default:
                    throw new InputMismatchException("Unsupported paramMode " + paramMode);
            }
        }
        while (paramModes.size() < nParams) {
            paramModes.add(ParamMode.POSITION);
        }
        return paramModes;
    }

    private static int getValue(int op, ParamMode paramMode, int[] program) {
        switch (paramMode) {
            case IMMEDIATE:
                return op;
            case POSITION:
                return program[op];
            default:
                throw new InputMismatchException("Unsupported paramMode " + paramMode);
        }
    }

    private static void add(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        int op1 = program[ip.getPosition() + 1];
        int op2 = program[ip.getPosition() + 2];
        int dest = program[ip.getPosition() + 3];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        program[dest] = val1 + val2;
        ip.modify(4);
    }

    private static void mul(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        int op1 = program[ip.getPosition() + 1];
        int op2 = program[ip.getPosition() + 2];
        int dest = program[ip.getPosition() + 3];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        program[dest] = val1 * val2;
        ip.modify(4);
    }

    private void input(InstructionPointer ip, int[] program) {
        int val = controller.getInput();
        int dest = program[ip.getPosition() + 1];
        program[dest] = val;
        ip.modify(2);
    }

    private void output(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 1);
        int op = program[ip.getPosition() + 1];
        int val = getValue(op, paramModes.get(0), program);
        controller.output(val);
        ip.modify(2);
    }

    private static void jumpIfTrue(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip.getPosition() + 1];
        int val1 = getValue(op1, paramModes.get(0), program);
        if (val1 != 0) {
            int op2 = program[ip.getPosition() + 2];
            int val2 = getValue(op2, paramModes.get(1), program);
            ip.set(val2);
        } else {
            ip.modify(3);
        }
    }

    private static void jumpIfFalse(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip.getPosition() + 1];
        int val1 = getValue(op1, paramModes.get(0), program);
        if (val1 == 0) {
            int op2 = program[ip.getPosition() + 2];
            int val2 = getValue(op2, paramModes.get(1), program);
            ip.set(val2);
        } else {
            ip.modify(3);
        }
    }

    private static void lessThan(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip.getPosition() + 1];
        int op2 = program[ip.getPosition() + 2];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        int dest = program[ip.getPosition() + 3];
        if (val1 < val2) {
            program[dest] = 1;
        } else {
            program[dest] = 0;
        }
        ip.modify(4);
    }

    private static void intcodeEquals(String paramModesString, InstructionPointer ip, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip.getPosition() + 1];
        int op2 = program[ip.getPosition() + 2];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        int dest = program[ip.getPosition() + 3];
        if (val1 == val2) {
            program[dest] = 1;
        } else {
            program[dest] = 0;
        }
        ip.modify(4);
    }
}
