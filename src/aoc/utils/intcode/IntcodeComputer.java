package aoc.utils.intcode;

import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.List;

public class IntcodeComputer {
    private Controller controller;
    private int ip;
    private int relativeBase;

    public IntcodeComputer(Controller controller) {
        this.controller = controller;
    }

    public void execute(int[] program) {
        ip = 0;
        relativeBase = 0;
        while (true) {
            String instruction = Integer.toString(program[ip]);
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
                    add(paramModesString, program);
                    break;
                case 2:
                    mul(paramModesString, program);
                    break;
                case 3:
                    input(program);
                    break;
                case 4:
                    output(paramModesString, program);
                    break;
                case 5:
                    jumpIfTrue(paramModesString, program);
                    break;
                case 6:
                    jumpIfFalse(paramModesString, program);
                    break;
                case 7:
                    lessThan(paramModesString, program);
                    break;
                case 8:
                    intcodeEquals(paramModesString, program);
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
                case '2':
                    paramModes.add(ParamMode.RELATIVE);
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

    private int getValue(int op, ParamMode paramMode, int[] program) {
        switch (paramMode) {
            case IMMEDIATE:
                return op;
            case POSITION:
                return program[op];
            case RELATIVE:
                return 0; //TODO
            default:
                throw new InputMismatchException("Unsupported paramMode " + paramMode);
        }
    }

    private void add(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        int op1 = program[ip + 1];
        int op2 = program[ip + 2];
        int dest = program[ip + 3];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        program[dest] = val1 + val2;
        ip += 4;
    }

    private void mul(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        int op1 = program[ip + 1];
        int op2 = program[ip + 2];
        int dest = program[ip + 3];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        program[dest] = val1 * val2;
        ip += 4;
    }

    private void input(int[] program) {
        int val = controller.getInput();
        int dest = program[ip + 1];
        program[dest] = val;
        ip += 2;
    }

    private void output(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 1);
        int op = program[ip + 1];
        int val = getValue(op, paramModes.get(0), program);
        controller.output(val);
        ip += 2;
    }

    private void jumpIfTrue(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip + 1];
        int val1 = getValue(op1, paramModes.get(0), program);
        if (val1 != 0) {
            int op2 = program[ip + 2];
            ip = getValue(op2, paramModes.get(1), program);
        } else {
            ip += 3;
        }
    }

    private void jumpIfFalse(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip + 1];
        int val1 = getValue(op1, paramModes.get(0), program);
        if (val1 == 0) {
            int op2 = program[ip + 2];
            ip = getValue(op2, paramModes.get(1), program);
        } else {
            ip += 3;
        }
    }

    private void lessThan(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip + 1];
        int op2 = program[ip + 2];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        int dest = program[ip + 3];
        if (val1 < val2) {
            program[dest] = 1;
        } else {
            program[dest] = 0;
        }
        ip += 4;
    }

    private void intcodeEquals(String paramModesString, int[] program) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        int op1 = program[ip + 1];
        int op2 = program[ip + 2];
        int val1 = getValue(op1, paramModes.get(0), program);
        int val2 = getValue(op2, paramModes.get(1), program);
        int dest = program[ip + 3];
        if (val1 == val2) {
            program[dest] = 1;
        } else {
            program[dest] = 0;
        }
        ip += 4;
    }
}
