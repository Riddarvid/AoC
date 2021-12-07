package aoc21.utils.intcode;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;

public class IntcodeComputer {
    private Controller controller;
    private int ip;
    private int relativeBase;
    private long[] memory;
    private boolean shouldDelay;
    private int delay;

    public IntcodeComputer(Controller controller, long[] memory) {
        this.controller = controller;
        this.memory = Arrays.copyOf(memory, 10000);
    }

    public void activateDelay(int delay) {
        shouldDelay = true;
        this.delay = delay;
    }

    public void execute() {
        ip = 0;
        relativeBase = 0;
        while (true) {
            String instruction = Long.toString(memory[ip]);
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
                    add(paramModesString);
                    break;
                case 2:
                    mul(paramModesString);
                    break;
                case 3:
                    input(paramModesString);
                    break;
                case 4:
                    output(paramModesString);
                    break;
                case 5:
                    jumpIfTrue(paramModesString);
                    break;
                case 6:
                    jumpIfFalse(paramModesString);
                    break;
                case 7:
                    lessThan(paramModesString);
                    break;
                case 8:
                    intcodeEquals(paramModesString);
                    break;
                case 9:
                    modifyRelativeBase(paramModesString);
                    break;
                default:
                    throw new InputMismatchException("Unsupported opCode " + opCode);
            }
        }
        //System.out.println("Program halted");
    }

    public long getMemory(int index) {
        return memory[index];
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

    private long getValue(long op, ParamMode paramMode, boolean writing) {
        if (writing) {
            switch (paramMode) {
                case IMMEDIATE:
                    return op;
                case POSITION:
                    return op;
                case RELATIVE:
                    return op + relativeBase;
                default:
                    throw new InputMismatchException("Unsupported paramMode " + paramMode);
            }
        } else {
            switch (paramMode) {
                case IMMEDIATE:
                    return op;
                case POSITION:
                    return memory[(int) op];
                case RELATIVE:
                    return memory[(int) (op + relativeBase)];
                default:
                    throw new InputMismatchException("Unsupported paramMode " + paramMode);
            }
        }
    }

    private void add(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        long op1 = memory[ip + 1];
        long op2 = memory[ip + 2];
        int dest = (int)getValue(memory[ip + 3], paramModes.get(2), true);
        long val1 = getValue(op1, paramModes.get(0), false);
        long val2 = getValue(op2, paramModes.get(1), false);
        memory[dest] = val1 + val2;
        ip += 4;
    }

    private void mul(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        long op1 = memory[ip + 1];
        long op2 = memory[ip + 2];
        int dest = (int)getValue(memory[ip + 3], paramModes.get(2), true);
        long val1 = getValue(op1, paramModes.get(0), false);
        long val2 = getValue(op2, paramModes.get(1), false);
        memory[dest] = val1 * val2;
        ip += 4;
    }

    private void input(String paramModesString) {
        if (shouldDelay) {
            try {
                Thread.sleep(delay);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        List<ParamMode> paramModes = getParamModes(paramModesString, 1);
        long val = controller.getInput();
        int dest = (int)getValue(memory[ip + 1], paramModes.get(0), true);
        memory[dest] = val;
        ip += 2;
    }

    private void output(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 1);
        long op = memory[ip + 1];
        long val = getValue(op, paramModes.get(0), false);
        controller.output(val);
        ip += 2;
    }

    private void jumpIfTrue(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        long op1 = memory[ip + 1];
        long val1 = getValue(op1, paramModes.get(0), false);
        if (val1 != 0) {
            long op2 = memory[ip + 2];
            ip = (int)getValue(op2, paramModes.get(1), false);
        } else {
            ip += 3;
        }
    }

    private void jumpIfFalse(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 2);
        long op1 = memory[ip + 1];
        long val1 = getValue(op1, paramModes.get(0), false);
        if (val1 == 0) {
            long op2 = memory[ip + 2];
            ip = (int)getValue(op2, paramModes.get(1), false);
        } else {
            ip += 3;
        }
    }

    private void lessThan(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        long op1 = memory[ip + 1];
        long op2 = memory[ip + 2];
        long val1 = getValue(op1, paramModes.get(0), false);
        long val2 = getValue(op2, paramModes.get(1), false);
        int dest = (int)getValue(memory[ip + 3], paramModes.get(2), true);
        if (val1 < val2) {
            memory[dest] = 1;
        } else {
            memory[dest] = 0;
        }
        ip += 4;
    }

    private void intcodeEquals(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 3);
        long op1 = memory[ip + 1];
        long op2 = memory[ip + 2];
        long val1 = getValue(op1, paramModes.get(0), false);
        long val2 = getValue(op2, paramModes.get(1), false);
        int dest = (int)getValue(memory[ip + 3], paramModes.get(2), true);
        if (val1 == val2) {
            memory[dest] = 1;
        } else {
            memory[dest] = 0;
        }
        ip += 4;
    }

    private void modifyRelativeBase(String paramModesString) {
        List<ParamMode> paramModes = getParamModes(paramModesString, 1);
        long op = memory[ip + 1];
        int val = (int)getValue(op, paramModes.get(0), false);
        relativeBase += val;
        ip += 2;
    }
}
