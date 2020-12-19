package days.day18;

import java.util.InputMismatchException;
import java.util.Stack;
import java.util.function.Function;

public class Expression {
    private final String expressionString;
    private final String rpn1;
    private final String rpn2;

    public Expression(String expressionString) {
        this.expressionString = expressionString;
        rpn1 = toRpn(Expression::getPrecedence1);
        rpn2 = toRpn(Expression::getPrecedence2);
    }

    private String toRpn(Function<Character, Integer> getPrecedence) {
        StringBuilder output = new StringBuilder();
        Stack<Character> operatorStack = new Stack<>();
        int index = 0;
        while (index < expressionString.length()) {
            char c = expressionString.charAt(index);
            if (Character.isDigit(c)) {
                output.append(c);
            } else if (c == '(') {
                operatorStack.push(c);
            } else if (c == ')') {
                while (operatorStack.peek() != '(') {
                    output.append(operatorStack.pop());
                }
                operatorStack.pop();
            } else if (c == ' ') {

            } else {//operator
                while (!operatorStack.empty() &&
                        operatorStack.peek() != '(' &&
                        getPrecedence.apply(operatorStack.peek()) >= getPrecedence.apply(c)) {
                    output.append(operatorStack.pop());
                }
                operatorStack.push(c);
            }
            index++;
        }
        while (!operatorStack.empty()) {
            output.append(operatorStack.pop());
        }
        return output.toString();
    }

    private static int getPrecedence1(char c) {
        if (c == '*' || c == '+') {
            return 0;
        }
        throw new InputMismatchException("Invalid operator " + c);
    }

    private static int getPrecedence2(char c) {
        if (c == '*') {
            return 0;
        }
        if (c == '+') {
            return 1;
        }
        throw new InputMismatchException("Invalid operator " + c);
    }

    public long evaluate1() {
        return evaluate(rpn1);
    }

    public long evaluate2() {
        return evaluate(rpn2);
    }

    private long evaluate(String rpn) {
        Stack<Long> operands = new Stack<>();
        for (char c : rpn.toCharArray()) {
            if (Character.isDigit(c)) {
                operands.push((long) (c - '0'));
            } else {
                long a = operands.pop();
                long b = operands.pop();
                if (c == '+') {
                    operands.push(a + b);
                } else {
                    operands.push(a * b);
                }
            }
        }
        return operands.pop();
    }

    @Override
    public String toString() {
        return "Infix: " + expressionString + "\nRPN: " + rpn1;
    }
}
