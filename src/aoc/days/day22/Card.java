package aoc.days.day22;

import java.util.Objects;

public class Card {
    private int value;
    private Card next;
    private Card previous;

    public Card(int value) {
        this.value = value;
    }

    public Card(Card other) {
        value = other.value;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public Card getNext() {
        return next;
    }

    public void setNext(Card next) {
        this.next = next;
    }

    public Card getPrevious() {
        return previous;
    }

    public void setPrevious(Card previous) {
        this.previous = previous;
    }

    public boolean hsNext() {
        return next != null;
    }

    public boolean hasPrevious() {
        return previous != null;
    }

    @Override
    public String toString() {
        return "" + value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Card card = (Card) o;
        return value == card.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
