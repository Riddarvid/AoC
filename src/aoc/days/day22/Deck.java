package aoc.days.day22;

public class Deck {
    private boolean isReversed;
    private Card top;
    private Card bottom;
    private final long size;

    public Deck(long size) {
        this.size = size;
        isReversed = false;
        Card current = new Card(0);
        top = current;
        for (int i = 1; i < size; i++) {
            Card next = new Card(i);
            current.setNext(next);
            next.setPrevious(current);
            current = next;
        }
        bottom = current;
    }

    public void dealIntoNewStack() {
        isReversed = !isReversed;
    }

    public void cut(long n) {
        if (isReversed) {
            if (n < 0) {
                cutFromTop(-n);
            } else {
                cutFromBottom(n);
            }
        } else {
            if (n < 0) {
                cutFromBottom(-n);
            } else {
                cutFromTop(n);
            }
        }
    }

    public void cutFromBottom(long n) {
        Card oldBottom = bottom;
        Card newBottom = bottom;
        for (int i = 0; i < n; i++) {
            newBottom = newBottom.getPrevious();
        }
        bottom = newBottom;
        Card newTop = newBottom.getNext();
        newTop.setPrevious(null);
        newBottom.setNext(null);
        top.setPrevious(oldBottom);
        oldBottom.setNext(top);
        top = newTop;
    }

    public void cutFromTop(long n) {
        Card oldTop = top;
        Card newTop = top;
        for (int i = 0; i < n; i++) {
            newTop = newTop.getNext();
        }
        top = newTop;
        Card newBottom = newTop.getPrevious();
        newBottom.setNext(null);
        newTop.setPrevious(null);
        bottom.setNext(oldTop);
        oldTop.setPrevious(bottom);
        bottom = newBottom;
    }

    public void dealWithIncrement(long n) {
        Card[] shuffled = new Card[(int)size];
        if (isReversed) {
            long cardIndex = 0;
            Card current = bottom;
            for (int i = 0; i < size; i++) {
                shuffled[(int)cardIndex] = current;
                current = current.getPrevious();
                cardIndex = (cardIndex + n) % size;
            }
        } else {
            long cardIndex = 0;
            Card current = top;
            for (int i = 0; i < size; i++) {
                shuffled[(int)cardIndex] = current;
                current = current.getNext();
                cardIndex = (cardIndex + n) % (int)size;
            }
        }
        rearrange(shuffled);
    }

    private Card step(Card card, long n) {
        if (n >= 0) {
            for (int i = 0; i < n; i++) {
                card = card.getNext();
                if (card == null) {
                    card = top;
                }
            }
        } else {
            for (int i = 0; i < (-n); i++) {
                card = card.getPrevious();
                if (card == null) {
                    card = bottom;
                }
            }
        }
        return card;
    }

    private void rearrange(Card[] shuffled) {
        isReversed = false;
        top = shuffled[0];
        top.setPrevious(null);
        bottom = shuffled[(int)size - 1];
        bottom.setNext(null);
        Card current = top;
        for (int i = 0; i < size - 1; i++) {
            Card next = shuffled[i + 1];
            current.setNext(next);
            next.setPrevious(current);
            current = next;
        }
    }

    public long indexOf(long n) {
        Card current = top;
        int index = 0;
        while (current.getValue() != n) {
            current = current.getNext();
            index++;
        }
        return index;
    }

    @Override
    public String toString() {
        if (isReversed) {
            StringBuilder sb = new StringBuilder();
            Card current = bottom;
            while (current != null) {
                sb.append(current.toString()).append(", ");
                current = current.getPrevious();
            }
            sb.delete(sb.length() - 2, sb.length());
            return sb.toString();
        } else {
            StringBuilder sb = new StringBuilder();
            Card current = top;
            while (current != null) {
                sb.append(current.toString()).append(", ");
                current = current.getNext();
            }
            sb.delete(sb.length() - 2, sb.length());
            return sb.toString();
        }
    }
}
